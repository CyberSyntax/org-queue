;;; org-queue-tasks.el --- Task management and navigation for org-queue -*- lexical-binding: t -*-

;;; Code:

(require 'cl-lib)
(require 'org-id)
(require 'org-queue-config)
(require 'org-queue-utils)
(require 'org-queue-srs-bridge)
(require 'seq)
(require 'subr-x)

(declare-function my-display-task-at-marker "org-queue-display" (task-or-marker))
(declare-function my-pulse-highlight-current-line "org-queue-display" (&optional time))
(declare-function org-queue-id-guard--refresh-queue-markers-for-file "org-queue-id-guard" (file &optional silent))

(defcustom my-queue-visible-buffer-count 8
  "How many distinct queue buffers to keep readily visible (not buried).
The set is centered on the current queue index, scanning forward with wrap."
  :type 'integer
  :group 'org-queue)

(defvar my-queue--last-visible-buffers nil
  "Last set of queue buffers kept visible by `my-queue-limit-visible-buffers'.")

(defvar my-queue--last-head-key nil
  "Cache of the queue head key to debounce visibility limiting.")

(defun my-queue--buffer-at-index (idx)
  "Return the buffer of the queue entry at IDX or nil."
  (when (and my-outstanding-tasks-list
             (>= idx 0)
             (< idx (length my-outstanding-tasks-list)))
    (my-safe-marker-buffer (nth idx my-outstanding-tasks-list))))

(defun my-queue-limit-visible-buffers (&optional n)
  "Keep only the next N distinct queue buffers readily visible (incremental).
Optimized: O(K) burying (K = newly disallowed buffers), debounced by queue head.
Does not reorder buffers; only buries buffers that just left the visible window."
  (let* ((count (or n my-queue-visible-buffer-count))
         (len (length my-outstanding-tasks-list)))
    (when (and (> len 0) (> count 0))
      (let* ((head (and my-outstanding-tasks-list
                        (nth my-outstanding-tasks-index my-outstanding-tasks-list)))
             (head-key (and head (my--task-unique-key head))))
        ;; Debounce: if the head hasn't changed, the allowed window is identical—skip.
        (when (not (and head-key (equal head-key my-queue--last-head-key)))
          (setq my-queue--last-head-key head-key)
          ;; Build the new allowed set (up to COUNT distinct buffers ahead, with wrap).
          (let ((allowed-list '())
                (allowed-h (make-hash-table :test 'eq))
                (collected 0)
                (i 0))
            (while (and (< collected count) (< i len))
              (let* ((idx (mod (+ my-outstanding-tasks-index i) len))
                     (buf (my-queue--buffer-at-index idx)))
                (when (and buf (buffer-live-p buf) (not (gethash buf allowed-h)))
                  (puthash buf t allowed-h)
                  (push buf allowed-list)
                  (setq collected (1+ collected))))
              (setq i (1+ i)))
            (setq allowed-list (nreverse allowed-list))
            ;; Incremental bury: only bury buffers that were visible previously but are no longer allowed.
            (dolist (buf my-queue--last-visible-buffers)
              (unless (gethash buf allowed-h)
                (when (buffer-live-p buf) (bury-buffer buf))))
            ;; Update the snapshot of visible buffers.
            (setq my-queue--last-visible-buffers allowed-list)))))))

;; --- Lazy prune helpers: remove not-due items on departure/arrival ---

(defun my-queue--task-due-p (task)
  "Return non-nil if TASK is due now using :available-at when present.
Rules:
- SRS: suppressed entirely during night shift.
- Due if available-at <= now. If :available-at is missing, compute it.
- Fallback to legacy checks only when availability cannot be computed."
  (let ((m (my-extract-marker task)))
    (when (and (markerp m) (marker-buffer m) (buffer-live-p (marker-buffer m)))
      (let* ((now (org-queue--now))
             (is-srs (plist-get task :srs)))
        (if (and is-srs (org-queue-night-shift-p))
            nil
          (let ((avail (or (plist-get task :available-at)
                           (with-current-buffer (marker-buffer m)
                             (org-with-point-at m
                               (org-queue--task-available-at task))))))
            (if avail
                (progn
                  (plist-put task :available-at avail)
                  (not (time-less-p now avail)))
              ;; Fallback (legacy behavior)
              (with-current-buffer (marker-buffer m)
                (org-with-point-at m
                  (if is-srs
                      (let ((due (org-queue-srs-next-due-time (point))))
                        (and due (not (org-queue-night-shift-p))
                             (not (time-less-p (org-queue--now) due))))
                    (my-is-outstanding-task)))))))))))

(defun my-queue--remove-index (idx &optional quiet no-save)
  "Remove entry at IDX from the queue; do not display anything.
If NO-SAVE is non-nil, do not write caches here (caller will save once)."
  (when (and my-outstanding-tasks-list
             (numberp idx)
             (>= idx 0)
             (< idx (length my-outstanding-tasks-list)))
    (let* ((removed (nth idx my-outstanding-tasks-list))
           (buf (my-safe-marker-buffer removed)))
      (setq my-outstanding-tasks-list
            (append (seq-take my-outstanding-tasks-list idx)
                    (seq-drop  my-outstanding-tasks-list (1+ idx))))
      (when (>= my-outstanding-tasks-index (length my-outstanding-tasks-list))
        (setq my-outstanding-tasks-index (max 0 (1- (length my-outstanding-tasks-list)))))
      (when (buffer-live-p buf) (bury-buffer buf))
      (my-queue-limit-visible-buffers)
      (unless quiet
        (message "Removed 1 item from queue; now %d total."
                (length my-outstanding-tasks-list)))
      t)))

(defun my-queue-filter-not-due-in-place (&optional quiet)
  "Remove all not-due items from the current queue list in one pass.
Returns the number of removed items. Only touches items already in the queue."
  (when my-outstanding-tasks-list
    (let* ((old my-outstanding-tasks-list)
           (new (let (acc)
                  (dolist (task old (nreverse acc))
                    (when (my-queue--task-due-p task)
                      (push task acc)))))
           (removed (- (length old) (length new))))
      (setq my-outstanding-tasks-list new)
      ;; Clamp index if needed
      (when (>= my-outstanding-tasks-index (length new))
        (setq my-outstanding-tasks-index (max 0 (1- (length new)))))
      ;; Tidy, save, refresh chooser
      (my-queue-limit-visible-buffers)
      (unless quiet
        (message "Pruned %d not-due item(s) from queue" removed))
      removed)))

(defun org-queue--non-srs-sort-key (task)
  "Return non-SRS ordering key for TASK: (todo-first, numeric-priority, last-repeat-float).
Lower is better for each component.
- todo-first: 0 for TODO, 1 otherwise
- numeric-priority: ascending
- last-repeat-float: seconds since epoch for :LAST_REPEAT: (missing treated as 0 => oldest)"
  (let* ((is-todo (or (plist-get task :is-todo)
                      (let ((m (my-extract-marker task)))
                        (and (markerp m)
                             (org-with-point-at m (my-is-todo-task))))))
         (prio (or (plist-get task :priority)
                   (let ((m (my-extract-marker task)))
                     (and (markerp m)
                          (org-with-point-at m (my-get-raw-priority-value))))))
         (prio (or prio org-priority-default))
         (lastf
          (let ((m (my-extract-marker task)))
            (if (and (markerp m) (marker-buffer m) (buffer-live-p (marker-buffer m)))
                (with-current-buffer (marker-buffer m)
                  (org-with-point-at m
                    (let ((lt (org-queue--parse-last-repeat)))
                      (if lt (float-time lt) 0.0))))
              0.0))))
    (list (if is-todo 0 1) prio lastf)))

(defun org-queue--non-srs-key<= (a b)
  "Return non-nil if TASK A should not come after TASK B under non-SRS ordering.
Compares (todo-first, priority, last-repeat-float) lexicographically."
  (let* ((ka (org-queue--non-srs-sort-key a))
         (kb (org-queue--non-srs-sort-key b)))
    (or (< (nth 0 ka) (nth 0 kb))
        (and (= (nth 0 ka) (nth 0 kb))
             (or (< (nth 1 ka) (nth 1 kb))
                 (and (= (nth 1 ka) (nth 1 kb))
                      (<= (nth 2 ka) (nth 2 kb))))))))

;; --- Micro update helpers (single-item only) -----------------------------
(defun org-queue--insert-non-srs-outstanding-sorted (task)
  "Insert non-SRS TASK into `my-outstanding-tasks-list' keeping non-SRS local order only.
Does not touch SRS items order; O(n) stable insertion."
  (let* ((n (length my-outstanding-tasks-list))
         (i 0)
         (inserted nil))
    (while (and (< i n) (not inserted))
      (let ((other (nth i my-outstanding-tasks-list)))
        (if (plist-get other :srs)
            (setq i (1+ i))
          (if (org-queue--non-srs-key<= task other)
              (progn
                (setq my-outstanding-tasks-list
                      (append (seq-take my-outstanding-tasks-list i)
                              (list task)
                              (seq-drop my-outstanding-tasks-list i)))
                (setq inserted t))
            (setq i (1+ i))))))
    (unless inserted
      ;; Append right after the last non-SRS if any, else at front.
      (let ((last-non-srs
             (let ((j 0) (last -1))
               (dolist (e my-outstanding-tasks-list last)
                 (unless (plist-get e :srs) (setq last j))
                 (setq j (1+ j))))))
        (if (>= last-non-srs 0)
            (setq my-outstanding-tasks-list
                  (append (seq-take my-outstanding-tasks-list (1+ last-non-srs))
                          (list task)
                          (seq-drop my-outstanding-tasks-list (1+ last-non-srs))))
          (setq my-outstanding-tasks-list (cons task my-outstanding-tasks-list)))))))

(defun org-queue--promote-pending-due-now! ()
  "Move any tasks from `my-today-pending-tasks` whose :available-at ≤ now
into `my-outstanding-tasks-list`, preserving pool-local order.

- Non‑SRS: keep the non‑SRS local ordering via
  `org-queue--insert-non-srs-outstanding-sorted`.
- SRS: append near the end of the existing SRS block (same approach used
  by the micro-update code).
- Night shift: never promote SRS during night shift.
Returns the number of tasks promoted."
  (let* ((now (org-queue--now))
         (night (org-queue-night-shift-p))
         (keep '())
         (promoted 0))
    (dolist (task my-today-pending-tasks)
      (let ((avail (plist-get task :available-at))
            (is-srs (plist-get task :srs)))
        (if (and avail (not (time-less-p now avail))
                 (or (not is-srs) (not night)))
            ;; Promote this task
            (progn
              (if is-srs
                  (let ((last-srs
                         (let ((j 0) (last -1))
                           (dolist (e my-outstanding-tasks-list last)
                             (when (plist-get e :srs) (setq last j))
                             (setq j (1+ j))))))
                    (if (>= last-srs 0)
                        (setq my-outstanding-tasks-list
                              (append (seq-take my-outstanding-tasks-list (1+ last-srs))
                                      (list task)
                                      (seq-drop my-outstanding-tasks-list (1+ last-srs))))
                      (setq my-outstanding-tasks-list
                            (append my-outstanding-tasks-list (list task)))))
                (org-queue--insert-non-srs-outstanding-sorted task))
              (setq promoted (1+ promoted)))
          ;; Keep pending
          (push task keep))))
    (setq my-today-pending-tasks (nreverse keep))
    promoted))

(defun org-queue--micro-update-current! (&optional reason)
  "Re-evaluate ONLY the current heading and update outstanding/pending minimally.
REASON is informational ('priority 'schedule 'advance 'postpone 'review 'stamp 'create)."
  (unless (derived-mode-p 'org-mode)
    (user-error "Not in org-mode buffer"))
  (save-excursion
    (org-back-to-heading t)
    (let* ((marker   (point-marker))
           (id       (or (org-entry-get nil "ID") (org-id-get-create)))
           (priority (my-get-raw-priority-value))
           (file     (buffer-file-name))
           (heading  (org-get-heading t t t t))
           (pos      (point))
           (srs      (eq (org-srs-entry-p (point)) 'current))
           (task     (list :id id :marker marker :priority priority
                           :file file :is-todo (my-is-todo-task)
                           :heading heading :pos pos :srs srs)))
      ;; refresh availability (sets :available-at and :due-date convenience)
      (setq task (org-queue--update-available-at! task))
      (let* ((key   (my--task-unique-key task))
             (now   (org-queue--now))
             (night (org-queue-night-shift-p))
             (avail (plist-get task :available-at))
             (due-now (and avail (not (time-less-p now avail))))
             (in-scope (org-queue--in-scope-p task)))
        ;; remove any existing instance (from both lists)
        (setq my-outstanding-tasks-list
              (cl-remove-if (lambda (x) (equal (my--task-unique-key x) key))
                            my-outstanding-tasks-list))
        (setq my-today-pending-tasks
              (cl-remove-if (lambda (x) (equal (my--task-unique-key x) key))
                            my-today-pending-tasks))
        ;; decide where to place it, respecting scope (past included)
        (cond
         (srs
          (when in-scope
            (cond
             (night
              ;; during night shift all SRS are postponed to pending
              (push task my-today-pending-tasks))
             (due-now
              ;; make it outstanding now; keep SRS block locality
              (let ((last-srs
                     (let ((j 0) (last -1))
                       (dolist (e my-outstanding-tasks-list last)
                         (when (plist-get e :srs) (setq last j))
                         (setq j (1+ j))))))
                (if (>= last-srs 0)
                    (setq my-outstanding-tasks-list
                          (append (seq-take my-outstanding-tasks-list (1+ last-srs))
                                  (list task)
                                  (seq-drop my-outstanding-tasks-list (1+ last-srs))))
                  (setq my-outstanding-tasks-list
                        (append my-outstanding-tasks-list (list task))))))
             ((and avail (org-queue--same-day-p avail now))
              ;; due later *today*
              (push task my-today-pending-tasks)))))
         (t ;; non-SRS
          (when in-scope
            (if due-now
                (org-queue--insert-non-srs-outstanding-sorted task)
              (push task my-today-pending-tasks)))))
        ;; finalize
    (setq my-outstanding-tasks-index 0)
    (setq my-queue--last-visible-buffers nil
          my-queue--last-head-key nil)
    (setq org-queue--flag-counts-cache nil)
    (my-queue-limit-visible-buffers)
    ;; Save only the current buffer (respects suppression flags).
    (org-queue--autosave-current)))))  ;; end micro-update

;; Variables for task list management
(defvar my-outstanding-tasks-list nil
  "List of outstanding tasks, sorted by priority.")

(defvar my-outstanding-tasks-index 0
  "Current index in the outstanding tasks list.")

(defvar my-today-pending-tasks nil
  "List of today's pending tasks (not yet available).
Each element is a task plist (same shape as outstanding) with :available-at set.")

;; === Daily maintenance gating (stamp + on-startup scheduling) ===
(defcustom org-queue-maintenance-on-startup 'if-needed
  "When to run `org-queue-maintenance` automatically at startup.
- nil       — never run automatically.
- if-needed — run at startup only if it hasn't run today (default).
- t         — always run at startup."
  :type '(choice (const :tag "Never" nil)
                 (const :tag "If not run today" if-needed)
                 (const :tag "Always" t))
  :group 'org-queue)

(defcustom org-queue-maintenance-idle-delay 7.0
  "Seconds of idle time after startup before automatic maintenance may run."
  :type 'number
  :group 'org-queue)

(defcustom org-queue-maintenance-stamp-file
  (expand-file-name "maintenance.stamp" org-queue-cache-dir)
  "File that records the last successful maintenance run."
  :type 'file
  :group 'org-queue)

(defun org-queue--maintenance-last-run-date ()
  "Return YYYY-MM-DD string of the last successful maintenance run, or nil."
  (when (file-exists-p org-queue-maintenance-stamp-file)
    (condition-case _err
        (let* ((data (with-temp-buffer
                       (insert-file-contents org-queue-maintenance-stamp-file)
                       (read (buffer-string)))))
          (plist-get data :date))
      (error nil))))

(defun org-queue--write-maintenance-stamp (&optional tm)
  "Record TM (or now) as the last successful maintenance run."
  (let* ((now  (or tm (current-time)))
         (date (format-time-string "%Y-%m-%d" now))
         (dir  (file-name-directory org-queue-maintenance-stamp-file)))
    (when (and dir (not (file-directory-p dir)))
      (ignore-errors (make-directory dir t)))
    (with-temp-file org-queue-maintenance-stamp-file
      (insert (prin1-to-string (list :date date :timestamp now))))))

(defun org-queue-daily-maintenance-maybe ()
  "Run maintenance once per day (lazy) based on the stamp file and user setting.
Runs on idle using `org-queue-maintenance-idle-delay`."
  (pcase org-queue-maintenance-on-startup
    ('t
     (run-with-idle-timer org-queue-maintenance-idle-delay nil
                          #'org-queue-maintenance))
    ('if-needed
     (let ((today (format-time-string "%Y-%m-%d"))
           (last  (org-queue--maintenance-last-run-date)))
       (when (not (string= today last))
         (run-with-idle-timer org-queue-maintenance-idle-delay nil
                              #'org-queue-maintenance))))
    (_ nil)))

(defvar my-org-id-locations-initialized nil
  "Whether org-id locations DB has been fully initialized this session.")

(defun my-org-id-initialize-id-locations ()
  "No-op: global org-id DB is not used in this design."
  (unless my-org-id-locations-initialized
    (setq my-org-id-locations-initialized t)
    nil))

;; Task identification functions

;; --- Time and availability helpers for two-queue architecture ---

(defun org-queue--now ()
  "Return the current time as an Emacs time value."
  (current-time))

(defun org-queue--same-day-p (time-a time-b)
  "Return non-nil if TIME-A and TIME-B fall on the same local calendar date."
  (when (and time-a time-b)
    (let* ((da (decode-time time-a))
           (db (decode-time time-b)))
      (and (= (nth 5 da) (nth 5 db))  ;; year
           (= (nth 4 da) (nth 4 db))  ;; month
           (= (nth 3 da) (nth 3 db)))))) ;; day

(defun org-queue--date-of-time (tm)
  "Return YYYY-MM-DD string of TM in local time."
  (when tm (format-time-string "%Y-%m-%d" tm)))

(defun org-queue--parse-last-repeat ()
  "Parse :LAST_REPEAT: property at point into Emacs time, or nil if absent/invalid."
  (let ((s (org-entry-get nil "LAST_REPEAT")))
    (when (and (stringp s) (not (string-empty-p s)))
      (ignore-errors (org-time-string-to-time s)))))

(defun org-queue--deferral-minutes (priority)
  "Return snooze deferral minutes based on PRIORITY (1..64).
f(p) = org-queue-non-srs-snooze-base-minutes + org-queue-non-srs-snooze-slope-minutes * p."
  (let* ((p (or priority org-priority-default))
         (base (max 0 (or org-queue-non-srs-snooze-base-minutes 0)))
         (slope (max 0 (or org-queue-non-srs-snooze-slope-minutes 0))))
    (+ base (* slope p))))

(defun org-queue--scheduled-time-here ()
  "Return SCHEDULED Emacs time for the current heading, or nil."
  (org-get-scheduled-time nil))

(defun org-queue--task-available-at (task)
  "Compute available-at (Emacs time) for TASK without side effects.
Rules:
- SRS: next SRS due time from drawer (or nil).
- Non-SRS:
  available-at = max(schedule-time, last-repeat + deferral(priority)).
  If LAST_REPEAT is absent, treat as no snooze (i.e., available-at = schedule-time).
  If QFORCE is set:
    - When `org-queue-qforce-ignores-last-repeat` is non-nil: available-at = now.
    - Else: available-at = max(now, last-repeat + deferral); if LAST_REPEAT absent, treat as now."
  (let* ((m (my-extract-marker task)))
    (when (and (markerp m) (marker-buffer m) (buffer-live-p (marker-buffer m)))
      (with-current-buffer (marker-buffer m)
        (org-with-point-at m
          (if (plist-get task :srs)
              ;; SRS path
              (org-queue-srs-next-due-time (point))
            ;; Non-SRS path
            (let* ((now (org-queue--now))
                   (priority (or (plist-get task :priority)
                                 (let ((ps (org-entry-get nil "PRIORITY")))
                                   (if ps (string-to-number ps) org-priority-default))))
                   (qforce (org-entry-get nil org-queue-force-outstanding-property))
                   (scheduled (org-queue--scheduled-time-here))
                   (last (org-queue--parse-last-repeat))
                   (def-min (org-queue--deferral-minutes priority))
                   (def-secs (seconds-to-time (* 60 def-min))))
              (cond
               ;; QFORCE overrides scheduling date; treat as today.
               (qforce
                (if org-queue-qforce-ignores-last-repeat
                    now
                  (if last
                      (let ((cand (time-add last def-secs)))
                        (if (time-less-p now cand) cand now))
                    ;; No LAST_REPEAT => no active snooze; available immediately.
                    now)))
               ;; No schedule => not considered today; return nil.
               ((null scheduled) nil)
               ;; With schedule: apply LAST_REPEAT deferral if present.
               (last
                (let ((cand (time-add last def-secs)))
                  (if (time-less-p scheduled cand) cand scheduled)))
               ;; No LAST_REPEAT: available at schedule-time
               (t scheduled)))))))))

(defun org-queue--task-is-today-p (task)
  "Return non-nil if TASK belongs to today (local date).
- SRS: next due date is today.
- Non-SRS: SCHEDULED date is today. QFORCE counts as today."
  (let* ((m (my-extract-marker task)))
    (when (and (markerp m) (marker-buffer m) (buffer-live-p (marker-buffer m)))
      (with-current-buffer (marker-buffer m)
        (org-with-point-at m
          (if (plist-get task :srs)
              (let ((due (org-queue-srs-next-due-time (point))))
                (and due (org-queue--same-day-p due (org-queue--now))))
            (or (org-entry-get nil org-queue-force-outstanding-property)
                (let ((sched (org-queue--scheduled-time-here)))
                  (and sched (org-queue--same-day-p sched (org-queue--now)))))))))))

(defun org-queue--update-available-at! (task)
  "Recompute and store :available-at on TASK; also set :due-date if derivable.
Returns TASK."
  (let* ((m (my-extract-marker task)))
    (when (and (markerp m) (marker-buffer m) (buffer-live-p (marker-buffer m)))
      (with-current-buffer (marker-buffer m)
        (org-with-point-at m
          (let ((avail (org-queue--task-available-at task)))
            (plist-put task :available-at avail)
            ;; Optional convenience date for display/debug
            (cond
             ((plist-get task :srs)
              (when avail (plist-put task :due-date (org-queue--date-of-time avail))))
             (t
              (let ((sched (org-queue--scheduled-time-here)))
                (when sched (plist-put task :due-date (org-queue--date-of-time sched))))))
            task))))))

(defun my-is-overdue-task ()
  "Return non-nil if the current task is overdue."
  (let ((scheduled-time (org-get-scheduled-time nil)))
    (and scheduled-time
	   (< (time-to-days scheduled-time) (time-to-days (current-time))))))

(defcustom org-queue-force-outstanding-property "QFORCE"
  "Property name used to force an entry to be treated as outstanding
even if it is scheduled in the future."
  :type 'string
  :group 'org-queue)

(defun my-is-outstanding-task ()
  "Return non-nil if the current task is outstanding:
- scheduled for today or earlier, or
- explicitly forced via `org-queue-force-outstanding-property' (SuperMemo-style Add to outstanding).
DONE tasks are excluded elsewhere."
  (let* ((scheduled-time (org-get-scheduled-time nil))
         (force (org-entry-get nil org-queue-force-outstanding-property)))
    (or (and scheduled-time
             (<= (time-to-days scheduled-time) (time-to-days (current-time))))
        (and force (not (string-empty-p force))))))

;; Org agenda skip functions
(defun my-org-agenda-skip-non-outstanding-tasks ()
  "Skip tasks that are not outstanding."
  (unless (my-is-outstanding-task)
    (org-end-of-subtree t)))

(defun my-org-agenda-skip-past-and-today-tasks ()
  "Skip tasks that are scheduled for today or earlier; show only future tasks."
  (let ((scheduled-time (org-get-scheduled-time nil)))
    (if (or (not scheduled-time)
	      (<= (time-to-days scheduled-time) (time-to-days (current-time))))
	  (org-end-of-subtree t))))

(defun my-org-agenda-skip-scheduled-tasks ()
  "Skip tasks that have a SCHEDULED date."
  (let ((scheduled-time (org-get-scheduled-time nil)))
    (if scheduled-time
	  (org-end-of-subtree t))))

(defun my--candidates-for-id-in-file (uuid file)
  "Return all heading markers in FILE that have :ID: UUID."
  (when (and (stringp uuid) (> (length uuid) 0)
             (stringp file) (file-exists-p file))
    (with-current-buffer (find-file-noselect file)
      (save-restriction
        (widen)
        (save-excursion
          (goto-char (point-min))
          (let ((rx (concat "^[ \t]*:ID:[ \t]*" (regexp-quote uuid) "[ \t]*$"))
                (ms '()))
            (while (re-search-forward rx nil t)
              (let ((after (match-end 0)))
                (org-back-to-heading t)
                (push (point-marker) ms)
                (goto-char after)))
            (nreverse ms)))))))

(defun my--task-sync-metadata (task mk)
  "Update TASK plist with MK's current location: :marker :file :heading :pos.
Return MK. Widen temporarily and only update :heading when the ID matches."
  (when (and (listp task)
             (markerp mk)
             (marker-buffer mk)
             (buffer-live-p (marker-buffer mk)))
    (plist-put task :marker mk)
    (with-current-buffer (marker-buffer mk)
      (save-restriction
        (widen)
        (save-excursion
          (goto-char (marker-position mk))
          (let ((file (buffer-file-name))
                (pos nil)
                (heading nil))
            (when (derived-mode-p 'org-mode)
              (org-back-to-heading t)
              (setq pos (point))
              (setq heading (org-get-heading t t t t)))
            (plist-put task :file file)
            (when pos (plist-put task :pos pos))
            ;; Only update heading if it really belongs to this task's ID.
            (when (and (derived-mode-p 'org-mode)
                       (plist-get task :id)
                       (string= (or (org-entry-get nil "ID") "")
                                (plist-get task :id)))
              (plist-put task :heading heading)))))))
  mk)

(defun my-extract-marker (task-or-marker)
  "Robustly extract a live marker from TASK-OR-MARKER (file-local only).
Strategy (single path):
- If a live marker is present, use it and sync plist metadata.
- Else resolve by :id within :file, preferring matching :heading, else closest to :pos (and sync).
- Else if :file and :pos exist, jump to that position (and sync)."
  (let (result)
    (cond
     ;; Given a marker directly
     ((markerp task-or-marker)
      (when (and (marker-buffer task-or-marker)
                 (buffer-live-p (marker-buffer task-or-marker)))
        (setq result task-or-marker)))
     ;; Given a plist task
     ((listp task-or-marker)
      (let* ((m       (plist-get task-or-marker :marker))
             (id      (plist-get task-or-marker :id))
             (file    (plist-get task-or-marker :file))  ;; resolution restricted to this file
             (heading (plist-get task-or-marker :heading))
             (pos0    (plist-get task-or-marker :pos)))
        ;; Prefer existing live marker and sync
        (when (and (not result)
                   (markerp m)
                   (marker-buffer m)
                   (buffer-live-p (marker-buffer m)))
          (setq result (my--task-sync-metadata task-or-marker m)))
        ;; Resolve by ID inside the recorded file (handles duplicates deterministically)
        (when (and (not result) id file)
          (let* ((cands (my--candidates-for-id-in-file id file))
                 (chosen
                  (cond
                   ;; Prefer candidate whose heading matches stored heading
                   ((and heading cands)
                    (cl-find-if
                     (lambda (mk)
                       (with-current-buffer (marker-buffer mk)
                         (save-excursion
                           (goto-char (marker-position mk))
                           (string= (or (org-get-heading t t t t) "")
                                    (or heading "")))))
                     cands))
                   ;; Otherwise, choose the closest to the stored pos
                   ((and (numberp pos0) cands)
                    (car (sort (copy-sequence cands)
                               (lambda (a b)
                                 (< (abs (- (marker-position a) pos0))
                                    (abs (- (marker-position b) pos0)))))))
                   ;; Else first candidate in file
                   (t (car cands)))))
            (when (and chosen
                       (marker-buffer chosen)
                       (buffer-live-p (marker-buffer chosen)))
              (setq result (my--task-sync-metadata task-or-marker chosen)))))
        ;; Fallback to file+pos if available
        (when (and (not result) file (file-exists-p file) (numberp pos0))
          (with-current-buffer (find-file-noselect file)
            (save-restriction
              (widen)
              (save-excursion
                (goto-char (min (max pos0 (point-min)) (point-max)))
                (org-back-to-heading t)
                (setq result (my--task-sync-metadata task-or-marker (point-marker))))))))))
    result))

(defun my-safe-marker-buffer (task-or-marker)
  "Safely get the buffer of TASK-OR-MARKER.
TASK-OR-MARKER can be a marker or a plist with a :marker property."
  (let ((marker (my-extract-marker task-or-marker)))
    (when (and marker 
               (markerp marker)
               (marker-buffer marker)
               (buffer-live-p (marker-buffer marker)))
      (marker-buffer marker))))

(defun my-safe-marker-position (task-or-marker)
  "Safely get the position of TASK-OR-MARKER.
TASK-OR-MARKER can be a marker or a plist with a :marker property."
  (let ((marker (my-extract-marker task-or-marker)))
    (when (and marker 
               (markerp marker)
               (marker-buffer marker)
               (buffer-live-p (marker-buffer marker)))
      (marker-position marker))))

(defun my-marker-from-uuid (uuid &optional file)
  "Return a live marker for heading UUID, searching only in FILE when provided.
If FILE is nil, no global lookup is performed (single-path design)."
  (when (and (stringp uuid) (> (length uuid) 0))
    (cond
     ((and file (file-exists-p file))
      (with-current-buffer (find-file-noselect file)
        (save-restriction
          (widen)
          (save-excursion
            (goto-char (point-min))
            (if (re-search-forward
                 (concat "^[ \t]*:ID:[ \t]*" (regexp-quote uuid) "[ \t]*$") nil t)
                (progn (org-back-to-heading t) (point-marker))
              nil)))))
     (t nil))))

(defun my-position-from-uuid (uuid)
  "Return the buffer position (point) of the Org heading with UUID (its :ID:).
Returns nil if not found."
  (let ((m (my-marker-from-uuid uuid)))
    (when (markerp m)
      (marker-position m))))

;; Task list functions

(defun my-ensure-task-list-present ()
  "Ensure the task list exists in memory without reordering it.
- If the in-memory list exists, do nothing.
- Else build once (initialization fallback). Clamp index."
  (unless my-outstanding-tasks-list
    (my-get-outstanding-tasks)
    (setq my-outstanding-tasks-index 0))
  (when (or (not (numberp my-outstanding-tasks-index))
            (< my-outstanding-tasks-index 0)
            (>= my-outstanding-tasks-index (length my-outstanding-tasks-list)))
    (setq my-outstanding-tasks-index 0)))
  
(defun my-ensure-synchronized-task-list ()
  "Ensure we have a current, synchronized task list and valid index."
  (my-get-outstanding-tasks)
  (setq my-outstanding-tasks-index 0)
  (my-queue-limit-visible-buffers))

(defun my--task-unique-key (task)
  "Key for deduplication: prefer :id, else file@pos."
  (or (plist-get task :id)
      (let* ((m (my-extract-marker task))
             (buf (and (markerp m) (marker-buffer m)))
             (pos (and (markerp m) (marker-position m)))
             (file (and buf (buffer-file-name buf))))
        (when (and file pos)
          (format "%s@%d" (file-truename file) pos)))))

(defun my-dedupe-outstanding-tasks ()
  "Remove duplicate tasks (by ID, else file@pos), keeping first occurrence."
  (let ((seen (make-hash-table :test 'equal))
        (out  '()))
    (dolist (task my-outstanding-tasks-list)
      (let ((k (my--task-unique-key task)))
        (unless (and k (gethash k seen))
          (when k (puthash k t seen))
          (push task out))))
    (setq my-outstanding-tasks-list (nreverse out))))

(defun org-queue--interleave-by-ratio (a b a-count b-count)
  "Interleave lists A (non-SRS) and B (SRS) by groups of A-COUNT and B-COUNT."
  (let ((out '()))
    (while (or a b)
      (dotimes (_ (max 0 a-count)) (when a (push (pop a) out)))
      (dotimes (_ (max 0 b-count)) (when b (push (pop b) out))))
    (nreverse out)))

(defun org-queue--interleave-by-ratio-start (a b a-count b-count start)
  "Interleave A (non-SRS) and B (SRS) by counts, starting with START ('non-srs or 'srs)."
  (if (eq start 'srs)
      (org-queue--interleave-by-ratio b a b-count a-count)
    (org-queue--interleave-by-ratio a b a-count b-count)))

(defun org-queue--earliest-time (tasks)
  "Return earliest of :available-at or :srs-due in TASKS, or nil."
  (let* ((ts (delq nil (mapcar (lambda (task)
                                 (or (plist-get task :available-at)
                                     (plist-get task :srs-due)))
                               tasks))))
    (when ts (car (sort (copy-sequence ts) #'time-less-p)))))

(defun org-queue--decide-mix-start (a-count b-count non-srs srs)
  "Decide which pool starts at the head based on `org-queue-mix-start`."
  (pcase org-queue-mix-start
    ('non-srs 'non-srs)
    ('srs     'srs)
    ('rotate  (let* ((block (+ (max 0 a-count) (max 0 b-count)))
                     (phase (mod org-queue--mix-phase (max 1 block))))
                (if (< phase a-count) 'non-srs 'srs)))
    ('auto    (let ((ta (org-queue--earliest-time non-srs))
                    (tb (org-queue--earliest-time srs)))
                (cond
                 ((and ta tb) (if (time-less-p tb ta) 'srs 'non-srs))
                 (tb 'srs)
                 (t 'non-srs))))
    (_ 'non-srs)))

(defun org-queue--advance-mix-phase (&optional a-count b-count)
  "Advance mix phase by 1 modulo (a-count+b-count)."
  (let* ((ratio (or (and (boundp 'org-queue-srs-mix-ratio) org-queue-srs-mix-ratio)
                    '(1 . 4)))
         (a (or a-count (car ratio)))
         (b (or b-count (cdr ratio)))
         (block (max 1 (+ (max 0 a) (max 0 b)))))
    (setq org-queue--mix-phase (mod (1+ org-queue--mix-phase) block))))

(defun org-queue--in-scope-p (task)
  "Return non-nil when TASK belongs to today's working set (today or earlier).

Non-SRS: scoped if SCHEDULED ≤ today, or QFORCE is set.
SRS    : scoped if the next due timestamp (from :SRSITEMS:) has
         a local calendar date ≤ today (i.e., overdue or today)."
  (let ((m (my-extract-marker task)))
    (when (and (markerp m) (marker-buffer m) (buffer-live-p (marker-buffer m)))
      (with-current-buffer (marker-buffer m)
        (org-with-point-at m
          (if (plist-get task :srs)
              (let ((due (org-queue-srs-next-due-time (point))))
                (and due (<= (time-to-days due) (time-to-days (current-time)))))
            (let* ((qforce (org-entry-get nil org-queue-force-outstanding-property))
                   (sched  (org-get-scheduled-time nil)))
              (or qforce
                  (and sched (<= (time-to-days sched)
                                 (time-to-days (current-time))))))))))))

(defun my-get-outstanding-tasks ()
  "Build outstanding/pending queues in ONE pass over files."
  (let* ((now (org-queue--now))
         (night (org-queue-night-shift-p))
         (non-srs-out '()) (non-srs-pending '())
         (srs-out '())      (srs-pending '()))
    (org-queue-map-entries
     (lambda ()
       (unless (my-is-done-task)
         (let* ((marker  (point-marker))
                (id      (or (org-entry-get nil "ID") (org-id-get-create)))
                (priority (my-get-raw-priority-value))
                (file    (buffer-file-name))
                (heading (org-get-heading t t t t))
                (pos     (point))
                (where   (org-srs-entry-p (point))))
           (cond
            ;; --- SRS entries
            ((eq where 'current)
             (let ((due (org-queue-srs-next-due-time (point))))
               (when due
                 (let ((task (list :id id :marker marker :priority priority
                                   :file file :is-todo nil :heading heading :pos pos
                                   :srs t :srs-due due :available-at due)))
                   (cond
                    (night
                     (push task srs-pending))
                    ((not (time-less-p now due))   ; due <= now
                     (push task srs-out))
                    ((org-queue--same-day-p due now)
                     (push task srs-pending)))))))
            ;; --- non-SRS entries
            (t
             (let* ((is-todo (my-is-todo-task))
                    (task (list :id id :marker marker :priority priority
                                :file file :is-todo is-todo :heading heading :pos pos)))
               (setq task (org-queue--update-available-at! task))
               (when (org-queue--in-scope-p task)
                 (let ((avail (plist-get task :available-at)))
                   (when avail
                     (if (not (time-less-p now avail))
                         (push task non-srs-out)
                       (push task non-srs-pending)))))))))))
     nil)

    ;; pool-local ordering
    (setq non-srs-out (cl-stable-sort non-srs-out #'org-queue--non-srs-key<=))
    (setq srs-out     (cl-stable-sort srs-out     #'org-queue--non-srs-key<=))

    ;; interleave head
    (let* ((ratio (if night '(1 . 0)
                    (or (and (boundp 'org-queue-srs-mix-ratio) org-queue-srs-mix-ratio)
                        '(1 . 4))))
           (a (car ratio)) (b (cdr ratio))
           (start (org-queue--decide-mix-start a b non-srs-out srs-out))
           (mixed (org-queue--interleave-by-ratio-start
                   (copy-sequence non-srs-out) (copy-sequence srs-out) a b start)))
      (setq my-outstanding-tasks-list mixed))

    ;; pending (dedupe)
    (let* ((pending (append (nreverse non-srs-pending) (nreverse srs-pending)))
           (seen (make-hash-table :test 'equal))
           (acc '()))
      (dolist (task pending)
        (let ((k (my--task-unique-key task)))
          (unless (and k (gethash k seen))
            (when k (puthash k t seen))
            (push task acc))))
      (setq my-today-pending-tasks (nreverse acc)))

    (my-dedupe-outstanding-tasks)
    (let ((todo-count (length (cl-remove-if-not (lambda (t) (plist-get t :is-todo))
                                                my-outstanding-tasks-list))))
      (message (if night
                   "=== QUEUE BUILT (single-pass; SRS suppressed for night shift) ==="
                 "=== QUEUE BUILT (single-pass; SRS integrated) ==="))
      (message "Outstanding: %d (TODO: %d) | Pending≤today: %d"
               (length my-outstanding-tasks-list) todo-count
               (length my-today-pending-tasks)))
    (setq my-outstanding-tasks-index 0)
    (my-queue-limit-visible-buffers)))

(defun my-remove-current-task ()
  "Remove the task at the current index from the queue and keep buffers tidy.

Behavior:
- Removes the current task from `my-outstanding-tasks-list`.
- Keeps the index pointing to the next task (or last task if you removed the last).
- Buries the removed task's buffer.
- Limits visible queue buffers to `my-queue-visible-buffer-count`.
- Saves the cache and index.
- Shows the new current task if any."
  (interactive)
  (my-ensure-task-list-present)

  ;; Validate
  (unless (and my-outstanding-tasks-list
               (> (length my-outstanding-tasks-list) 0)
               (>= my-outstanding-tasks-index 0)
               (< my-outstanding-tasks-index (length my-outstanding-tasks-list)))
    (user-error "No valid task at current index to remove"))

  (let* ((old-index my-outstanding-tasks-index)
         (old-list my-outstanding-tasks-list)
         (removed-task (nth old-index old-list))
         (removed-buffer (my-safe-marker-buffer removed-task)))
    ;; Remove from list
    (setq my-outstanding-tasks-list
          (append (seq-take old-list old-index)
                  (seq-drop old-list (1+ old-index))))

    (cond
     ;; If the queue became empty, finalize and rebuild.
     ((zerop (length my-outstanding-tasks-list))
      (when (buffer-live-p removed-buffer)
        (bury-buffer removed-buffer))
      (setq my-outstanding-tasks-index 0)
      (message "Task removed. Queue is now empty.")
      ;; Optionally rebuild right away
      (my-get-outstanding-tasks)
      (setq my-outstanding-tasks-index 0))

     ;; Otherwise, keep index pointing to the next item (or clamp to last).
     (t
      (let ((new-length (length my-outstanding-tasks-list)))
        (when (>= my-outstanding-tasks-index new-length)
          (setq my-outstanding-tasks-index (1- new-length))))
      (when (buffer-live-p removed-buffer)
        (bury-buffer removed-buffer))
      ;; Limit visible queue buffers (no global MRU reordering)
      (my-queue-limit-visible-buffers)
      ;; Save
      (message "Task removed. Now at %d/%d"
               (1+ my-outstanding-tasks-index)
               (length my-outstanding-tasks-list))
      (org-queue-show-top)))))

;; Task management functions
(defun my-auto-postpone-overdue-tasks ()
  "Auto-postpone all overdue TODO tasks (excluding DONE tasks)."
  (interactive)
  (save-some-buffers t)
  (let ((processed-count 0)
        (total-overdue 0))
    (let ((org-queue--suppress-ui t))
      ;; First pass: count
      (org-queue-map-entries
       (lambda ()
         (when (and (my-is-overdue-task) (my-is-todo-task))
           (setq total-overdue (1+ total-overdue))))
       nil)
      ;; Second pass: process
      (org-queue-map-entries
       (lambda ()
         (when (and (my-is-overdue-task) (my-is-todo-task))
           (my-ensure-priority-set)
           (my-postpone-schedule)
           (setq processed-count (1+ processed-count))
           (when (= (mod processed-count 10) 0)
             (message "Processed %d/%d overdue TODO tasks..." processed-count total-overdue))))
       nil))
    (save-some-buffers t)
    (message "✓ Auto-postponed %d overdue TODO tasks." processed-count)))

(defun my-postpone-duplicate-priority-tasks ()
  "In each file, keep only one outstanding task per numeric priority; postpone duplicates."
  (interactive)
  (let ((files (org-queue-file-list)))
    (unless files
      (user-error "No Org files for org-queue. Configure `org-queue-directory` or `org-queue-file-roots`."))
    (let ((seen (make-hash-table :test 'equal)))
      (dolist (file files)
        (when (file-exists-p file)
          (with-current-buffer (find-file-noselect file)
            (let ((org-queue--suppress-ui t))
              (save-excursion
                (org-map-entries
                (lambda ()
                  (when (my-is-outstanding-task)
                    (let* ((pstr (org-entry-get nil "PRIORITY"))
                            (prio (if pstr (string-to-number pstr) org-priority-default))
                            (key (cons (file-truename file) prio)))
                      (if (gethash key seen)
                          (progn
                            (my-postpone-schedule)
                            (message "Postponed duplicate priority %d in %s"
                                      prio (file-name-nondirectory file)))
                        (puthash key t seen)))))
                nil 'file)))))
    (save-some-buffers t)
    (message "Processed duplicate outstanding priorities.")))))

(defun my-enforce-priority-constraints ()
  "Enforce monotone cap: lower priorities can't outnumber any higher priority.
Walk priorities in ascending order; postpone overflow FIFO within each priority."
  (interactive)
  (let* ((files (org-queue-file-list))
         (priority-counts (make-hash-table :test 'equal))
         (tasks-by-priority (make-hash-table :test 'equal)))
    (unless files
      (user-error "No Org files for org-queue. Configure `org-queue-directory` or `org-queue-file-roots`."))
    ;; Collect outstanding entries by numeric PRIORITY.
    (dolist (file files)
      (when (file-exists-p file)
        (with-current-buffer (find-file-noselect file)
          (save-excursion
            (org-map-entries
             (lambda ()
               (when (my-is-outstanding-task)
                 (let* ((pstr  (org-entry-get nil "PRIORITY"))
                        (prio  (if pstr (string-to-number pstr) org-priority-default))
                        (entry (cons (buffer-file-name) (point))))
                   (puthash prio
                            (nconc (gethash prio tasks-by-priority) (list entry))
                            tasks-by-priority)
                   (puthash prio
                            (1+ (gethash prio priority-counts 0))
                            priority-counts))))
             nil 'file)))))

    (let* ((sorted (sort (copy-sequence (hash-table-keys priority-counts)) #'<))
           (current-max nil))
      (if (zerop (length sorted))
          (message "[COMPLETE] No outstanding tasks found")
        (let ((org-queue--suppress-ui t))
          (dolist (prio sorted)
            (let* ((count (gethash prio priority-counts 0))
                   (fifo  (copy-sequence (gethash prio tasks-by-priority))))
              (cond
               ((zerop count))  ;; nothing at this priority
               ((null current-max)
                (setq current-max count)
                (message "[CONSTRAINT] Pri %d sets cap to %d" prio current-max))
               ((> count current-max)
                (let ((excess (- count current-max)))
                  (message "[ENFORCE] Pri %d overflow (%d > %d). Postponing %d..."
                           prio count current-max excess)
                  (cl-loop repeat excess
                           for entry = (pop fifo)
                           while entry
                           do (pcase-let ((`(,file . ,pos) entry))
                                (with-current-buffer (find-file-noselect file)
                                  (goto-char pos)
                                  (my-postpone-schedule)))))))
              ;; write back the reduced FIFO for this priority
              (puthash prio fifo tasks-by-priority))
            ;; new cap is min(old cap, count at this prio), but keep it numeric
            (setq current-max (if (numberp current-max)
                                  (min current-max (gethash prio priority-counts 0))
                                (gethash prio priority-counts 0)))))
        (save-some-buffers t)
        (message "[COMPLETE] Constraint enforcement done; final cap %s"
                 (or current-max 0))))))

(defun my-postpone-consecutive-same-file-tasks ()
  "Postpone consecutive tasks from the same file, keeping only the first.
Saves buffers and regenerates the task list for consistency."
  (interactive)
  (my-reset-outstanding-tasks-index)
  (when my-outstanding-tasks-list
    (let ((prev-file nil)
          (modified-buffers nil)
          (original-count (length my-outstanding-tasks-list)))
      ;; Iterate over the task PLISTS, not raw markers
      (dolist (entry my-outstanding-tasks-list)
        (let ((org-queue--suppress-ui t))
          (let* ((marker (my-extract-marker entry))
                (buffer (and (markerp marker) (marker-buffer marker)))
                (file   (and buffer (buffer-file-name buffer))))
            (when buffer
              (if (and file (equal file prev-file))
                  (progn
                    (with-current-buffer buffer
                      (goto-char (marker-position marker))
                      (my-postpone-schedule))
                    (push buffer modified-buffers)
                    (message "Postponed duplicate in: %s"
                            (file-name-nondirectory file)))
                (setq prev-file file))))))
      ;; Save all changed buffers
      (dolist (buf (delete-dups modified-buffers))
        (when (buffer-live-p buf)
          (with-current-buffer buf
            (save-buffer))))
      ;; If we actually postponed anything, rebuild the list
      (when modified-buffers
        (my-get-outstanding-tasks)
        (message "%d consecutive task(s) postponed, list regenerated."
                 (- original-count (length my-outstanding-tasks-list))))))
  (my-reset-outstanding-tasks-index))

;; Task navigation functions
(defun my-reset-outstanding-tasks-index ()
  "Reset the outstanding tasks index to start from the first task."
  (interactive)
  (my-get-outstanding-tasks)
  (setq my-outstanding-tasks-index 0)
  (message "Outstanding tasks index reset."))

(defun my-reset-and-show-current-outstanding-task ()
  "Reset the outstanding tasks index and then show the top outstanding task."
  (interactive)
  (my-launch-anki)
  (my-reset-outstanding-tasks-index)
  (org-queue-show-top))

(defun my-show-current-outstanding-task-no-srs (&optional pulse)
  "Show current outstanding task from the in-memory queue without SRS/Anki."
  (interactive)
  (widen-and-recenter)
  (my-ensure-task-list-present)
  (if (and my-outstanding-tasks-list
           (< my-outstanding-tasks-index (length my-outstanding-tasks-list)))
      (let ((task (nth my-outstanding-tasks-index my-outstanding-tasks-list)))
        (my-display-task-at-marker task)
        (when pulse (my-pulse-highlight-current-line))
        (my-queue-limit-visible-buffers))
    (message "No outstanding tasks found.")))

(defun org-queue-show-top (&optional pulse)
  (interactive)
  (my-ensure-task-list-present)
  ;; Promote anything that just became available (can be O(n), keep it)
  (ignore-errors (org-queue--promote-pending-due-now!))
  (if (or (null my-outstanding-tasks-list)
          (zerop (length my-outstanding-tasks-list)))
      (message "No outstanding tasks found.")
    (setq my-outstanding-tasks-index 0)
    (let ((pruned 0)
          (org-queue--suppress-ui t))  ;; suppress chooser churn while pruning
      (while (and my-outstanding-tasks-list
                  (>= my-outstanding-tasks-index 0)
                  (< my-outstanding-tasks-index (length my-outstanding-tasks-list))
                  (not (my-queue--task-due-p (nth 0 my-outstanding-tasks-list))))
        ;; Remove head quietly (no save), keep index at 0
        (my-queue--remove-index 0 t t)
        (setq my-outstanding-tasks-index 0)
        (setq pruned (1+ pruned))))
    ;; Show current head immediately; feels instant
    (my-show-current-outstanding-task-no-srs (or pulse t))))

(defvar org-queue--midnight-timer nil
  "Timer that triggers a daily midnight refresh of org-queue.")

(defvar org-queue--pending-promotion-timer nil)

(defun org-queue-start-pending-promotion-timer ()
  "Start or restart a periodic timer that promotes due pending tasks."
  (when org-queue--pending-promotion-timer
    (cancel-timer org-queue--pending-promotion-timer))
  (setq org-queue--pending-promotion-timer
        (run-with-timer 60 60  ;; first in 60s, then every 60s
                        (lambda ()
                          (ignore-errors (org-queue--promote-pending-due-now!))))))

(defvar-local org-queue--micro-update-timer nil)

(defun org-queue--request-micro-update (&optional reason)
  "Coalesce queue micro-updates on idle to avoid thrashing.
Schedules a single micro-update + autosave (+ optional show-top) after brief idle."
  (when org-queue--micro-update-timer
    (cancel-timer org-queue--micro-update-timer))
  (let ((buf (current-buffer))
        (why reason))
    (setq org-queue--micro-update-timer
          (run-with-idle-timer
           0.2 nil
           (lambda (b r)
             (when (buffer-live-p b)
               (with-current-buffer b
                 (ignore-errors (org-queue--micro-update-current! r))
                 (org-queue--autosave-current)
                 (when org-queue-auto-show-top-after-change
                   (org-queue-show-top t)))))
           buf why))))

(defun org-queue--midnight-refresh ()
  "Clear today's pending, rebuild queues, save, and show top. Reschedule for next midnight."
  (setq my-today-pending-tasks nil)
  (my-get-outstanding-tasks)
  ;; Refresh queue markers strictly inside their files; drop unresolved ones.
  (let* ((files (delete-dups
                 (delq nil
                       (mapcar (lambda (t) (plist-get t :file))
                               (append my-outstanding-tasks-list
                                       my-today-pending-tasks))))))
    (dolist (f files)
      (ignore-errors
        (org-queue-id-guard--refresh-queue-markers-for-file f t))))
  (ignore-errors (org-queue-show-top t))
  ;; Also trigger daily maintenance for the new day (gated; runs once).
  (org-queue-daily-maintenance-maybe)
  ;; Reschedule
  (run-at-time 1 nil #'org-queue--schedule-midnight-refresh))

(defun org-queue--schedule-midnight-refresh ()
  "Schedule org-queue midnight refresh for the next local midnight."
  (when org-queue--midnight-timer
    (cancel-timer org-queue--midnight-timer)
    (setq org-queue--midnight-timer nil))
  (let* ((now (current-time))
         (dec (decode-time now))
         (year (nth 5 dec))
         (month (nth 4 dec))
         (day (nth 3 dec))
         (next-midnight (encode-time 0 0 0 (1+ day) month year)))
    (setq org-queue--midnight-timer
          (run-at-time next-midnight nil #'org-queue--midnight-refresh))))

;; Utility functions for task management
(defun org-queue-startup ()
  "org-queue startup:
- Initialize org-id DB,
- If cache exists for today, use it;
  otherwise, build and save the queue.
- Schedule initial task display (no automatic SRS/Anki here)."
  (interactive)
  (my-launch-anki)
  ;; IDs first
  (my-org-id-initialize-id-locations)

  ;; Build queue in-memory (no disk cache)
  (org-queue--with-batched-saves
    (my-get-outstanding-tasks)
    (setq my-outstanding-tasks-index 0))
  (message "org-queue: queue built")

  ;; Fast prune current queue (removes SRS during night shift and any not-due items)
  (ignore-errors (my-queue-filter-not-due-in-place t))

  ;; Schedule task display (no SRS auto-start here)
  (run-with-idle-timer 1.5 nil
                       (lambda ()
                         (condition-case err
                             (progn
                               (delete-other-windows)
                               (org-queue-show-top)) ;; single-head
                           (error
                            (message "Error preparing task display: %s" (error-message-string err))))))
  ;; Kick off today's maintenance if needed (lazy; runs once per day).
  (org-queue-daily-maintenance-maybe))

;; Automatic startup
(add-hook 'emacs-startup-hook #'org-queue-startup 100)

(defun org-queue-maintenance ()
  "Run org-queue maintenance.
Runs the complete maintenance pipeline and flushes a single save at the end."
  (interactive)
  (my-org-id-initialize-id-locations)

  ;; Suppress UI refresh during the heavy run; batch-suppress saves and flush once at the end.
  (let ((org-queue--suppress-ui t))
    (org-queue--without-autosave
      (progn
        ;; Optional: org-roam one-shot DB sync (no autosync hooks)
        (when (require 'org-roam nil t)
          (let ((warning-minimum-level :error))
            (when (fboundp 'org-roam-db-sync)
              (ignore-errors (org-roam-db-sync)))))

        ;; Ensure base invariants
        (when (fboundp 'my-ensure-priorities-and-schedules-for-all-headings)
          (my-ensure-priorities-and-schedules-for-all-headings))

        ;; Advance near-future schedules so the queue is meaningful today
        (when (fboundp 'my-auto-advance-schedules)
          (my-auto-advance-schedules 8))

        ;; Reduce noise
        (my-auto-postpone-overdue-tasks)
        (my-postpone-duplicate-priority-tasks)

        ;; Enforce "higher priority caps lower priority counts"
        (when (fboundp 'my-enforce-priority-constraints)
          (my-enforce-priority-constraints))

        ;; Re-ensure after changes
        (when (fboundp 'my-ensure-priorities-and-schedules-for-all-headings)
          (my-ensure-priorities-and-schedules-for-all-headings))

        ;; Keep only the first consecutive task per file
        (when (fboundp 'my-postpone-consecutive-same-file-tasks)
          (my-postpone-consecutive-same-file-tasks))

        ;; Remove DONE cruft
        (when (fboundp 'my-cleanup-all-done-tasks)
          (my-cleanup-all-done-tasks)))

      ;; Rebuild/refresh lists (single canonical path)
      (my-get-outstanding-tasks)))

  ;; After the macro exits, a single save-some-buffers t has been executed.
  (delete-other-windows)
  (org-queue-show-top t)
  (org-queue--write-maintenance-stamp)
  (save-some-buffers t)
  (message "org-queue: maintenance complete.")
  (ignore-errors (org-queue--schedule-midnight-refresh)))

 ;; ------------------------------------------------------------
 ;; LIVE MICRO-TRACKING (single path; always enabled)
 ;; Recompute queue on any change to SCHEDULED or PRIORITY.
 ;; ------------------------------------------------------------

(defun org-queue--after-schedule (&rest _)
  "After `org-schedule`, reflect the change in the queue and save."
  (when (derived-mode-p 'org-mode)
    (unless org-queue--suppress-micro-update
      (org-queue--request-micro-update 'schedule))))

(defun org-queue--after-priority (&rest _)
  "After priority commands, reflect the change in the queue and save."
  (when (derived-mode-p 'org-mode)
    (unless org-queue--suppress-micro-update
      (org-queue--request-micro-update 'priority))))

;; Advice core commands (covers C-c C-s and priority up/down)
(unless (advice-member-p #'org-queue--after-schedule 'org-schedule)
  (advice-add 'org-schedule :after #'org-queue--after-schedule))
(dolist (fn '(org-priority org-priority-up org-priority-down))
  (unless (advice-member-p #'org-queue--after-priority fn)
    (advice-add fn :after #'org-queue--after-priority)))

(defun org-queue--on-change-scheduled-or-priority (beg end _len)
  (when (eq major-mode 'org-mode)
    (save-excursion
      (goto-char beg)
      (when (re-search-forward
             "\\(SCHEDULED:[^\n]*\\|\\[#[ \t]*[0-9]+[ \t]*\\]\\)" end t)
        (unless org-queue--suppress-micro-update
          (ignore-errors (org-back-to-heading t))
          (org-queue--request-micro-update 'edit))))))

(add-hook 'org-mode-hook
          (lambda ()
            (add-hook 'after-change-functions
                      #'org-queue--on-change-scheduled-or-priority
                      nil t)))


(provide 'org-queue-tasks)
;;; org-queue-tasks.el ends here
