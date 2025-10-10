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

(defcustom my-queue-visible-buffer-count 8
  "How many distinct queue buffers to keep readily visible (not buried).
The set is centered on the current queue index, scanning forward with wrap."
  :type 'integer
  :group 'org-queue)

(defun my-queue--buffer-at-index (idx)
  "Return the buffer of the queue entry at IDX or nil."
  (when (and my-outstanding-tasks-list
             (>= idx 0)
             (< idx (length my-outstanding-tasks-list)))
    (my-safe-marker-buffer (nth idx my-outstanding-tasks-list))))

(defun my-queue-limit-visible-buffers (&optional n)
  "Bury all queue buffers except the next N distinct ones from current index.
N defaults to `my-queue-visible-buffer-count'. This does not reorder buffers,
only buries the ones that should not be in the immediate working set."
  (let* ((count (or n my-queue-visible-buffer-count))
         (len (length my-outstanding-tasks-list)))
    (when (and (> len 0) (> count 0))
      ;; Collect allowed buffers: starting at current index, scan forward with wrap,
      ;; collecting distinct buffers until we have COUNT of them (or we exhaust).
      (let ((allowed (make-hash-table :test 'eq))
            (collected 0)
            (i 0))
        (while (and (< collected count) (< i len))
          (let* ((idx (mod (+ my-outstanding-tasks-index i) len))
                 (buf (my-queue--buffer-at-index idx)))
            (when (and buf (buffer-live-p buf) (not (gethash buf allowed)))
              (puthash buf t allowed)
              (setq collected (1+ collected))))
          (setq i (1+ i)))
        ;; Now bury all queue buffers that are not allowed
        (dotimes (j len)
          (let ((buf (my-queue--buffer-at-index j)))
            (when (and buf (buffer-live-p buf) (not (gethash buf allowed)))
              (bury-buffer buf))))))))

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

(defun my-queue--refresh-chooser-buffers ()
  (dolist (b (list "*Org Queue*" "*Org Queue (Subset)*"))
    (when (get-buffer b)
      (with-current-buffer b
        (when (derived-mode-p 'org-queue-chooser-mode)
          (when (fboundp 'org-queue-chooser-refresh)
            (ignore-errors (org-queue-chooser-refresh))))))))

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
      (unless no-save
        (my-save-outstanding-tasks-to-file))
      (unless quiet
        (message "Removed 1 item from queue; now %d total."
                 (length my-outstanding-tasks-list)))
      (my-queue--refresh-chooser-buffers)
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
      (my-save-outstanding-tasks-to-file)
      (my-queue--refresh-chooser-buffers)
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

(defun org-queue--reassign-top-after-change (&optional reason)
  "Recompute placement of the current top item after a change (schedule/priority/snooze).
No file scan. Updates:
- my-outstanding-tasks-list (resorts and re-mixes SRS by ratio, with night-shift gating),
- my-today-pending-tasks (adds/removes the top as needed),
- my-outstanding-tasks-index set to 0,
- Saves cache and refreshes chooser,
- Displays the new top (no SRS auto-start).

Optional REASON is a symbol for logging/debugging (e.g., 'schedule, 'advance, 'postpone, 'snooze)."
  (let* ((now (org-queue--now))
         (night (org-queue-night-shift-p)))
    (my-ensure-task-list-present)
    ;; Always operate on index 0 (single-head discipline)
    (setq my-outstanding-tasks-index 0)
    (if (or (null my-outstanding-tasks-list)
            (zerop (length my-outstanding-tasks-list)))
        (progn
          (my-save-outstanding-tasks-to-file)
          (my-queue--refresh-chooser-buffers)
          (message "No outstanding tasks after change"))
      (let* ((top (nth 0 my-outstanding-tasks-list))
             (key (my--task-unique-key top)))
        ;; Refresh availability of TOP
        (setq top (org-queue--update-available-at! top))
        (let* ((is-srs (plist-get top :srs))
               (avail  (plist-get top :available-at))
               ;; Classification helpers
               (due-now (and avail (not (time-less-p now avail))))
               (is-today (org-queue--task-is-today-p top)))
          ;; Remove TOP from both lists by unique key
          (setq my-outstanding-tasks-list
                (cl-remove-if (lambda (task) (equal (my--task-unique-key task) key))
                              my-outstanding-tasks-list))
          (setq my-today-pending-tasks
                (cl-remove-if (lambda (task) (equal (my--task-unique-key task) key))
                              my-today-pending-tasks))
          ;; Partition remaining outstanding into non-SRS and SRS
          (let ((cur-non-srs '())
                (cur-srs '()))
            (dolist (task my-outstanding-tasks-list)
              (if (plist-get task :srs) (push task cur-srs) (push task cur-non-srs)))
            (setq cur-non-srs (nreverse cur-non-srs))
            (setq cur-srs     (nreverse cur-srs))
            ;; Decide where TOP goes now
            (cond
             ;; SRS path
             (is-srs
              (cond
               ;; Off-today (no due today): remove from both
               ((not is-today)
                ;; nothing to add
                )
               ;; Today but night shift: pending only
               (night
                (push top my-today-pending-tasks))
               ;; Today and not night-shift:
               (due-now
                (push top cur-srs))
               (t
                (push top my-today-pending-tasks))))
             ;; Non-SRS path
             (t
              (cond
               ;; Off-today (rescheduled): remove from both
               ((not is-today)
                ;; nothing to add
                )
               ;; Today and due-now: outstanding non-SRS
               (due-now
                (push top cur-non-srs))
               ;; Today but not yet available: pending
               (t
                (push top my-today-pending-tasks)))))

            ;; Resort current non-SRS outstanding with tertiary LAST_REPEAT sort
            (setq cur-non-srs
                  (cl-stable-sort cur-non-srs
                                  (lambda (a b)
                                    (let* ((ka (org-queue--non-srs-sort-key a))
                                           (kb (org-queue--non-srs-sort-key b)))
                                      (or (< (nth 0 ka) (nth 0 kb))
                                          (and (= (nth 0 ka) (nth 0 kb))
                                               (or (< (nth 1 ka) (nth 1 kb))
                                                   (and (= (nth 1 ka) (nth 1 kb))
                                                        (< (nth 2 ka) (nth 2 kb))))))))))
            (setq cur-srs
                  (cl-stable-sort cur-srs
                                  (lambda (a b)
                                    (let ((da (or (plist-get a :srs-due) (plist-get a :available-at)))
                                          (db (or (plist-get b :srs-due) (plist-get b :available-at))))
                                      (cond
                                       ((and da db) (time-less-p da db))
                                       (da t)
                                       (db nil)
                                       (t nil))))))

            ;; Interleave by ratio (suppress SRS entirely during night shift), start-aware
            (let* ((ratio (if night '(1 . 0)
                            (or (and (boundp 'org-queue-srs-mix-ratio)
                                     org-queue-srs-mix-ratio)
                                '(1 . 4))))
                   (a-count (car ratio))
                   (b-count (cdr ratio))
                   (start (org-queue--decide-mix-start a-count b-count cur-non-srs cur-srs))
                   (mixed (org-queue--interleave-by-ratio-start
                           (copy-sequence cur-non-srs)
                           (copy-sequence cur-srs)
                           a-count b-count start)))
              (setq my-outstanding-tasks-list mixed)))

          ;; Dedup pending (by unique key)
          (let ((seen (make-hash-table :test 'equal))
                (acc '()))
            (dolist (task (nreverse my-today-pending-tasks))
              (let ((k (my--task-unique-key task)))
                (unless (and k (gethash k seen))
                  (when k (puthash k t seen))
                  ;; Ensure pending has availability set
                  (push (or (and (plist-get task :available-at) task)
                            (org-queue--update-available-at! task))
                        acc))))
            (setq my-today-pending-tasks (nreverse acc)))

          ;; Finalize; optionally show top
          (setq my-outstanding-tasks-index 0)
          (my-save-outstanding-tasks-to-file)
          (my-queue--refresh-chooser-buffers)
          (when (and (not org-queue--suppress-ui)
                     org-queue-auto-show-top-after-change)
            (org-queue-show-top t)))))))

(defun org-queue-rebuild-soft ()
  "Promote items from `my-today-pending-tasks` into the outstanding list without rescanning files.
Behavior:
- Promote pending items whose :available-at <= now (SRS gated by night shift).
- Filter out any no-longer-due items from outstanding (e.g., after SRS rating or night-shift toggle).
- Resort outstanding non-SRS; interleave with outstanding SRS using the configured ratio.
- Do not touch disk file rosters."
  (interactive)
  (my-launch-anki)
  (my-ensure-task-list-present)

  (let* ((now (org-queue--now))
         (night (org-queue-night-shift-p))
         (prom-non-srs '())
         (prom-srs '())
         (keep-pending '()))
    ;; Recompute availability and decide promotion for each pending item
    (dolist (task my-today-pending-tasks)
      (setq task (org-queue--update-available-at! task))
      (let* ((is-srs (plist-get task :srs))
             (avail (plist-get task :available-at)))
        (cond
         ;; No availability (e.g., rescheduled off today): drop from today's pending
         ((null avail) nil)
         ;; Still not available
         ((time-less-p now avail)
          (push task keep-pending))
         ;; SRS: only promote when not night-shift
         (is-srs
          (if night
              (push task keep-pending)
            (push task prom-srs)))
         ;; Non-SRS: promote
         (t
          (push task prom-non-srs)))))

    ;; Partition current outstanding into non-SRS and SRS, and drop any no-longer-due items
    (let ((cur-non-srs '())
          (cur-srs '()))
      (dolist (task my-outstanding-tasks-list)
        (if (plist-get task :srs)
            (push task cur-srs)
          (push task cur-non-srs)))
      (setq cur-non-srs (nreverse cur-non-srs))
      (setq cur-srs (nreverse cur-srs))
      ;; Remove entries that are no longer due now (e.g., SRS just rated, or night-shift just toggled)
      (setq cur-non-srs (cl-remove-if-not #'my-queue--task-due-p cur-non-srs))
      (setq cur-srs     (cl-remove-if-not #'my-queue--task-due-p cur-srs))
      ;; Merge promotions
      (setq cur-non-srs (append cur-non-srs prom-non-srs))
      (setq cur-srs     (append cur-srs     prom-srs))
      ;; Sort non-SRS outstanding: TODO-first, priority asc, LAST_REPEAT asc (older first).
      (setq cur-non-srs
            (cl-stable-sort cur-non-srs
                            (lambda (a b)
                              (let* ((ka (org-queue--non-srs-sort-key a))
                                     (kb (org-queue--non-srs-sort-key b)))
                                (or (< (nth 0 ka) (nth 0 kb))
                                    (and (= (nth 0 ka) (nth 0 kb))
                                         (or (< (nth 1 ka) (nth 1 kb))
                                             (and (= (nth 1 ka) (nth 1 kb))
                                                  (< (nth 2 ka) (nth 2 kb))))))))))
      ;; Sort SRS by due/available-at ascending for stability
      (setq cur-srs
            (cl-stable-sort cur-srs
                            (lambda (a b)
                              (let ((da (or (plist-get a :srs-due) (plist-get a :available-at)))
                                    (db (or (plist-get b :srs-due) (plist-get b :available-at))))
                                (cond
                                 ((and da db) (time-less-p da db))
                                 (da t)
                                 (db nil)
                                 (t nil))))))
      ;; Interleave by configured ratio; suppress SRS entirely during night shift
      (let* ((ratio (if night '(1 . 0)
                      (or (and (boundp 'org-queue-srs-mix-ratio)
                               org-queue-srs-mix-ratio)
                          '(1 . 4))))
             (a-count (car ratio))
             (b-count (cdr ratio))
             (start (org-queue--decide-mix-start a-count b-count cur-non-srs cur-srs))
             (mixed (org-queue--interleave-by-ratio-start
                     (copy-sequence cur-non-srs)
                     (copy-sequence cur-srs)
                     a-count b-count start)))
        (setq my-outstanding-tasks-list mixed)))

    ;; Dedupe outstanding
    (my-dedupe-outstanding-tasks)

    ;; Rebuild today's pending list from keep-pending (dedup)
    (let ((seen (make-hash-table :test 'equal))
          (acc '()))
      (dolist (task (nreverse keep-pending))
        (let ((k (my--task-unique-key task)))
          (unless (and k (gethash k seen))
            (when k (puthash k t seen))
            (push task acc))))
      (setq my-today-pending-tasks (nreverse acc)))

    ;; Reset index to top, save, and refresh UI
    (setq my-outstanding-tasks-index 0)
    (my-save-outstanding-tasks-to-file)
    (my-queue--refresh-chooser-buffers)
    (unless org-queue--suppress-ui
      (org-queue-show-top t))
    (message "org-queue: soft promotion done — outstanding: %d, pending today: %d"
             (length my-outstanding-tasks-list)
             (length my-today-pending-tasks))))

;; Variables for task list management
(defvar my-outstanding-tasks-list nil
  "List of outstanding tasks, sorted by priority.")

(defvar my-outstanding-tasks-index 0
  "Current index in the outstanding tasks list.")

(defvar my-today-pending-tasks nil
  "List of today's pending tasks (not yet available).
Each element is a task plist (same shape as outstanding) with :available-at set.")

;; Define a customizable variable for the cache file.
(defcustom my-outstanding-tasks-cache-file
  (expand-file-name "outstanding.cache" org-queue-cache-dir)
  "File path to store the cached outstanding tasks list along with its date stamp."
  :type 'file
  :group 'org-queue)

(defcustom my-outstanding-tasks-index-file
  (expand-file-name "index.cache" org-queue-cache-dir)
  "File path to store the current task index."
  :type 'file
  :group 'org-queue)

(defvar my-org-id-locations-initialized nil
  "Whether org-id locations DB has been fully initialized this session.")

(defun my-org-id-initialize-id-locations ()
  "Scan all org-queue files once to build the org-id cache."
  (unless my-org-id-locations-initialized
    (setq my-org-id-locations-initialized t)
    (when (fboundp 'org-id-update-id-locations)
      (ignore-errors
        (org-id-update-id-locations (org-queue-file-list))))))

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

;; Marker extraction and safety functions
(defun my-extract-marker (task-or-marker)
  "Robustly extract a live marker from TASK-OR-MARKER.
Strategy:
- If a live marker is present, use it and sync plist metadata.
- Else resolve by :id within :file, preferring matching :heading, else closest to :pos (and sync).
- Else fallback to org-id-find (and sync).
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
             (file    (plist-get task-or-marker :file))
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
        ;; Fallback to org-id DB if file scan failed
        (when (and (not result) id (fboundp 'org-id-find))
          (let ((mk (org-id-find id 'marker)))
            (when (and mk
                       (marker-buffer mk)
                       (buffer-live-p (marker-buffer mk)))
              (setq result (my--task-sync-metadata task-or-marker mk)))))
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
  "Return a live marker for heading UUID, preferring FILE when given."
  (when (and (stringp uuid) (> (length uuid) 0))
    (let ((m (org-id-find uuid 'marker)))
      (cond
       ;; If the found marker is already in the right file, use it.
       ((and m file
             (buffer-live-p (marker-buffer m))
             (with-current-buffer (marker-buffer m)
               (and buffer-file-name
                    (file-equal-p (file-truename buffer-file-name)
                                  (file-truename file)))))
        m)
       ;; Otherwise, search the specific FILE directly for the ID drawer.
       ((and file (file-exists-p file))
        (with-current-buffer (find-file-noselect file)
          (save-restriction
            (widen)
            (save-excursion
              (goto-char (point-min))
              (if (re-search-forward
                   (concat "^[ \t]*:ID:[ \t]*" (regexp-quote uuid) "[ \t]*$") nil t)
                  (progn (org-back-to-heading t) (point-marker))
                ;; fallback to whatever org-id-find returned
                m)))))
       (t m)))))

(defun my-position-from-uuid (uuid)
  "Return the buffer position (point) of the Org heading with UUID (its :ID:).
Returns nil if not found."
  (let ((m (my-marker-from-uuid uuid)))
    (when (markerp m)
      (marker-position m))))

;; Task list persistence functions
(defun my-save-outstanding-tasks-to-file ()
  "Save task list to cache using unique (file . id) pairs (dedup by ID).
Also saves the index separately."
  ;; Ensure cache dir exists
  (let ((dir (file-name-directory my-outstanding-tasks-cache-file)))
    (when (and dir (not (file-directory-p dir)))
      (ignore-errors (make-directory dir t))))
  (let* ((today (format-time-string "%Y-%m-%d"))
         (seen (make-hash-table :test 'equal))
         (pairs '()))
    (dolist (entry my-outstanding-tasks-list)
      (let* ((marker (my-extract-marker entry)))
        (when (and (markerp marker)
                   (marker-buffer marker)
                   (buffer-live-p (marker-buffer marker))
                   (buffer-file-name (marker-buffer marker)))
          (with-current-buffer (marker-buffer marker)
            (save-restriction
              (widen)
              (save-excursion
                (goto-char (marker-position marker))
                (let* ((id   (org-entry-get nil "ID"))
                       (full (file-truename (buffer-file-name)))
                       (path (if (and (boundp 'org-queue-directory)
                                      org-queue-directory
                                      (file-in-directory-p full org-queue-directory))
                                 (file-relative-name full org-queue-directory)
                               full)))
                  (when (and id (not (gethash id seen)))
                    (puthash id t seen)
                    (push (cons path id) pairs)))))))))
    (with-temp-file my-outstanding-tasks-cache-file
      (insert (prin1-to-string (list :date today
                                     :tasks (nreverse pairs)))))
    (my-save-index-to-file)))

(defun my-save-index-to-file ()
  "Save the current task index to a device-specific file."
  ;; Ensure cache dir exists
  (let ((dir (file-name-directory my-outstanding-tasks-index-file)))
    (when (and dir (not (file-directory-p dir)))
      (ignore-errors (make-directory dir t))))
  (with-temp-file my-outstanding-tasks-index-file
    (insert (prin1-to-string (list :index my-outstanding-tasks-index
                                   :timestamp (current-time))))))

(defun my-load-index-from-file ()
  "Load the task index from device-specific file."
  (when (file-exists-p my-outstanding-tasks-index-file)
    (condition-case nil
        (let* ((data (with-temp-buffer
                       (insert-file-contents my-outstanding-tasks-index-file)
                       (read (buffer-string))))
               (saved-index (plist-get data :index)))
          (when (and saved-index (numberp saved-index) (>= saved-index 0))
            (setq my-outstanding-tasks-index saved-index)
            t))
      (error nil))))

(defun my-load-outstanding-tasks-from-file ()
  "Load cached tasks stored as (file . id) pairs and rebuild plist tasks with markers.
Dedupes by ID; reindexes unresolved IDs' files ONCE (grouped); rewrites cache deduped."
  (when (file-exists-p my-outstanding-tasks-cache-file)
    (let* ((data (with-temp-buffer
                   (insert-file-contents my-outstanding-tasks-cache-file)
                   (read (buffer-string))))
           (saved-date  (plist-get data :date))
           (saved-pairs (plist-get data :tasks))
           (today (format-time-string "%Y-%m-%d")))
      (when (string= saved-date today)
        (let ((seen-ids (make-hash-table :test 'equal))
              (result-list nil)
              (files-to-reindex nil)
              (resolved 0))
          ;; First pass: resolve what we can; collect files to reindex for the rest
          (dolist (task-pair saved-pairs)
            (let* ((stored-path (car task-pair))
                   (id (cdr task-pair)))
              (when (and id (not (gethash id seen-ids)))
                (puthash id t seen-ids)
                (let* ((abs-path (if (or (file-name-absolute-p stored-path)
                                         (string-match-p "^[A-Za-z]:[/\\\\]" stored-path))
                                     stored-path
                                   (expand-file-name stored-path
                                                     (or org-queue-directory default-directory))))
                       (marker (and id (org-id-find id 'marker))))
                  (if (and marker (markerp marker) (marker-buffer marker)
                           (buffer-live-p (marker-buffer marker)))
                      (with-current-buffer (marker-buffer marker)
                        (save-excursion
                          (goto-char (marker-position marker))
                          (let* ((priority (my-get-raw-priority-value))
                                 (flag (my-priority-flag priority))
                                 (file (buffer-file-name))
                                 (heading (org-get-heading t t t t))
                                 (pos (point))
                                 (srs (eq (org-srs-entry-p (point)) 'current))
                                 (task-plist (list :id id
                                                   :marker marker
                                                   :priority priority
                                                   :flag flag
                                                   :file file
                                                   :is-todo (my-is-todo-task)
                                                   :heading heading
                                                   :pos pos
                                                   :srs srs)))
                            (push task-plist result-list)
                            (setq resolved (1+ resolved)))))
                    (when (and abs-path (file-exists-p abs-path))
                      (push (file-truename abs-path) files-to-reindex)))))))

          ;; One-shot reindex (deduped) then retry unresolved IDs
          (setq files-to-reindex (delete-dups files-to-reindex))
          (when files-to-reindex
            (ignore-errors (org-id-update-id-locations files-to-reindex)))
          (dolist (task-pair saved-pairs)
            (let* ((stored-path (car task-pair))
                   (id (cdr task-pair)))
              (when (and id
                         (not (cl-find id result-list
                                       :key (lambda (e) (plist-get e :id))
                                       :test #'equal)))
                (let ((marker (org-id-find id 'marker)))
                  (when (and marker (markerp marker) (marker-buffer marker)
                             (buffer-live-p (marker-buffer marker)))
                    (with-current-buffer (marker-buffer marker)
                      (save-excursion
                        (goto-char (marker-position marker))
                        (let* ((priority (my-get-raw-priority-value))
                               (flag (my-priority-flag priority))
                               (file (buffer-file-name))
                               (heading (org-get-heading t t t t))
                               (pos (point))
                               (srs (eq (org-srs-entry-p (point)) 'current))
                               (task-plist (list :id id
                                                 :marker marker
                                                 :priority priority
                                                 :flag flag
                                                 :file file
                                                 :is-todo (my-is-todo-task)
                                                 :heading heading
                                                 :pos pos
                                                 :srs srs)))
                          (push task-plist result-list)
                          (setq resolved (1+ resolved)))))))))

          ;; Finalize
          (setq my-outstanding-tasks-list (nreverse result-list))
          (my-dedupe-outstanding-tasks)
          (message "org-queue: resolved %d IDs (from cache)" resolved)
          (my-save-outstanding-tasks-to-file)
          (unless (and (my-load-index-from-file)
                       (< my-outstanding-tasks-index (length my-outstanding-tasks-list)))
            (setq my-outstanding-tasks-index 0))
          t))))))

(defun my-ensure-task-list-present ()
  "Ensure the task list exists in memory without reordering it.
- If the in-memory list exists, do nothing.
- Else try to load today's cache (no rebuild).
- If cache is missing, build once (initialization fallback). Clamp index."
  (unless my-outstanding-tasks-list
    (unless (my-load-outstanding-tasks-from-file)
      (my-get-outstanding-tasks)
      (setq my-outstanding-tasks-index 0)
      (my-save-outstanding-tasks-to-file)))
  (when (or (not (numberp my-outstanding-tasks-index))
            (< my-outstanding-tasks-index 0)
            (>= my-outstanding-tasks-index (length my-outstanding-tasks-list)))
    (setq my-outstanding-tasks-index 0)))
  
(defun my-ensure-synchronized-task-list ()
  "Ensure we have a current, synchronized task list and valid index."
  ;; Try to load from cache first
  (if (my-load-outstanding-tasks-from-file)
      (progn
        (my-dedupe-outstanding-tasks)
        (my-save-outstanding-tasks-to-file))
    ;; If cache is stale or missing, regenerate
    (my-get-outstanding-tasks)
    (setq my-outstanding-tasks-index 0)
    (my-save-outstanding-tasks-to-file))
  ;; Double-check index validity after loading
  (when (or (>= my-outstanding-tasks-index (length my-outstanding-tasks-list))
            (< my-outstanding-tasks-index 0))
    (setq my-outstanding-tasks-index 0)
    (my-save-index-to-file))
  ;; Limit visible buffers to a small, focused set
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

(defun my-get-outstanding-tasks ()
  "Build two queues:
- my-outstanding-tasks-list: items available now (due and not suppressed),
- my-today-pending-tasks: today’s items not yet available (time-gated SRS or snoozed/time-gated non-SRS).
Then interleave outstanding SRS with non-SRS by `org-queue-srs-mix-ratio` (SRS suppressed at night)."
  (let* ((now (org-queue--now))
         (today (format-time-string "%Y-%m-%d" now))
         (night (org-queue-night-shift-p))
         (non-srs-out '())
         (non-srs-pending '()))
    ;; Collect non-SRS candidates scheduled today or earlier (or QFORCE),
    ;; exclude DONE and any SRS entries.
    (org-queue-map-entries
     (lambda ()
       (when (and (not (my-is-done-task))
                  (not (org-srs-entry-p (point))))
         (let* ((scheduled (org-get-scheduled-time nil))
                (qforce (org-entry-get nil org-queue-force-outstanding-property))
                ;; include if QFORCE, or scheduled date <= today
                (eligible (or qforce
                              (and scheduled
                                   (<= (time-to-days scheduled)
                                       (time-to-days now))))))
           (when eligible
             (let* ((marker   (point-marker))
                    (id       (or (org-entry-get nil "ID") (org-id-get-create)))
                    (priority (my-get-raw-priority-value))
                    (flag     (my-priority-flag priority))
                    (file     (buffer-file-name))
                    (heading  (org-get-heading t t t t))
                    (is-todo  (my-is-todo-task))
                    (pos      (point))
                    (task     (list :id id :marker marker :priority priority
                                    :flag flag :file file :is-todo is-todo
                                    :heading heading :pos pos)))
               ;; Compute and cache availability
               (setq task (org-queue--update-available-at! task))
               (let ((avail (plist-get task :available-at)))
                 (when avail
                   (if (not (time-less-p now avail))  ;; avail <= now => outstanding
                       (push task non-srs-out)
                     ;; avail > now => pending only if it belongs to today (SCHEDULED today or QFORCE)
                     (when (org-queue--task-is-today-p task)
                       (push task non-srs-pending))))))))))
     nil)

    ;; Sort non-SRS outstanding: TODO-first, priority asc, LAST_REPEAT asc (older first).
    (setq non-srs-out
          (cl-stable-sort non-srs-out
                          (lambda (a b)
                            (let* ((ka (org-queue--non-srs-sort-key a))
                                   (kb (org-queue--non-srs-sort-key b)))
                              (or (< (nth 0 ka) (nth 0 kb))
                                  (and (= (nth 0 ka) (nth 0 kb))
                                       (or (< (nth 1 ka) (nth 1 kb))
                                           (and (= (nth 1 ka) (nth 1 kb))
                                                (< (nth 2 ka) (nth 2 kb))))))))))

    ;; Collect SRS: due-now and pending-later-today
    (let* ((pair (org-queue-collect-srs-today))
           (srs-due-now (car pair))
           (srs-pending (cdr pair))
           ;; Interleave outstanding by ratio; suppress SRS during night shift
           (ratio (if night '(1 . 0)
                    (or (and (boundp 'org-queue-srs-mix-ratio)
                             org-queue-srs-mix-ratio)
                        '(1 . 4))))
           (a-count (car ratio))
           (b-count (cdr ratio))
           (start (org-queue--decide-mix-start a-count b-count non-srs-out srs-due-now))
           (mixed (org-queue--interleave-by-ratio-start
                   (copy-sequence non-srs-out)
                   (copy-sequence srs-due-now)
                   a-count b-count start)))
      ;; Assign outstanding
      (setq my-outstanding-tasks-list mixed)

      ;; Build pending-today = non-SRS pending ∪ SRS pending (dedup by task key)
      (let* ((pending (append (nreverse non-srs-pending) (nreverse srs-pending)))
            (seen (make-hash-table :test 'equal))
            (acc '()))
        (dolist (task pending)
          (let ((k (my--task-unique-key task)))
            (unless (and k (gethash k seen))
              (when k (puthash k t seen))
              ;; Ensure :available-at is present for pending entries
              (push (or (and (plist-get task :available-at) task)
                        (org-queue--update-available-at! task))
                    acc))))
        (setq my-today-pending-tasks (nreverse acc))))

    ;; Dedupe outstanding list and finalize
    (my-dedupe-outstanding-tasks)
    (let ((todo-count (length (cl-remove-if-not (lambda (task) (plist-get task :is-todo))
                                                my-outstanding-tasks-list)))
          (total-count (length my-outstanding-tasks-list))
          (pending-count (length my-today-pending-tasks)))
      (message (if (org-queue-night-shift-p)
                   "=== QUEUE BUILT (two-queue; SRS suppressed for night shift) ==="
                 "=== QUEUE BUILT (two-queue; SRS integrated) ==="))
      (message "Outstanding: %d (TODO: %d) | Pending today: %d"
               total-count todo-count pending-count))
    ;; Start from top
    (setq my-outstanding-tasks-index 0)
    (my-queue-limit-visible-buffers)
    ;; Flush any eager ID creations done during this scan, unless we're batching.
    (unless org-queue--suppress-save
      (save-some-buffers t))))

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
      (my-save-outstanding-tasks-to-file)
      (message "Task removed. Queue is now empty.")
      ;; Optionally rebuild right away
      (my-get-outstanding-tasks)
      (setq my-outstanding-tasks-index 0)
      (my-save-outstanding-tasks-to-file))

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
      (my-save-outstanding-tasks-to-file)
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
  (my-save-outstanding-tasks-to-file)  ;; This will save both tasks and index
  (message "Outstanding tasks index reset."))

(defun my-reset-and-show-current-outstanding-task ()
  "Reset the outstanding tasks index and then show the top outstanding task."
  (interactive)
  (my-launch-anki)
  (my-reset-outstanding-tasks-index)
  (org-queue-show-top))

(defun org-queue-hard-refresh ()
  "Force reindex + rebuild the queue, refresh chooser if visible, and show current task."
  (interactive)
  (my-launch-anki)
  (org-queue--with-batched-saves
    (org-queue-reindex-files)           ;; optional: log size
    (my-get-outstanding-tasks)          ;; rebuild from files (may create IDs)
    (my-save-outstanding-tasks-to-file) ;; update cache
    )
  ;; refresh chooser buffers if open
  (dolist (b (list "*Org Queue*" "*Org Queue (Subset)*"))
    (when (get-buffer b)
      (with-current-buffer b
        (when (derived-mode-p 'org-queue-chooser-mode)
          (ignore-errors (org-queue-chooser-refresh))))))
  ;; show current task (no SRS restart)
  (org-queue-show-top t)
  (message "org-queue: hard refresh complete (%d task(s))"
           (length my-outstanding-tasks-list)))

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
        (my-queue-limit-visible-buffers)
        (my-show-current-flag-status))
    (message "No outstanding tasks found.")))

(defun org-queue-show-top (&optional pulse)
  "Show the top (index 0) outstanding task.
Prunes non-due items at the top repeatedly until a due item is found or the list is empty.
Does not rescan files; does not auto-start SRS/Anki."
  (interactive)
  (my-ensure-task-list-present)
  (if (or (null my-outstanding-tasks-list)
          (zerop (length my-outstanding-tasks-list)))
      (message "No outstanding tasks found.")
    (progn
      (setq my-outstanding-tasks-index 0)
      ;; Prune while avoiding per-item saves
      (let ((pruned 0))
        (while (and my-outstanding-tasks-list
                    (>= my-outstanding-tasks-index 0)
                    (< my-outstanding-tasks-index (length my-outstanding-tasks-list))
                    (not (my-queue--task-due-p (nth 0 my-outstanding-tasks-list))))
          (my-queue--remove-index 0 t t) ;; quiet + no-save
          (setq my-outstanding-tasks-index 0)
          (setq pruned (1+ pruned)))
        ;; Save once if we pruned anything
        (when (> pruned 0)
          (my-save-outstanding-tasks-to-file)))
      (if (or (null my-outstanding-tasks-list)
              (zerop (length my-outstanding-tasks-list)))
          (message "No outstanding tasks found.")
        (progn
          ;; Save index and display (no SRS auto-start)
          (my-save-index-to-file)
          (my-show-current-outstanding-task-no-srs (or pulse t)))))))

(defvar org-queue--midnight-timer nil
  "Timer that triggers a daily midnight refresh of org-queue.")

(defun org-queue--midnight-refresh ()
  "Clear today's pending, rebuild queues, save, and show top. Reschedule for next midnight."
  (setq my-today-pending-tasks nil)
  (my-get-outstanding-tasks)
  (my-save-outstanding-tasks-to-file)
  (ignore-errors (org-queue-show-top t))
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

  ;; Try cache first (today)
  (let ((cache-loaded (and (fboundp 'my-load-outstanding-tasks-from-file)
                          (my-load-outstanding-tasks-from-file))))
    (if cache-loaded
        (message "org-queue: cache loaded for today")
      (progn
        (message "org-queue: cache missing/stale -> maintenance")
        (org-queue--with-batched-saves
          (my-get-outstanding-tasks)
          (setq my-outstanding-tasks-index 0)
          (my-save-outstanding-tasks-to-file))
        (message "org-queue: queue built and saved"))))

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
                            (message "Error preparing task display: %s" (error-message-string err)))))))

;; Automatic startup
(add-hook 'emacs-startup-hook #'org-queue-startup 100)

(defun org-queue-maintenance ()
  "Run org-queue maintenance.
Runs the complete maintenance pipeline and flushes a single save at the end."
  (interactive)
  (my-org-id-initialize-id-locations)

  ;; Suppress UI refresh during the heavy run; batch-suppress saves and flush once at the end.
  (let ((org-queue--suppress-ui t))
    (org-queue--with-batched-saves
      (progn
        ;; Optional: tighten org-roam/IDs if available
        (when (require 'org-roam nil t)
          (let ((warning-minimum-level :error))
            (org-roam-db-autosync-mode 1)
            (ignore-errors (org-id-update-id-locations (org-queue-file-list)))))

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

      ;; Rebuild/refresh lists and caches (cache files are written as usual)
      (my-get-outstanding-tasks)
      (setq my-outstanding-tasks-index 0)
      (my-save-outstanding-tasks-to-file)))

  ;; After the macro exits, a single save-some-buffers t has been executed.
  (delete-other-windows)
  (org-queue-show-top t)
  (message "org-queue: maintenance complete.")
  (ignore-errors (org-queue--schedule-midnight-refresh)))

(provide 'org-queue-tasks)
;;; org-queue-tasks.el ends here
