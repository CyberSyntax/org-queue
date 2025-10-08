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
  "Return non-nil if TASK is due now.
- Non-SRS: outstanding (scheduled today or earlier, or QFORCE).
- SRS: has next-due time <= now (suppressed during night shift)."
  (let ((m (my-extract-marker task)))
    (when (and (markerp m) (marker-buffer m) (buffer-live-p (marker-buffer m)))
      (org-with-point-at m
        (cond
         ;; SRS: only due if not night shift and next-due <= now
         ((plist-get task :srs)
          (and (not (org-queue-night-shift-p))
               (let ((due (org-queue-srs-next-due-time (point))))
                 (and due (not (time-less-p (current-time) due))))))
         ;; Non-SRS: outstanding?
         (t (my-is-outstanding-task)))))))

(defun my-queue--refresh-chooser-buffers ()
  (dolist (b (list "*Org Queue*" "*Org Queue (Subset)*"))
    (when (get-buffer b)
      (with-current-buffer b
        (when (derived-mode-p 'org-queue-chooser-mode)
          (when (fboundp 'org-queue-chooser-refresh)
            (ignore-errors (org-queue-chooser-refresh))))))))

(defun my-queue--remove-index (idx &optional quiet)
  "Remove entry at IDX from the queue; do not display anything."
  (when (and my-outstanding-tasks-list
             (numberp idx)
             (>= idx 0)
             (< idx (length my-outstanding-tasks-list)))
    (let* ((removed (nth idx my-outstanding-tasks-list))
           (buf (my-safe-marker-buffer removed)))
      (setq my-outstanding-tasks-list
            (append (seq-take my-outstanding-tasks-list idx)
                    (seq-drop  my-outstanding-tasks-list (1+ idx))))
      ;; Clamp index
      (when (>= my-outstanding-tasks-index (length my-outstanding-tasks-list))
        (setq my-outstanding-tasks-index (max 0 (1- (length my-outstanding-tasks-list)))))
      ;; Tidy buffers, save, refresh chooser
      (when (buffer-live-p buf) (bury-buffer buf))
      (my-queue-limit-visible-buffers)
      (my-save-outstanding-tasks-to-file)
      (unless quiet
        (message "Removed 1 item from queue; now %d total."
                 (length my-outstanding-tasks-list)))
      (my-queue--refresh-chooser-buffers)
      t)))

(defun my-queue--prune-current-if-not-due (&optional quiet)
  "If the current queue item isn’t due, remove it. Return t if pruned."
  (when (and my-outstanding-tasks-list
             (>= my-outstanding-tasks-index 0)
             (< my-outstanding-tasks-index (length my-outstanding-tasks-list)))
    (let ((task (nth my-outstanding-tasks-index my-outstanding-tasks-list)))
      (unless (my-queue--task-due-p task)
        (my-queue--remove-index my-outstanding-tasks-index quiet)))))

(defun my-queue--skip-not-due-forward (start-idx &optional quiet)
  "From START-IDX, remove not-due items at that index until a due item is found or list empty.
Return final index (or 0 if empty)."
  (let ((idx start-idx))
    (while (and my-outstanding-tasks-list
                (>= idx 0)
                (< idx (length my-outstanding-tasks-list))
                (not (my-queue--task-due-p (nth idx my-outstanding-tasks-list))))
      ;; Remove the candidate at IDX; after removal the next candidate slides into same IDX
      (my-queue--remove-index idx t))
    (if (zerop (length my-outstanding-tasks-list)) 0
      (min idx (1- (length my-outstanding-tasks-list))))))

(defun my-queue--skip-not-due-backward (start-idx &optional quiet)
  "From START-IDX, remove not-due items at that index (backward semantics).
We still remove at START-IDX and keep idx = min(idx, last) after each removal.
Return final index (or 0 if empty)."
  (let ((idx start-idx))
    (while (and my-outstanding-tasks-list
                (>= idx 0)
                (< idx (length my-outstanding-tasks-list))
                (not (my-queue--task-due-p (nth idx my-outstanding-tasks-list))))
      (my-queue--remove-index idx t)
      (setq idx (min idx (max 0 (1- (length my-outstanding-tasks-list))))))
    (if (zerop (length my-outstanding-tasks-list)) 0
      (min idx (1- (length my-outstanding-tasks-list))))))

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

(defcustom org-queue-file-roster-ttl 20
  "Seconds to cache the list of Org files for soft rebuild."
  :type 'integer :group 'org-queue)

(defvar org-queue--file-roster-cache nil)
(defvar org-queue--file-roster-ts 0)

(defun org-queue-file-list-cached (&optional force)
  "Return cached file roster or reindex when stale or FORCE."
  (let* ((now (float-time))
         (stale (> (- now org-queue--file-roster-ts) org-queue-file-roster-ttl)))
    (when (or force (null org-queue--file-roster-cache) stale)
      (setq org-queue--file-roster-cache (org-queue-reindex-files t))
      (setq org-queue--file-roster-ts now))
    org-queue--file-roster-cache))

(defun org-queue-rebuild-soft ()
  "Rebuild queue (priority sort + SRS mixing) without reindexing disk.
Uses cached file roster (see `org-queue-file-roster-ttl')."
  (interactive)
  (let ((files (org-queue-file-list-cached)))
    (cl-letf (((symbol-function 'org-queue-file-list)
               (lambda () files)))
      (my-get-outstanding-tasks)  ;; does priority sort and SRS mixing
      (setq my-outstanding-tasks-index
            (min my-outstanding-tasks-index
                 (max 0 (1- (length my-outstanding-tasks-list)))))
      (my-save-outstanding-tasks-to-file)
      (my-queue--refresh-chooser-buffers)
      (my-show-current-outstanding-task-no-srs t)
      (message "org-queue: soft rebuild (no reindex): %d task(s)"
               (length my-outstanding-tasks-list)))))

(defun org-queue-prune-queue ()
  "Prune not-due items from the in-memory queue; keep order; no resort."
  (interactive)
  (let ((removed (or (my-queue-filter-not-due-in-place) 0)))
    (my-show-current-outstanding-task-no-srs t)
    (message "Pruned %d; %d remain"
             removed (length my-outstanding-tasks-list))))

;; Backward-compat alias (safe to keep; remove later if you wish)
(defalias 'org-queue-fast-refresh 'org-queue-prune-queue)

;; Variables for task list management
(defvar my-outstanding-tasks-list nil
  "List of outstanding tasks, sorted by priority.")

(defvar my-outstanding-tasks-index 0
  "Current index in the outstanding tasks list.")

;; Define a customizable variable for the cache file.
(defcustom my-outstanding-tasks-cache-file
  (expand-file-name "org-queue-outstanding-tasks.cache" cache-dir)
  "File path to store the cached outstanding tasks list along with its date stamp.
                  By default, this file will be inside the cache directory (cache-dir)."
  :type 'string
  :group 'org-queue)

(defcustom my-outstanding-tasks-index-file
  (expand-file-name "org-queue-index.cache" cache-dir)
  "File path to store the current task index."
  :type 'string
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
  (cl-block my-extract-marker
    (cond
     ;; Given a marker directly
     ((markerp task-or-marker)
      (when (and (marker-buffer task-or-marker)
                 (buffer-live-p (marker-buffer task-or-marker)))
        (cl-return-from my-extract-marker task-or-marker)))
     ;; Given a plist task
     ((listp task-or-marker)
      (let* ((m (plist-get task-or-marker :marker))
             (id (plist-get task-or-marker :id))
             (file (plist-get task-or-marker :file))
             (heading (plist-get task-or-marker :heading))
             (pos0 (plist-get task-or-marker :pos)))
        ;; Prefer existing live marker and sync
        (when (and (markerp m)
                   (marker-buffer m)
                   (buffer-live-p (marker-buffer m)))
          (cl-return-from my-extract-marker
            (my--task-sync-metadata task-or-marker m)))
        ;; Resolve by ID inside the recorded file (handles duplicates deterministically)
        (when (and id file)
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
            (when (and (markerp chosen)
                       (marker-buffer chosen)
                       (buffer-live-p (marker-buffer chosen)))
              (cl-return-from my-extract-marker
                (my--task-sync-metadata task-or-marker chosen)))))
        ;; Fallback to org-id DB if file scan failed
        (when (and id (fboundp 'org-id-find))
          (let ((mk (org-id-find id 'marker)))
            (when (and (markerp mk)
                       (marker-buffer mk)
                       (buffer-live-p (marker-buffer mk)))
              (cl-return-from my-extract-marker
                (my--task-sync-metadata task-or-marker mk)))))
        ;; Fallback to file+pos if available
        (when (and file (file-exists-p file) (numberp pos0))
          (with-current-buffer (find-file-noselect file)
            (save-restriction
              (widen)
              (save-excursion
                (goto-char (min (max pos0 (point-min)) (point-max)))
                (org-back-to-heading t)
                (cl-return-from my-extract-marker
                  (my--task-sync-metadata task-or-marker (point-marker)))))))))
     (t nil))))

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
Dedupes by ID while reading; dedupes result list; rewrites cache deduped."
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
              (resolved 0)
              (unresolved 0))
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
                  (unless (and marker (markerp marker)
                               (marker-buffer marker)
                               (buffer-live-p (marker-buffer marker)))
                    (when (and id abs-path (file-exists-p abs-path))
                      (ignore-errors
                        (org-id-update-id-locations (list (file-truename abs-path))))
                      (setq marker (org-id-find id 'marker))))
                  (if (and marker
                           (markerp marker)
                           (marker-buffer marker)
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
                    (setq unresolved (1+ unresolved))
                    (message "org-queue: could not resolve ID %s (stored path: %s)" id stored-path))))))

          (setq my-outstanding-tasks-list (nreverse result-list))
          (my-dedupe-outstanding-tasks)
          (message "org-queue: resolved %d IDs, %d unresolved (from cache)" resolved unresolved)
          (my-save-outstanding-tasks-to-file)

          (unless (and (my-load-index-from-file)
                       (< my-outstanding-tasks-index (length my-outstanding-tasks-list)))
            (setq my-outstanding-tasks-index 0))
          t)))))

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

(defun my-get-outstanding-tasks ()
  "Populate task list with metadata and integrate due SRS items by ratio."
  (let ((non-srs '()))
    ;; Collect non-SRS outstanding tasks (original behavior with SRS excluded)
    (setq non-srs nil)
    (org-queue-map-entries
     (lambda ()
       (when (and (my-is-outstanding-task)
                  (not (my-is-done-task))
                  (not (org-srs-entry-p (point))))
         (let* ((marker (point-marker))
                (id (or (org-entry-get nil "ID") (org-id-get-create)))
                (priority (my-get-raw-priority-value))
                (flag (my-priority-flag priority))
                (file (buffer-file-name))
                (heading (org-get-heading t t t t))
                (todo-state (org-get-todo-state))
                (is-todo (my-is-todo-task))
                (pos (point))
                (task-data (list :id id
                                 :marker marker
                                 :priority priority
                                 :flag flag
                                 :file file
                                 :is-todo is-todo
                                 :heading heading
                                 :pos pos))
                (sort-key (list (if is-todo 0 1) priority)))
           (push (cons sort-key task-data) non-srs))))
     nil)
    ;; Stable sort non-SRS by TODO-ness, then priority, tie by ID
    (setq non-srs
          (mapcar #'cdr
                  (cl-stable-sort
                   non-srs
                   (lambda (a b)
                     (let* ((ka (car a)) (kb (car b))
                            (todoa (car ka)) (todob (car kb))
                            (pa (cadr ka))   (pb (cadr kb)))
                       (cond
                        ((/= todoa todob) (< todoa todob))
                        ((/= pa pb)       (< pa pb))
                        (t (string< (or (plist-get (cdr a) :id) "")
                                    (or (plist-get (cdr b) :id) "")))))))))
    ;; Collect SRS due items now
    (let* ((night (org-queue-night-shift-p))
          (srs (unless night (org-queue-collect-srs-due-items)))
          (ratio (if night
                      '(1 . 0)  ;; suppress SRS during night shift
                    (or (and (boundp 'org-queue-srs-mix-ratio)
                            org-queue-srs-mix-ratio)
                        '(1 . 4))))
          (a-count (car ratio))
          (b-count (cdr ratio))
          (mixed (org-queue--interleave-by-ratio (copy-sequence non-srs)
                                                  (copy-sequence srs)
                                                  a-count b-count)))
      (setq my-outstanding-tasks-list mixed))
    (my-dedupe-outstanding-tasks)
    (let ((todo-count (length (cl-remove-if-not (lambda (task) (plist-get task :is-todo))
                                                my-outstanding-tasks-list)))
          (total-count (length my-outstanding-tasks-list)))
      (message (if (org-queue-night-shift-p)
                  "=== QUEUE BUILT (SRS suppressed for night shift) ==="
                "=== QUEUE BUILT (SRS integrated) ==="))
      (message "Total tasks in queue: %d (TODO: %d)" total-count todo-count))
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
      (my-show-current-outstanding-task)))))

(defun my-move-current-task-to-position (target-pos)
  "Move current task to TARGET-POS in the queue (1-based).
Does not attempt to globally reorder buffers. Instead, limits visible
queue buffers around the new position."
  (interactive "nMove to queue position (1-based): ")
  
  ;; Validate
  (unless (and my-outstanding-tasks-list
               (>= my-outstanding-tasks-index 0)
               (< my-outstanding-tasks-index (length my-outstanding-tasks-list)))
    (error "No valid current task"))
  
  (let* ((target-index (1- target-pos))  ; Convert to 0-based
         (max-index (1- (length my-outstanding-tasks-list)))
         (clamped-index (max 0 (min target-index max-index)))
         (current-task (nth my-outstanding-tasks-index my-outstanding-tasks-list)))
    
    ;; Remove from current position
    (setq my-outstanding-tasks-list
          (append (seq-take my-outstanding-tasks-list my-outstanding-tasks-index)
                  (seq-drop my-outstanding-tasks-list (1+ my-outstanding-tasks-index))))
    
    ;; Insert at new position
    (setq my-outstanding-tasks-list
          (append (seq-take my-outstanding-tasks-list clamped-index)
                  (list current-task)
                  (seq-drop my-outstanding-tasks-list clamped-index)))
    
    ;; Update index to new position
    (setq my-outstanding-tasks-index clamped-index)
    
    ;; Limit visible queue buffers around the new index
    (my-queue-limit-visible-buffers)
    
    ;; Save state
    (my-save-outstanding-tasks-to-file)
    
    (message "Moved to position %d/%d"
             (1+ clamped-index)
             (length my-outstanding-tasks-list))
    (my-show-current-outstanding-task)))

;; Task management functions
(defun my-auto-postpone-overdue-tasks ()
  "Auto-postpone all overdue TODO tasks (excluding DONE tasks)."
  (interactive)
  (save-some-buffers t)
  
  (let ((processed-count 0)
        (total-overdue 0))
    
    ;; First pass: count total overdue TODO tasks
    (org-queue-map-entries
     (lambda ()
       (when (and (my-is-overdue-task) (my-is-todo-task))  ; Add TODO check
         (setq total-overdue (1+ total-overdue))))
     nil)
    
    ;; Second pass: process overdue TODO tasks
    (org-queue-map-entries
     (lambda ()
       (when (and (my-is-overdue-task) (my-is-todo-task))  ; Add TODO check
         (my-ensure-priority-set)
         (my-postpone-schedule)
         (setq processed-count (1+ processed-count))
         
         (when (= (mod processed-count 10) 0)
           (message "Processed %d/%d overdue TODO tasks..." processed-count total-overdue))))
     nil)
    
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
               nil 'file))))))
    (save-some-buffers t)
    (message "Processed duplicate outstanding priorities.")))

(defun my-enforce-priority-constraints ()
  "Enforce monotone cap: count of lower priorities must not exceed any higher priority count.
Process priorities from 1 to 64; postpone overflow FIFO within each priority."
  (interactive)
  (let* ((files (org-queue-file-list))
         (priority-counts (make-hash-table :test 'equal))
         (tasks-by-priority (make-hash-table :test 'equal)))
    (unless files
      (user-error "No Org files for org-queue. Configure `org-queue-directory` or `org-queue-file-roots`."))
    ;; Collect
    (dolist (file files)
      (when (file-exists-p file)
        (with-current-buffer (find-file-noselect file)
          (save-excursion
            (org-map-entries
             (lambda ()
               (when (my-is-outstanding-task)
                 (let* ((pstr (org-entry-get nil "PRIORITY"))
                        (prio (if pstr (string-to-number pstr) org-priority-default))
                        (entry (cons (buffer-file-name) (point))))
                   (puthash prio
                            (nconc (gethash prio tasks-by-priority) (list entry))
                            tasks-by-priority)
                   (puthash prio
                            (1+ (gethash prio priority-counts 0))
                            priority-counts))))
             nil 'file)))))

    (let* ((sorted (sort (hash-table-keys priority-counts) #'<))
           (current-max nil))
      (if (zerop (length sorted))
          (message "[COMPLETE] No outstanding tasks found")
        (dolist (prio sorted)
          (let ((count (gethash prio priority-counts 0))
                (fifo  (copy-sequence (gethash prio tasks-by-priority '()))))
            (cond
             ((zerop count) nil)
             ((null current-max)
              (setq current-max count)
              (message "[CONSTRAINT] Pri %d sets cap to %d" prio current-max))
             ((> count current-max)
              (let ((excess (- count current-max)))
                (message "[ENFORCE] Pri %d overflow (%d > %d). Postponing %d..."
                         prio count current-max excess)
                (dotimes (_ excess)
                  (when-let ((entry (pop fifo)))
                    (let ((file (car entry)) (pos (cdr entry)))
                      (with-current-buffer (find-file-noselect file)
                        (goto-char pos)
                        (my-postpone-schedule))))))))
            (puthash prio fifo tasks-by-priority))
          (setq current-max (min current-max (gethash prio priority-counts 0)))))
      (save-some-buffers t)
      (message "[COMPLETE] Constraint enforcement done; final cap %s"
               (or current-max 0)))))

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
        ;; Extract the real marker from the plist
        (let* ((marker (my-extract-marker entry))
               (buffer (and (markerp marker)
                            (marker-buffer marker)))
               (file   (and buffer
                            (buffer-file-name buffer))))
          (when buffer
            (if (and file (equal file prev-file))
                (progn
                  ;; postpone it
                  (with-current-buffer buffer
                    (goto-char (marker-position marker))
                    (my-postpone-schedule))
                  (push buffer modified-buffers)
                  (message "Postponed duplicate in: %s"
                           (file-name-nondirectory file)))
              (setq prev-file file)))))
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
  "Reset the outstanding tasks index and then show the current outstanding task."
  (interactive)  ;; Allows the function to be executed via M-x in Emacs
  (my-reset-outstanding-tasks-index)  ;; Call function to reset tasks index
  (my-show-current-outstanding-task))  ;; Call function to show the first/current task

(defun org-queue-hard-refresh ()
  "Force reindex + rebuild the queue, refresh chooser if visible, and show current task."
  (interactive)
  (org-queue-reindex-files)             ;; optional: log size
  (my-get-outstanding-tasks)            ;; rebuild from files
  (my-save-outstanding-tasks-to-file)   ;; update cache
  ;; refresh chooser buffers if open
  (dolist (b (list "*Org Queue*" "*Org Queue (Subset)*"))
    (when (get-buffer b)
      (with-current-buffer b
        (when (derived-mode-p 'org-queue-chooser-mode)
          (ignore-errors (org-queue-chooser-refresh))))))
  ;; show current task (no SRS restart)
  (my-show-current-outstanding-task-no-srs t)
  (message "org-queue: hard refresh complete (%d task(s))"
           (length my-outstanding-tasks-list)))

(defvar my-queue--orchestrating nil
  "Prevent re-entrancy while orchestrating SRS/Anki.")

(defun my-queue-handle-srs-after-task-display (&optional task)
  "Launch/focus Anki only when appropriate.
Rules:
- Never during night shift.
- Only for non-SRS items (TASK lacks :srs)."
  (unless my-queue--orchestrating
    (let ((my-queue--orchestrating t))
      (condition-case err
          (let ((night (org-queue-night-shift-p))
                (is-srs (and (listp task) (plist-get task :srs))))
            (when (and (not night) (not is-srs))
              (my-launch-anki)))
        (error
         (message "org-queue: SRS/Anki orchestration failed: %s"
                  (error-message-string err)))))))

(defun my-show-next-outstanding-task ()
  "Show the next outstanding task using the in-memory queue only."
  (interactive)
  (widen-and-recenter)
  (my-ensure-task-list-present)

  ;; Prune the item you’re leaving if it’s no longer due (SRS future, scheduled future, or night shift SRS).
  (while (my-queue--prune-current-if-not-due t))

  (if (and my-outstanding-tasks-list
           (> (length my-outstanding-tasks-list) 0))
      (let* ((len (length my-outstanding-tasks-list))
             (candidate (mod (1+ my-outstanding-tasks-index) len)))
        ;; Skip any not-due items we would land on (remove them as we go).
        (setq candidate (my-queue--skip-not-due-forward candidate t))
        (setq my-outstanding-tasks-index candidate)
        (my-save-index-to-file)
        (let ((task (nth candidate my-outstanding-tasks-list)))
          (my-display-task-at-marker task)
          (my-pulse-highlight-current-line)
          (my-queue-limit-visible-buffers)
          (my-queue-handle-srs-after-task-display task)
          (my-show-current-flag-status)))
    (message "No outstanding tasks found.")))

(defun my-show-previous-outstanding-task ()
  "Show the previous outstanding task using the in-memory queue only."
  (interactive)
  (widen-and-recenter)
  (my-ensure-task-list-present)

  ;; Prune the item you’re leaving if it’s no longer due
  (while (my-queue--prune-current-if-not-due t))

  (if (and my-outstanding-tasks-list
           (> (length my-outstanding-tasks-list) 0))
      (let* ((len (length my-outstanding-tasks-list))
             (candidate (mod (1- my-outstanding-tasks-index) len)))
        ;; Skip any not-due items we would land on (remove them)
        (setq candidate (my-queue--skip-not-due-backward candidate t))
        (setq my-outstanding-tasks-index candidate)
        (my-save-index-to-file)
        (let ((task (nth candidate my-outstanding-tasks-list)))
          (my-display-task-at-marker task)
          (my-pulse-highlight-current-line)
          (my-queue-limit-visible-buffers)
          (my-show-current-flag-status)))
    (message "No outstanding tasks to navigate.")))

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

(defun my-show-current-outstanding-task ()
  "Show the current outstanding task from the in-memory queue."
  (interactive)
  (widen-and-recenter)
  (my-ensure-task-list-present)

  ;; If current is not due (e.g., SRS reviewed already, or night shift started), prune it first.
  (while (my-queue--prune-current-if-not-due t))

  (if (and my-outstanding-tasks-list
           (< my-outstanding-tasks-index (length my-outstanding-tasks-list)))
      (let ((task (nth my-outstanding-tasks-index my-outstanding-tasks-list)))
        (my-display-task-at-marker task)
        (my-pulse-highlight-current-line)
        (my-queue-limit-visible-buffers)
        (my-queue-handle-srs-after-task-display task)
        (my-show-current-flag-status))
    (message "No outstanding tasks found.")))

(defun my-show-next-outstanding-task-in-new-tab ()
  "Open a new tab and show the next outstanding task."
  (interactive)
  (if (fboundp 'tab-new)
      (progn
        (tab-new)
        (my-show-next-outstanding-task))
    (message "tab-new not available in this Emacs version")
    (my-show-next-outstanding-task)))

(defun my-show-previous-outstanding-task-in-new-tab ()
  "Open a new tab and show the previous outstanding task."
  (interactive)
  (if (fboundp 'tab-new)
      (progn
        (tab-new)
        (my-show-previous-outstanding-task))
    (message "tab-new not available in this Emacs version")
    (my-show-previous-outstanding-task)))

(defun my-show-current-outstanding-task-in-new-tab ()
  "Open a new tab and show the current outstanding task."
  (interactive)
  (if (fboundp 'tab-new)
      (progn
        (tab-new)
        (my-show-current-outstanding-task))
    (message "tab-new not available in this Emacs version")
    (my-show-current-outstanding-task)))

;; Utility functions for task management
(defun org-queue-startup ()
  "Minimal org-queue startup:
- Initialize org-id DB,
- If cache exists for today, use it;
  otherwise, build and save the queue.
- Schedule initial task display (no automatic SRS/Anki here)."
  (interactive)
  ;; IDs first
  (my-org-id-initialize-id-locations)

  ;; Try cache first (today)
  (let ((cache-loaded (and (fboundp 'my-load-outstanding-tasks-from-file)
                           (my-load-outstanding-tasks-from-file))))
    (if cache-loaded
        (message "org-queue: cache loaded for today")
      (progn
        (message "org-queue: cache missing/stale -> minimal maintenance")
        ;; Build and save queue
        (my-get-outstanding-tasks)
        (setq my-outstanding-tasks-index 0)
        (my-save-outstanding-tasks-to-file)
        (message "org-queue: queue built and saved"))))

  ;; Fast prune current queue (removes SRS during night shift and any not-due items)
  (ignore-errors (my-queue-filter-not-due-in-place t))

  ;; Schedule task display (no SRS auto-start here)
  (run-with-idle-timer 1.5 nil
                       (lambda ()
                         (condition-case err
                             (progn
                               (delete-other-windows)
                               (my-show-current-outstanding-task)) ;; prune-aware
                           (error
                            (message "Error preparing task display: %s" (error-message-string err)))))))

;; Automatic startup
(add-hook 'emacs-startup-hook #'org-queue-startup 100)

(defun org-queue-maintenance (&optional full)
  "Run org-queue maintenance.
Without prefix: quick maintenance (fast).
With C-u prefix: full maintenance (the heavy pipeline you used to run at startup).
Usage
- Quick (fast): M-x org-queue-maintenance
- Full (heavy pipeline): C-u M-x org-queue-maintenance"
  (interactive "P")
  (my-org-id-initialize-id-locations)
  (save-some-buffers t)

  (if full
      (progn
        ;; Optional: tighten org-roam/IDs if available
        (when (require 'org-roam nil t)
          (let ((warning-minimum-level :error))
            (org-roam-db-autosync-mode 1)
            (ignore-errors (org-id-update-id-locations (org-queue-file-list)))))

        ;; Ensure base invariants
        (when (fboundp 'my-ensure-priorities-and-schedules-for-all-headings)
          (my-ensure-priorities-and-schedules-for-all-headings))

        ;; Advance near-future schedules so queue is meaningful today
        (when (fboundp 'my-auto-advance-schedules)
          (my-auto-advance-schedules 8))

        ;; Reduce noise
        (my-auto-postpone-overdue-tasks)
        (my-postpone-duplicate-priority-tasks)

        ;; Enforce “higher priority caps lower priority counts”
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
    ;; Quick maintenance (fast)
    (my-auto-postpone-overdue-tasks)
    (my-postpone-duplicate-priority-tasks))

  (save-some-buffers t)
  (my-get-outstanding-tasks)
  (setq my-outstanding-tasks-index 0)
  (my-save-outstanding-tasks-to-file)
  (delete-other-windows)
  (my-show-current-outstanding-task-no-srs t)
  (message "org-queue: %s maintenance complete."
           (if full "full" "quick")))

(provide 'org-queue-tasks)
;;; org-queue-tasks.el ends here
