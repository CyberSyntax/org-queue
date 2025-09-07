;;; org-queue-tasks.el --- Task management and navigation for org-queue -*- lexical-binding: t -*-

;;; Code:

(require 'cl-lib)
(require 'org-id)
(require 'org-queue-config)
(require 'org-queue-utils)
(require 'org-queue-srs-bridge)

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
  "Scan all agenda files once to build the org-id cache."
  (unless my-org-id-locations-initialized
    (setq my-org-id-locations-initialized t)
    (when (fboundp 'org-id-update-id-locations)
      (ignore-errors
        (org-id-update-id-locations (org-agenda-files))))))

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

;; Set up agenda sorting strategy
(setq org-agenda-sorting-strategy
	'((agenda priority-down category-down)
	  (todo priority-down)
	  (tags priority-down)
	  (search category-keep)))

;; Marker extraction and safety functions
(defun my-extract-marker (task-or-marker)
  "Extract a live marker from TASK-OR-MARKER.
Prefers resolving from :id (UUID) when present; falls back to :marker."
  (cond
   ((markerp task-or-marker)
    task-or-marker)
   ((and (listp task-or-marker) (plist-get task-or-marker :id))
    (or (my-marker-from-uuid (plist-get task-or-marker :id)
                             (plist-get task-or-marker :file))
        (plist-get task-or-marker :marker)))
   ((and (listp task-or-marker) (plist-get task-or-marker :marker))
    (plist-get task-or-marker :marker))
   (t nil)))

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
  "Return a live marker for the heading with UUID.
If not found and FILE is non-nil, refresh IDs only for FILE and retry.
Returns nil if still not found."
  (when (and (stringp uuid) (> (length uuid) 0))
    (or (org-id-find uuid 'marker)
        (when (and file (file-exists-p file))
          (ignore-errors
            (org-id-update-id-locations (list (file-truename file))))
          (org-id-find uuid 'marker)))))

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
                  (push (cons path id) pairs))))))))
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
          ;; Dedup while reading pairs
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
                  ;; Targeted refresh only if not found
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
                                 (task-plist (list :id id
                                                   :marker marker
                                                   :priority priority
                                                   :flag flag
                                                   :file file)))
                            (push task-plist result-list)
                            (setq resolved (1+ resolved)))))
                    (setq unresolved (1+ unresolved))
                    (message "org-queue: could not resolve ID %s (stored path: %s)" id stored-path))))))

          (setq my-outstanding-tasks-list (nreverse result-list))
          ;; Dedupe final list (belt-and-suspenders)
          (my-dedupe-outstanding-tasks)

	  (when org-queue-verbose
	    (message "org-queue: resolved %d IDs, %d unresolved (from cache)" resolved unresolved))
          ;; Fix cache on disk immediately (deduped)
          (my-save-outstanding-tasks-to-file)

          ;; Load index; fallback to 0
          (unless (and (my-load-index-from-file)
                       (< my-outstanding-tasks-index (length my-outstanding-tasks-list)))
            (setq my-outstanding-tasks-index 0))
          t)))))

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

(defun my-get-outstanding-tasks ()
  "Populate task list with metadata for stable tracking."
  (setq my-outstanding-tasks-list nil)
  (org-map-entries
   (lambda ()
     (when (and (my-is-outstanding-task)
                (not (my-is-done-task))
                (not (org-srs-entry-p (point))))
       (let* ((marker (point-marker))
              ;; Ensure ID exists so duplicates can be removed reliably
              (id (or (org-entry-get nil "ID") (org-id-get-create)))
              (priority (my-get-raw-priority-value))
              (flag (my-priority-flag priority))
              (file (buffer-file-name))
              (heading (org-get-heading t t t t))
              (todo-state (org-get-todo-state))
              (is-todo (my-is-todo-task))
              (task-data (list :id id
                               :marker marker
                               :priority priority
                               :flag flag
                               :file file
                               :is-todo is-todo)))
         (message "QUEUE BUILD: %s | TODO-state: %s | is-todo: %s | priority: %s"
                  heading todo-state is-todo priority)
         (let ((sort-key (list (if is-todo 0 1) priority)))
           (push (cons sort-key task-data) my-outstanding-tasks-list)))))
   nil 'agenda)
  ;; Sort by two-tier system: TODO status first, then priority
  (setq my-outstanding-tasks-list
        (mapcar #'cdr
                (sort my-outstanding-tasks-list
                      (lambda (a b)
                        (let ((key-a (car a))
                              (key-b (car b)))
                          (if (= (car key-a) (car key-b))
                              (< (cadr key-a) (cadr key-b))
                            (< (car key-a) (car key-b))))))))
  ;; Deduplicate (by :id, else file@pos)
  (my-dedupe-outstanding-tasks)
  ;; Summary
  (let ((todo-count (length (cl-remove-if-not (lambda (task) (plist-get task :is-todo))
                                              my-outstanding-tasks-list)))
        (total-count (length my-outstanding-tasks-list)))
    (message "=== QUEUE BUILT ===")
    (message "Total tasks in queue: %d" total-count)
    (message "TODO tasks in queue: %d" todo-count))
  (setq my-outstanding-tasks-index 0)
  (my-queue-limit-visible-buffers))

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
  ;; Ensure we have synchronized data
  (my-ensure-synchronized-task-list)

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
      (my-show-current-outstanding-task)
      ;; SRS handling
      (my-queue-handle-srs-after-task-display)))))

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
    (org-map-entries
     (lambda ()
       (when (and (my-is-overdue-task) (my-is-todo-task))  ; Add TODO check
         (setq total-overdue (1+ total-overdue))))
     nil 'agenda)
    
    ;; Second pass: process overdue TODO tasks
    (org-map-entries
     (lambda ()
       (when (and (my-is-overdue-task) (my-is-todo-task))  ; Add TODO check
         (my-ensure-priority-set)
         (my-postpone-schedule)
         (setq processed-count (1+ processed-count))
         
         (when (= (mod processed-count 10) 0)
           (message "Processed %d/%d overdue TODO tasks..." processed-count total-overdue))))
     nil 'agenda)
    
    (save-some-buffers t)
    (message "✓ Auto-postponed %d overdue TODO tasks." processed-count)))

(defun my-postpone-duplicate-priority-tasks ()
  "Postpone duplicate outstanding tasks within the same file that share the same priority.
For each file, only one task per priority level remains as outstanding. 
All other tasks with the same priority in the same file are postponed.
Tasks across different files or with different priorities within the same file are unaffected."
  (interactive)
  ;; Ensure we are operating on agenda files
  (let ((agenda-files (org-agenda-files)))
    (unless agenda-files
	(user-error "No agenda files found. Please set `org-agenda-files` accordingly."))
    (let ((seen-tasks (make-hash-table :test 'equal))) ; Hash table to track seen (file, priority) pairs
	;; Iterate over each agenda file
	(dolist (file agenda-files)
	  (with-current-buffer (find-file-noselect file)
	    (save-excursion
	      (goto-char (point-min))
	      ;; Iterate over all headings in the current file
	      (org-map-entries
	       (lambda ()
		 (when (my-is-outstanding-task)
		   (let* ((priority-string (org-entry-get nil "PRIORITY"))
			  (priority (if priority-string
					(string-to-number priority-string)
				      ;; If priority not set, use default
				      org-priority-default))
			  (key (cons (file-truename file) priority)))
		     (if (gethash key seen-tasks)
			 ;; Duplicate found, postpone it
			 (progn
			   (my-postpone-schedule)
			   (message "Postponed duplicate task with priority %d in file %s." priority file))
		       ;; First occurrence, mark as seen
		       (puthash key t seen-tasks))))))
	       nil 'file))))
    (message "Duplicate outstanding tasks have been processed.")))

(defun my-enforce-priority-constraints ()
  "Enforce hierarchical task constraints where higher priorities dictate lower priority limits.
Processes priorities in descending order (priority 1 → 64) using FIFO task postponement."
  (interactive)
  (let* ((agenda-files (org-agenda-files))
	  (priority-counts (make-hash-table :test 'equal))
	  (tasks-by-priority (make-hash-table :test 'equal)))

    ;; ==== PHASE 1: Data Collection ====
    (dolist (file agenda-files)
    (with-current-buffer (find-file-noselect file)
	(save-excursion
	  (goto-char (point-min))
	  ;; Process tasks from top to bottom (earlier positions first)
	  (org-map-entries
	    (lambda ()
	      (when (my-is-outstanding-task)
		(let* ((priority-str (org-entry-get nil "PRIORITY"))
		      (priority (if priority-str 
				  (string-to-number priority-str)
				  org-priority-default))
		      (task-entry (cons (buffer-file-name) (point))))
		  ;; Store tasks in reverse order to facilitate O(1) pop-front
		  (puthash priority 
			  (nconc (gethash priority tasks-by-priority) (list task-entry))
			  tasks-by-priority)
		  (puthash priority 
			  (1+ (gethash priority priority-counts 0))
			  priority-counts))))
	    nil 'file))))

    ;; ==== PHASE 2: Priority Order Setup ====
    ;; 1. Get sorted priorities 1 (highest) to 64 (lowest)
    (let* ((sorted-priorities (sort (hash-table-keys priority-counts) #'<))
	    (current-max nil)
	    highest-processed)

	;; Check if there are any priorities to process
	(if (zerop (length sorted-priorities))
	    (message "[COMPLETE] No outstanding tasks found - no constraints to enforce")
	  
	  ;; ==== PHASE 3: Constraint Processing ====
	  ;; Process highest (1 → 64) priorities first
	  (dolist (prio sorted-priorities)
	    ;; No reverse needed since (sort '<) returns 1,2,...,64
	    (let ((count (gethash prio priority-counts 0))
		  (tasks (gethash prio tasks-by-priority '())))
	      (cond
		((zerop count) ; Skip empty priorities
		nil)

		;; Set initial constraint from highest priority with tasks
		((null current-max)
		(setq current-max count
		      highest-processed prio)
		(message "[CONSTRAINT] Priority %d = new global max: %d" 
			prio current-max))

		;; Enforce constraints for lower priorities
		((> count current-max)
		(let ((excess (- count current-max)))
		  (message "[ENFORCE] Priority %d overflow (%d > max %d). Postponing %d tasks..."
			  prio count current-max excess)
		  ;; Process oldest tasks first (FIFO through list order)
		  (dotimes (_ excess)
		    (when-let ((task (pop tasks))) 
		      (let ((file (car task))
			    (pos (cdr task)))
			;; Postpone logic
			(with-current-buffer (find-file-noselect file)
			  (goto-char pos)
			  (my-postpone-schedule)
			  (message "Postponed priority %d task: %s" prio 
				  (file-name-nondirectory file))))))
		  (puthash prio tasks tasks-by-priority)))
		
		;; Update current-max for subsequent priorities
		(t
		(setq current-max (min current-max count))))))

	  ;; ==== PHASE 4: Cleanup ====
	  (save-some-buffers t)
	  (message "[COMPLETE] Constrained %d priorities. Final max: %d"
		  (hash-table-count priority-counts)
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

(defvar my-queue--orchestrating nil
  "Prevent re-entrancy while orchestrating SRS/Anki.")

(defun my-queue-handle-srs-after-task-display ()
  "Launch/focus Anki first, then (re)start org-srs reviews.
Guarded by `my-queue--orchestrating' to avoid re-entrancy."
  (unless my-queue--orchestrating
    (let ((my-queue--orchestrating t))
      (condition-case err
          (if my-android-p
              (my-launch-anki)
            (progn
              ;; Launch Anki first so you can start studying immediately
              (my-launch-anki)
              ;; Then (re)start SRS in Emacs
              (ignore-errors (my-srs-quit-reviews))
              (ignore-errors (my-srs-start-reviews))))
        (error
         (message "org-queue: SRS/Anki orchestration failed: %s"
                  (error-message-string err)))))))

(defun my-show-next-outstanding-task ()
  "Show the next outstanding task with proper SRS handling.
Also limits visible queue buffers around the new current task."
  (interactive)

  (widen-and-recenter)

  ;; Ensure we have synchronized data
  (my-ensure-synchronized-task-list)
  
  ;; If no list exists or we're at the end, refresh
  (when (or (not my-outstanding-tasks-list)
            (>= my-outstanding-tasks-index (length my-outstanding-tasks-list)))
    (if my-outstanding-tasks-list
        (setq my-outstanding-tasks-index 0)
      (my-get-outstanding-tasks)))
  
  ;; Advance index with wrap
  (when (and my-outstanding-tasks-list
             (< my-outstanding-tasks-index (length my-outstanding-tasks-list)))
    (setq my-outstanding-tasks-index (1+ my-outstanding-tasks-index))
    (when (>= my-outstanding-tasks-index (length my-outstanding-tasks-list))
      (setq my-outstanding-tasks-index 0)))
  
  ;; Save the updated index
  (my-save-index-to-file)
  
  ;; Show the task
  (if (and my-outstanding-tasks-list
           (< my-outstanding-tasks-index (length my-outstanding-tasks-list)))
      (let ((task-or-marker (nth my-outstanding-tasks-index my-outstanding-tasks-list)))
        (my-display-task-at-marker task-or-marker)
        (my-pulse-highlight-current-line)

        ;; Limit visible queue buffers now that we have moved
        (my-queue-limit-visible-buffers)
        
        ;; SRS handling
        (my-queue-handle-srs-after-task-display)

        (my-show-current-flag-status))
    (message "No outstanding tasks found.")))

(defun my-show-previous-outstanding-task ()
  "Show the previous outstanding task, cycling if needed.
Also limits visible queue buffers around the new current task."
  (interactive)

  (widen-and-recenter)
  (my-ensure-synchronized-task-list)
  
  (if my-outstanding-tasks-list
      (progn
        (if (<= my-outstanding-tasks-index 0)
            (setq my-outstanding-tasks-index (1- (length my-outstanding-tasks-list)))
          (setq my-outstanding-tasks-index (1- my-outstanding-tasks-index)))
        
        (my-save-index-to-file)
        
        (let ((task-or-marker (nth my-outstanding-tasks-index my-outstanding-tasks-list)))
          (my-display-task-at-marker task-or-marker)
          (my-pulse-highlight-current-line)

          ;; Limit visible queue buffers now that we have moved
          (my-queue-limit-visible-buffers)

          (my-show-current-flag-status)))
    (message "No outstanding tasks to navigate.")))

(defun my-show-current-outstanding-task-no-srs (&optional pulse)
  "Show current outstanding task without re-triggering SRS/Anki."
  (interactive)
  (widen-and-recenter)
  (my-ensure-synchronized-task-list)
  (when (and my-outstanding-tasks-list
             (< my-outstanding-tasks-index (length my-outstanding-tasks-list)))
    (let ((task (nth my-outstanding-tasks-index my-outstanding-tasks-list)))
      (my-display-task-at-marker task)
      (when pulse (my-pulse-highlight-current-line))
      (my-queue-limit-visible-buffers)
      (my-show-current-flag-status))))

(defun my-show-current-outstanding-task ()
  "Show the current outstanding task, or get a new list and show the first task if not valid.
Also limits visible queue buffers around the current task."
  (interactive)

  (widen-and-recenter)
  (my-ensure-synchronized-task-list)
    
  (if (and my-outstanding-tasks-list
           (< my-outstanding-tasks-index (length my-outstanding-tasks-list)))
      (let ((task-or-marker (nth my-outstanding-tasks-index my-outstanding-tasks-list)))
        (my-display-task-at-marker task-or-marker)
        (my-pulse-highlight-current-line)

        ;; Limit visible queue buffers for current position
        (my-queue-limit-visible-buffers)

        ;; SRS handling
        (my-queue-handle-srs-after-task-display)

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
(defun my-auto-task-setup ()
  "Initialize and set up automatic task management processes upon Emacs startup."
  (my-launch-anki)
  (message "Starting automatic task setup...")
  (my-org-id-initialize-id-locations)

  ;; Step 1: Try to load from cache
  (message "Step 1: Attempting to load tasks from file...")
  (let ((cache-loaded (and (fboundp 'my-load-outstanding-tasks-from-file)
                           (my-load-outstanding-tasks-from-file))))
    (message "Step 1 complete: Cache loaded? %s" cache-loaded)
    (message "Checking task format - First task: %S"
             (and my-outstanding-tasks-list (car my-outstanding-tasks-list)))

    ;; Step 2: Maintenance if no valid cache
    (unless cache-loaded
      (message "Step 2B: No cache, running maintenance...")
      (when (require 'org-roam nil t)
        (message "Step 2B.1: Setting up org-roam...")
        (let ((warning-minimum-level :error))
          (org-roam-db-autosync-mode)
          (org-id-update-id-locations (org-agenda-files)))
        (message "Step 2B.1 complete"))

      (message "Step 2B.2.1: Ensuring priorities and schedules...")
      (my-ensure-priorities-and-schedules-for-all-headings)

      (message "Step 2B.2.2: Advancing schedules...")
      (my-auto-advance-schedules 8)

      (message "Step 2B.2.3: Postponing overdue tasks...")
      (my-auto-postpone-overdue-tasks)

      (message "Step 2B.2.4: Postponing duplicate priority tasks...")
      (my-postpone-duplicate-priority-tasks)

      (message "Step 2B.2.5: Enforcing priority constraints...")
      (my-enforce-priority-constraints)

      (message "Step 2B.2.6: Re-ensuring priorities and schedules...")
      (my-ensure-priorities-and-schedules-for-all-headings)

      (message "Step 2B.2.7: Postponing consecutive same-file tasks...")
      (my-postpone-consecutive-same-file-tasks)

      (message "Step 2B.2.8: Cleaning up DONE tasks...")
      (my-cleanup-all-done-tasks)

      (message "Step 2B.3: Getting outstanding tasks...")
      (my-get-outstanding-tasks)
      (setq my-outstanding-tasks-index 0)

      (message "Step 2B.4: Saving tasks to file...")
      (my-save-outstanding-tasks-to-file)
      (message "Maintenance complete")))

  ;; Step 4: Optional SRS pre-init (set org-queue-preinit-srs to t if you want it)
  (when (boundp 'org-queue-preinit-srs)
    (when org-queue-preinit-srs
      (message "Step 4: Pre-initializing SRS system...")
      (unless my-android-p
	(condition-case err
            (progn
              (my-srs-quit-reviews)
              (let ((temp-frame (make-frame '((visibility . nil) (width . 80) (height . 24)))))
		(unwind-protect
                    (with-selected-frame temp-frame
                      (cl-letf (((symbol-function 'read-key) (lambda (&rest _) 32)))
			(ignore-errors (my-srs-start-reviews)))
                      (my-srs-quit-reviews))
                  (delete-frame temp-frame))))
          (error
           (message "org-queue: SRS pre-init disabled due to error: %s"
                    (error-message-string err)))))))

  (message "✓ Automatic task setup completed successfully.")

  ;; Schedule task display
  (run-with-idle-timer 1.5 nil
                       (lambda ()
                         (condition-case err
                             (if (and my-outstanding-tasks-list
                                      (> (length my-outstanding-tasks-list) 0)
                                      (< my-outstanding-tasks-index (length my-outstanding-tasks-list)))
                                 (let ((task-or-marker (nth my-outstanding-tasks-index my-outstanding-tasks-list)))
                                   (delete-other-windows)
                                   (my-display-task-at-marker task-or-marker)
                                   (my-show-current-flag-status))
                               (message "Task list empty or invalid index"))
                           (error
                            (message "Error preparing task display: %s" (error-message-string err))))))
  (message "Task display scheduled."))

;; Add startup hook
(add-hook 'emacs-startup-hook #'my-auto-task-setup 100)

(provide 'org-queue-tasks)
;;; org-queue-tasks.el ends here
