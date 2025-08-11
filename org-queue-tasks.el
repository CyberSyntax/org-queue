;;; org-queue-tasks.el --- Task management and navigation for org-queue -*- lexical-binding: t -*-

;;; Code:

(require 'cl-lib)
(require 'org-id)
(require 'org-queue-config)
(require 'org-queue-utils)
(require 'org-queue-srs-bridge)

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

(defcustom org-queue-directory nil
  "Base directory for task files for Org Queue.
                If nil, a safe default directory will be used and created automatically."
  :type 'directory
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

(defun my-is-outstanding-task ()
  "Return non-nil if the current task is overdue or due today."
  (let ((scheduled-time (org-get-scheduled-time nil))
        (heading (org-get-heading t t t t)))
    (let ((result (and scheduled-time
                       (<= (time-to-days scheduled-time) (time-to-days (current-time))))))
      result)))

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
  "Save task list to cache using (file . id) pairs instead of positions.
Also saves the index separately."
  (let* ((today (format-time-string "%Y-%m-%d"))
         (pairs
          (cl-loop for entry in my-outstanding-tasks-list
                   for marker = (my-extract-marker entry)
                   if (and (markerp marker)
                           (marker-buffer marker)
                           (buffer-live-p (marker-buffer marker))
                           (buffer-file-name (marker-buffer marker)))
                   collect
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
                         (when id (cons path id))))))))
    (with-temp-file my-outstanding-tasks-cache-file
      (insert (prin1-to-string (list :date today
                                     :tasks (delq nil pairs)))))
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
This function uses targeted org-id location updates for efficiency,
logs unresolved IDs, and skips entries that cannot be resolved."
  (when (file-exists-p my-outstanding-tasks-cache-file)
    (let* ((data (with-temp-buffer
                   (insert-file-contents my-outstanding-tasks-cache-file)
                   (read (buffer-string))))
           (saved-date (plist-get data :date))
           (saved-tasks (plist-get data :tasks))
           (today (format-time-string "%Y-%m-%d")))
      (when (string= saved-date today)
        ;; No longer doing agenda-wide update here - moved to per-ID basis
        (let ((result-list nil)
              (resolved 0)
              (unresolved 0))
          (dolist (task-pair saved-tasks)
            (let* ((stored-path (car task-pair))
                   (id (cdr task-pair))
                   (abs-path (if (or (file-name-absolute-p stored-path)
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
              ;; Now check if we have a valid marker
              (if (and marker
                       (markerp marker)
                       (marker-buffer marker)
                       (buffer-live-p (marker-buffer marker)))
                  ;; Build plist for resolved task
                  (with-current-buffer (marker-buffer marker)
                    (save-excursion
                      (goto-char (marker-position marker))
                      (let* ((priority (my-get-raw-priority-value))
                             (flag (my-priority-flag priority))
                             (file abs-path)
                             (task-plist (list :id id
                                               :marker marker
                                               :priority priority
                                               :flag flag
                                               :file file)))
                        (push task-plist result-list)
                        (setq resolved (1+ resolved)))))
                ;; Unresolved path
                (setq unresolved (1+ unresolved))
                (message "org-queue: could not resolve ID %s (stored path: %s)"
                         id stored-path))))
          (setq my-outstanding-tasks-list (nreverse result-list))
          (message "org-queue: resolved %d IDs, %d unresolved (from cache)" resolved unresolved)
          ;; Load index; fallback to 0
          (unless (and (my-load-index-from-file)
                       (< my-outstanding-tasks-index (length my-outstanding-tasks-list)))
            (setq my-outstanding-tasks-index 0))
          t)))))

(defun my-ensure-synchronized-task-list ()
  "Ensure we have a current, synchronized task list and valid index."
  ;; Try to load from cache first
  (unless (my-load-outstanding-tasks-from-file)
    ;; If cache is stale or missing, regenerate
    (my-get-outstanding-tasks)
    (setq my-outstanding-tasks-index 0)
    (my-save-outstanding-tasks-to-file))
  
  ;; Double-check index validity after loading
  (when (or (>= my-outstanding-tasks-index (length my-outstanding-tasks-list))
            (< my-outstanding-tasks-index 0))
    (setq my-outstanding-tasks-index 0)
    (my-save-index-to-file)))

(defun my-get-outstanding-tasks ()
  "Populate task list with metadata for stable tracking.
  Orders tasks with TODO items first (by priority), then non-TODO items (by priority)."
  (setq my-outstanding-tasks-list nil)
  (org-map-entries
   (lambda ()
     (when (and (my-is-outstanding-task)
                (not (my-is-done-task))              ; Exclude DONE tasks - FIXED
                (not (org-srs-entry-p (point))))
       (let* ((marker (point-marker))
              (id (org-entry-get nil "ID"))
              (priority (my-get-raw-priority-value))
              (flag (my-priority-flag priority))
              (file (buffer-file-name))
              (heading (org-get-heading t t t t))    ; DEBUG: Get heading
              (todo-state (org-get-todo-state))      ; DEBUG: Get TODO state
              (is-todo (my-is-todo-task))            ; Check if it's TODO
              (task-data (list :id id
                              :marker marker
                              :priority priority
                              :flag flag
                              :file file
                              :is-todo is-todo)))     ; Store TODO status
         
         ;; DEBUG: Log each task being processed
         (message "QUEUE BUILD: %s | TODO-state: %s | is-todo: %s | priority: %s" 
                  heading todo-state is-todo priority)
         
         ;; Create sorting key: (todo-status, priority)
         ;; TODO items get 0 (higher priority), non-TODO get 1 (lower priority)
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
                          ;; Compare first by TODO status (0 vs 1)
                          (if (= (car key-a) (car key-b))
                              ;; If same TODO status, compare by priority
                              (< (cadr key-a) (cadr key-b))
                            ;; Different TODO status, TODO (0) comes first
                            (< (car key-a) (car key-b))))))))
  
  ;; DEBUG: Final summary
  (let ((todo-count (length (cl-remove-if-not (lambda (task) (plist-get task :is-todo)) my-outstanding-tasks-list)))
        (total-count (length my-outstanding-tasks-list)))
    (message "=== QUEUE BUILT ===")
    (message "Total tasks in queue: %d" total-count)
    (message "TODO tasks in queue: %d" todo-count))
  
  (setq my-outstanding-tasks-index 0))

(defun my-remove-current-task ()
  "Remove the task at current index from outstanding tasks list and update cache file."
  (interactive)
  
  ;; Validate the task list
  (unless (and my-outstanding-tasks-list 
               (> (length my-outstanding-tasks-list) 0)
               (< my-outstanding-tasks-index (length my-outstanding-tasks-list)))
    (message "No valid task at current index to remove")
    (cl-return-from my-remove-current-task nil))
  
  ;; Remove the task
  (setq my-outstanding-tasks-list
        (append (seq-take my-outstanding-tasks-list my-outstanding-tasks-index)
                (seq-drop my-outstanding-tasks-list (1+ my-outstanding-tasks-index))))
  
  ;; Set index using your exact logic
  (if (<= my-outstanding-tasks-index 0)
      (setq my-outstanding-tasks-index (1- (length my-outstanding-tasks-list)))
    (setq my-outstanding-tasks-index (1- my-outstanding-tasks-index)))
  
  ;; Update cache file (this will also save the updated index separately)
  (my-save-outstanding-tasks-to-file)
  
  ;; Show feedback
  (message "Task removed. %d tasks remaining." (length my-outstanding-tasks-list))
  
  ;; Regenerate list if it became empty
  (when (zerop (length my-outstanding-tasks-list))
    (my-get-outstanding-tasks)
    (setq my-outstanding-tasks-index 0)
    (my-save-outstanding-tasks-to-file)))

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

(defun my-show-next-outstanding-task ()
  "Show the next outstanding task in priority order with proper SRS handling."
  (interactive)

  (widen-and-recenter)

  ;; Ensure we have synchronized data
  (my-ensure-synchronized-task-list)

  ;; First check if SRS session just ended (detect message)
  (when (and (current-message)
             (string-match-p "No more cards to review" (current-message)))
    (message "Review session complete - continuing with tasks")
    (sit-for 1)  ;; Brief pause for user to see the message
    (setq my-srs-reviews-exhausted t))
  
  ;; If no list exists or we're at the end, get/refresh the list
  (when (or (not my-outstanding-tasks-list)
            (>= my-outstanding-tasks-index (length my-outstanding-tasks-list)))
    (if my-outstanding-tasks-list
        ;; If we have a list but reached the end, reset index to 0
        (setq my-outstanding-tasks-index 0)
      ;; If no list, get it
      (my-get-outstanding-tasks)))
  
  ;; Increment index for next task
  (when (and my-outstanding-tasks-list 
             (< my-outstanding-tasks-index (length my-outstanding-tasks-list)))
    (setq my-outstanding-tasks-index (1+ my-outstanding-tasks-index))
    ;; Handle wraparound if we go past the end
    (when (>= my-outstanding-tasks-index (length my-outstanding-tasks-list))
      (setq my-outstanding-tasks-index 0)))
  
  ;; Save the updated index
  (my-save-index-to-file)
  
  ;; Now show the task at the current index
  (if (and my-outstanding-tasks-list
           (< my-outstanding-tasks-index (length my-outstanding-tasks-list)))
      (let ((task-or-marker (nth my-outstanding-tasks-index my-outstanding-tasks-list)))
        ;; Display operations
        (my-display-task-at-marker task-or-marker)
        (my-pulse-highlight-current-line)
        
        ;; Handle SRS reviews
        (if my-android-p
            ;; On Android: skip SRS, just launch Anki
            (my-launch-anki)
          ;; On desktop: use SRS integration
          (if (not my-srs-reviews-exhausted)
              (progn
                (my-srs-quit-reviews)
                (condition-case nil
                    (my-srs-start-reviews)
                  (error (setq my-srs-reviews-exhausted t))))
            (my-launch-anki)))

        (my-show-current-flag-status))
    (message "No outstanding tasks found.")))

(defun my-show-previous-outstanding-task ()
  "Show the previous outstanding task in priority order, cycling if needed."
  (interactive)

  (widen-and-recenter)
  
  ;; Ensure we have synchronized data
  (my-ensure-synchronized-task-list)
  
  (if my-outstanding-tasks-list
      (progn
        ;; Update index BEFORE showing the task for consistency
        (if (<= my-outstanding-tasks-index 0)
            (setq my-outstanding-tasks-index (1- (length my-outstanding-tasks-list)))
          (setq my-outstanding-tasks-index (1- my-outstanding-tasks-index)))
        
        ;; Save the updated index
        (my-save-index-to-file)
        
        (let ((task-or-marker (nth my-outstanding-tasks-index my-outstanding-tasks-list)))
          (my-display-task-at-marker task-or-marker)
          (my-pulse-highlight-current-line)
          (my-show-current-flag-status)))
    (message "No outstanding tasks to navigate.")))

(defun my-show-current-outstanding-task ()
  "Show the current outstanding task, or get a new list and show the first task if not valid."
  (interactive)

  (widen-and-recenter)  ;; This will only work if already in org-mode
  
  ;; Ensure we have synchronized data
  (my-ensure-synchronized-task-list)
    
  (if (and my-outstanding-tasks-list 
           (< my-outstanding-tasks-index (length my-outstanding-tasks-list)))
      (let ((task-or-marker (nth my-outstanding-tasks-index my-outstanding-tasks-list)))
        ;; Display operations (will switch to proper org buffer)
        (my-display-task-at-marker task-or-marker)
        (my-pulse-highlight-current-line)
        (my-show-current-flag-status))
    ;; Truly no tasks - unlikely after synchronization above
    (message "No outstanding tasks found.")))

;; Utility functions for task management
(defun my-auto-task-setup ()
  "Initialize and set up automatic task management processes upon Emacs startup."
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
            (if (not my-srs-reviews-exhausted)
                (progn
                  (my-srs-quit-reviews)
                  (let ((temp-frame (make-frame '((visibility . nil) (width . 80) (height . 24)))))
                    (unwind-protect
                        (with-selected-frame temp-frame
                          (cl-letf (((symbol-function 'read-key) (lambda (&rest _) 32)))
                            (condition-case nil
                                (my-srs-start-reviews)
                              (error (setq my-srs-reviews-exhausted t))))
                          (my-srs-quit-reviews))
                      (delete-frame temp-frame))))
              (my-launch-anki))
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
