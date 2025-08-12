;;; org-queue-priority.el --- Priority management for org-queue -*- lexical-binding: t -*-

;;; Code:

(require 'org-queue-config)
(require 'org-queue-utils)
(require 'org-queue-srs-bridge)

(defun my-set-priority-with-heuristics (&optional specific-range retried)
  "Set a random priority within a user-defined heuristic range with retry mechanism.
Optional RETRIED is used internally to prevent infinite recursion."
  (interactive)
  (let* ((priority-ranges my-priority-ranges)
	   (max-retries 3)
	   (retry-delay 0.01)
	   (range
	    (cond
	     (specific-range
	      (cdr (assoc specific-range priority-ranges)))
	     ((called-interactively-p 'any)
	      (let* ((default-range (or (my-get-current-priority-range) (+ 6 (random 4))))
		     (user-choice (read-number
				   "Select a priority range (0-9): "
				   default-range)))
		(cdr (assoc user-choice priority-ranges))))
	     (t
	      (cdr (assoc (or (my-get-current-priority-range) (+ 6 (random 4))) priority-ranges)))))
	   (success nil)
	   (attempt 0)
	   random-priority)
    (if range
	  (let* ((min-priority (car range))
		 (max-priority (cdr range))
		 (desired-priority (+ min-priority
				      (random (1+ (- max-priority min-priority)))))
		 final-priority)
	    (setq random-priority desired-priority)
	    (while (and (not success) (< attempt max-retries))
	      (condition-case err
		  (progn
		    ;; Ensure heading state consistency
		    (when (org-at-heading-p) 
		      (org-back-to-heading t)
		      (org-show-entry)
		      (redisplay))
		    ;; Original priority adjustment logic
		    (let* ((current-priority (string-to-number
					     (or (org-entry-get nil "PRIORITY")
						 (number-to-string org-priority-default))))
			   (delta (- desired-priority current-priority)))
		      (cond
		       ((< delta 0)
			(dotimes (_ (abs delta))
			  (org-priority-up)))
		       ((> delta 0)
			(dotimes (_ delta)
			  (org-priority-down)))
		       (t
			(if (= current-priority org-priority-highest)
			    (progn
			      (org-priority-down)
			      (org-priority-up))
			  (if (= current-priority org-priority-lowest)
			      (progn
				(org-priority-up)
				(org-priority-down))
			    (progn
			      (org-priority-up)
			      (org-priority-down))))))
		      ;; Priority validation
		      (setq final-priority (string-to-number
					   (or (org-entry-get nil "PRIORITY")
					       (number-to-string org-priority-default))))
		      (unless (and final-priority 
				   (integerp final-priority)
				   (= final-priority desired-priority))
			(error "Priority validation failed")))
		    (setq success t))
		;; Error handling with automatic retry
		(error 
		 (setq attempt (1+ attempt))
		 (when (and (< attempt max-retries) 
			    (not (org-entry-get nil "PRIORITY")))
		   (org-entry-put nil "PRIORITY" 
				 (number-to-string org-priority-default)))
		 (if (< attempt max-retries)
		     (progn (message "Retrying (%d/%d)..." attempt max-retries)
			    (sleep-for retry-delay))
		   ;; Trigger auto-retry if not already retried
		   (message "Failed after %d attempts: %s" 
			    max-retries (error-message-string err))
		   (unless retried
		     (message "Auto-retrying...")
		     (my-set-priority-with-heuristics specific-range t))))))
	    (when success 
	      (message "Priority set to: %d" random-priority)))
	(message "Invalid range."))))

(defun my-increase-priority-range ()
  "Increase the priority range by moving to a lower number (0 is the highest priority).
Adjusts the priority within the new range, even if already at the highest."
  (interactive)
  (let ((current-range (or (my-get-current-priority-range) 9)))
    (let ((new-range (max 0 (1- current-range))))
	(my-set-priority-with-heuristics new-range)
	(message "Priority range increased to %d" new-range))))

(defun my-decrease-priority-range ()
  "Decrease the priority range by moving to a higher number (9 is the lowest priority).
Adjusts the priority within the new range, even if already at the lowest."
  (interactive)
  (let ((current-range (or (my-get-current-priority-range) 9)))
    (let ((new-range (min 9 (1+ current-range))))
	(my-set-priority-with-heuristics new-range)
	(message "Priority range decreased to %d" new-range))))

(defun my-ensure-priority-set (&optional max-attempts)
  "Ensure the current heading has a priority set.
If PRIORITY is not set, assign one within the appropriate range.
If PRIORITY is set, reassign a priority within the same range.
Skip priority setting if this entry's SRS drawer is in its parent.
MAX-ATTEMPTS: Maximum number of retry attempts (defaults to 15)."
  (let ((max-attempts (or max-attempts 15))
        (attempt 0)
        (success nil))
    
    ;; First check if this is a parent-level SRS entry
    (save-excursion
      (org-back-to-heading t)
      (when (eq (org-srs-entry-p (point)) 'parent)
        (message "Skipping priority set - SRS drawer is in parent entry")
        (setq success t)))  ;; Mark as successful to skip the loop
    
    (while (and (not success) (< attempt max-attempts))
      (setq attempt (1+ attempt))
      
      (condition-case err
          (save-excursion
            ;; Move to the current heading
            (org-back-to-heading t)
            ;; Ensure the heading is fully visible
            (org-show-entry)
            ;; Retrieve the current PRIORITY property
            (let ((current-priority (org-entry-get nil "PRIORITY")))
              (if (and current-priority (not (string= current-priority " ")))
                  ;; PRIORITY is set; determine its range and reassign within the same range
                  (let* ((priority-value (string-to-number current-priority))
                         (current-range (my-find-priority-range priority-value)))
                    (if current-range
                        (progn
                          (my-set-priority-with-heuristics current-range)
                          (message "Priority reassigned within range %d." current-range)
                          (setq success t))
                      (message "Current priority %d does not fall within any defined range."
                               priority-value)))
                ;; PRIORITY is not set; assign a random priority within appropriate ranges
                (let* ((matching-ranges
                        (cl-remove-if-not
                         (lambda (range)
                           (let ((min (car (cdr range)))
                                 (max (cdr (cdr range))))
                             (and (<= min org-priority-lowest)
                                  (>= max org-priority-default))))
                         my-priority-ranges))
                       (range-ids (mapcar #'car matching-ranges)))
                  (if range-ids
                      (let ((selected-range (nth (random (length range-ids)) range-ids)))
                        (my-set-priority-with-heuristics selected-range)
                        (message "Priority was not set. Assigned random priority within range %d."
                                 selected-range)
                        (setq success t))
                    (error "No valid range found for default priority settings. Check configurations."))))))
        (error
         (message "Attempt %d/%d failed in my-ensure-priority-set: %s" 
                  attempt max-attempts (error-message-string err))
         (when (>= attempt max-attempts)
           (signal (car err) (cdr err))))))
    
    (unless success
      (error "Failed to set priority after %d attempts" max-attempts))))

(defun my-get-raw-priority-value ()
  "Return numeric priority at point.
Order:
- Numeric PRIORITY property if present,
- Else [#N] cookie from the heading,
- Else random in [org-priority-default .. org-priority-lowest]."
  (let* ((s (org-entry-get nil "PRIORITY"))
         (cookie (my--org-heading-get-cookie-priority)))
    (cond
     ((and s (string-match-p "^[ \t]*[0-9]+[ \t]*$" s))
      (string-to-number (string-trim s)))
     ((numberp cookie)
      cookie)
     (t
      (+ org-priority-default
         (random (1+ (- org-priority-lowest org-priority-default))))))))

(defun my--org-heading-get-cookie-priority ()
  "Return numeric [#N] cookie at current heading, or nil."
  (save-excursion
    (org-back-to-heading t)
    (let ((bol (line-beginning-position))
          (eol (line-end-position)))
      (goto-char bol)
      (when (re-search-forward "\\[#[ \t]*\\([0-9]+\\)[ \t]*\\]" eol t)
        (string-to-number (match-string 1))))))

(defun my-read-numeric-priority-here ()
  "Read numeric priority at point from property or cookie."
  (let ((s (org-entry-get nil "PRIORITY")))
    (cond
     ((and s (string-match-p "^[ \t]*[0-9]+[ \t]*$" s))
      (string-to-number (string-trim s)))
     (t
      (my--org-heading-get-cookie-priority)))))

(defun my-set-numeric-priority-here (n)
  "Set numeric priority at the current heading both as property and [#N] cookie."
  (let* ((min (or (and (numberp org-priority-highest) org-priority-highest) 1))
         (max (or (and (numberp org-priority-lowest)  org-priority-lowest) 64))
         (n   (max min (min max (or n min)))))
    ;; Set property
    (org-entry-put nil "PRIORITY" (number-to-string n))
    ;; Update/insert [#N] cookie in the heading line
    (save-excursion
      (org-back-to-heading t)
      (let ((bol (line-beginning-position))
            (eol (line-end-position)))
        (goto-char bol)
        (if (re-search-forward "\\[#[ \t]*[0-9]+[ \t]*\\]" eol t)
            (replace-match (format "[#%d]" n) t t)
          (goto-char bol)
          (when (looking-at "^\\*+\\s-+")
            (goto-char (match-end 0)))
          (insert (format "[#%d] " n)))))))

(defun my-get-priority-value ()
  "Get the priority value of the current task from the task list."
  (let* ((task (nth my-outstanding-tasks-index my-outstanding-tasks-list))
         (marker (my-extract-marker task))
         (priority-str (when (and marker (marker-buffer marker))
                         (org-with-point-at marker
                           (org-entry-get nil "PRIORITY")))))
    (if priority-str
        (string-to-number priority-str)
      (+ org-priority-default
         (random (+ 1 (- org-priority-lowest org-priority-default)))))))

;; Spread: core queue spread function
(defun my--clamp (x lo hi)
  "Clamp X into [LO, HI]."
  (max lo (min hi x)))

(defun my-queue-spread-priorities (start-idx end-idx lo hi)
  "Evenly distribute integer PRIORITY values between LO and HI (inclusive)
over the queue subset [START-IDX..END-IDX] (0-based indices).
Returns the list of priorities that were applied, in order.

Notes:
- Preserves queue order; does NOT resort the list.
- Writes to each entry's Org PRIORITY property and updates the cached plist (:priority and :flag).
- If not enough distinct integer slots exist (HI-LO+1 < N), duplicates are inevitable."
  (unless (and (numberp start-idx) (numberp end-idx) (<= start-idx end-idx))
    (user-error "Invalid index range"))
  (my-ensure-synchronized-task-list)
  (let* ((n (1+ (- end-idx start-idx)))
         (span (- hi lo))
         ;; If n == 1, assign the middle; else space across [0..n-1]
         (targets
          (if (= n 1)
              (list (round (/ (+ lo hi) 2.0)))
            (cl-loop for i from 0 to (1- n)
                     for q = (/ (float i) (float (1- n)))  ;; 0..1
                     for v = (round (+ lo (* q span)))
                     collect v)))
         ;; Make sure they are at least non-decreasing and clamped
         (adjusted
          (let ((acc '())
                (prev (1- lo)))
            (dolist (v targets (nreverse acc))
              (setq v (my--clamp v lo hi))
              ;; Ensure monotonic non-decreasing; if not enough slots, duplicates may remain.
              (when (< v prev) (setq v prev))
              (push v acc)
              (setq prev v)))))
    ;; Apply to entries
    (cl-loop
     for idx from start-idx to end-idx
     for newp in adjusted
     do (let* ((task (nth idx my-outstanding-tasks-list))
               (marker (my-extract-marker task)))
          (when (and (markerp marker)
                     (marker-buffer marker)
                     (buffer-live-p (marker-buffer marker)))
            (org-with-point-at marker
              (org-entry-put nil "PRIORITY" (number-to-string newp)))
            ;; Update plist cache too
            (plist-put task :priority newp)
            (plist-put task :flag (my-priority-flag newp)))))
    (my-save-outstanding-tasks-to-file)
    adjusted))

(provide 'org-queue-priority)
;;; org-queue-priority.el ends here
