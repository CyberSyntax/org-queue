(require 'org-agenda)
(require 'cl-lib)  ;; Required for cl-find-if and cl-remove-if-not

(random t)

(defun random-float (min max)
  "Return a random float between MIN and MAX."
  (+ min (* (- max min) (/ (float (random 1000000)) 1000000))))

(setq org-priority-highest 1)
(setq org-priority-default 32)
(setq org-priority-lowest 64)

(defcustom my-priority-ranges
  '((0 . (1 . 1))
    (1 . (2 . 5))
    (2 . (5 . 12))
    (3 . (12 . 18))
    (4 . (18 . 24))
    (5 . (24 . 30))
    (6 . (30 . 37))
    (7 . (37 . 45))
    (8 . (45 . 58))
    (9 . (58 . 64)))
  "Global priority ranges for setting random priorities.
									    Each entry is a cons cell where the car is the range identifier
									    and the cdr is a cons cell representing the minimum and maximum priority values."
  :type '(alist :key-type integer :value-type (cons integer integer))
  :group 'org-queue)

(defun my-find-priority-range (priority)
  "Find the range identifier for a given PRIORITY."
  (let ((range-found
	   (cl-find-if
	    (lambda (range)
	      (let ((min (car (cdr range)))
		    (max (cdr (cdr range))))
		(and (>= priority min) (<= priority max))))
	    my-priority-ranges)))
    (when range-found
	(car range-found))))

(defun my-get-current-priority-range ()
  "Determine the priority range of the current heading.
									    Returns the range identifier if priority is set; otherwise, nil."
  (let ((current-priority (org-entry-get nil "PRIORITY")))
    (when (and current-priority (not (string= current-priority " ")))
	(let ((priority-value (string-to-number current-priority)))
	  (my-find-priority-range priority-value)))))

(defvar org-queue-mode-map (make-sparse-keymap)
  "Keymap for org-queue-mode.")

(progn
  ;; Commands
  (define-key org-queue-mode-map (kbd ",") #'my-set-priority-with-heuristics)
  (define-key org-queue-mode-map (kbd "s") #'my-schedule-command)
  (define-key org-queue-mode-map (kbd "f") #'my-show-next-outstanding-task)
  (define-key org-queue-mode-map (kbd "b") #'my-show-previous-outstanding-task)
  (define-key org-queue-mode-map (kbd "c") #'my-show-current-outstanding-task)
  (define-key org-queue-mode-map (kbd "R") #'my-reset-and-show-current-outstanding-task)
  (define-key org-queue-mode-map (kbd "i") #'my-increase-priority-range)
  (define-key org-queue-mode-map (kbd "d") #'my-decrease-priority-range)
  (define-key org-queue-mode-map (kbd "D") #'org-demote-subtree)
  (define-key org-queue-mode-map (kbd "a") #'my-advance-schedule)
  (define-key org-queue-mode-map (kbd "p") #'my-postpone-schedule)
  (define-key org-queue-mode-map (kbd "P") #'org-promote-subtree)
  (define-key org-queue-mode-map (kbd "n") #'org-narrow-to-subtree)
  (define-key org-queue-mode-map (kbd "w") #'widen-and-recenter)
  (define-key org-queue-mode-map (kbd "W") #'org-cut-subtree)
  (define-key org-queue-mode-map (kbd "Y") #'org-paste-subtree)
  (define-key org-queue-mode-map (kbd "u") #'org-show-parent-heading-cleanly)
  (when (require 'gptel nil t)
    (define-key org-queue-mode-map (kbd "g") #'gptel))

  ;; Exit key
  (define-key org-queue-mode-map (kbd "e") 
		(lambda () (interactive) (org-queue-mode -1))))

;; Define a simple ignore function - no conditionals
(defun org-queue-ignore ()
  "Ignore key presses unconditionally."
  (interactive)
  (message "Key blocked by org-queue-mode (press e to exit)")
  (ignore))

;; Block standard typing keys (ASCII 32-126)
(dolist (char (number-sequence 32 126))
  (let ((key (char-to-string char)))
    (unless (lookup-key org-queue-mode-map (kbd key))
      (define-key org-queue-mode-map (kbd key) #'org-queue-ignore))))

;; Block common editing keys
(dolist (key-binding '("<backspace>" "<delete>" "<deletechar>"
                      "<return>" "RET" "DEL"))
  (unless (lookup-key org-queue-mode-map (kbd key-binding))
    (define-key org-queue-mode-map (kbd key-binding) #'org-queue-ignore)))

;; Block pasting and other input-adding commands
(dolist (input-key '("C-y" "C-d" "C-k" "C-o" "C-j" "C-m"))
  (define-key org-queue-mode-map (kbd input-key) #'org-queue-ignore))

;; Explicitly allow navigation keys and copy operations
(dolist (allowed-key '("<tab>" "TAB" "<up>" "<down>" "<left>" "<right>"
                      "<prior>" "<next>" "<home>" "<end>" 
                      "C-v" "M-v" "C-l" "C-n" "C-p"
                      "C-c" "M-w" "C-w"))  ;; allow copying and cutting
  (define-key org-queue-mode-map (kbd allowed-key) nil))

;; State Containers
(defvar org-queue--status-active nil
  "Global activation tracking")
(defvar org-queue--original-cursor nil
  "Persistent cursor state storage")

;; Mode Line Presentation Layer
(defface org-queue-global-lighter
  '((t :inherit font-lock-builtin-face
	 :height 0.85
	 :weight medium
	 :foreground "#B71C1C"
	 :background "#FFCDD2"))
  "Cross-theme compatible status indicator")

(defvar org-queue--lighter-display nil)
(setq-default global-mode-string 
  '(:eval (when org-queue-mode
	      (propertize "  ■ WORK" 'face 'org-queue-global-lighter))))

;; Temporal Constants
(defconst org-queue--idle-delay 3
  "Seconds before showing status reminder")
(defconst org-queue--blink-interval 0.7
  "Cursor blink rate in seconds")

(define-minor-mode org-queue-mode
  "Global minor mode for task queue management."
  :init-value nil
  :global t
  :keymap org-queue-mode-map
  :lighter " OrgQ"  ;; Retained original simpler indicator
  (if org-queue-mode
	(progn
	  (setq org-queue--status-active t
		org-queue--original-cursor cursor-type
		cursor-type '(box . 3)  ; Thicker box cursor
		blink-cursor-blinks 0
		blink-cursor-interval org-queue--blink-interval)
	  (add-hook 'post-command-hook #'org-queue--notify-presence)
	  (run-with-idle-timer org-queue--idle-delay nil
	    (lambda ()
	      (unless (active-minibuffer-window)
		(message "%s" (propertize "[Active] Task context engaged (e to exit)"
					 'face 'font-lock-comment-face))))))
    ;; Clean State Transition
    (setq org-queue--status-active nil
	    cursor-type org-queue--original-cursor
	    blink-cursor-blinks 40
	    blink-cursor-interval 0.5)
    (remove-hook 'post-command-hook #'org-queue--notify-presence)
    (message "%s" (propertize "[Idle] Context released" 
			      'face 'font-lock-comment-face))))

(defun org-queue--notify-presence ()
  "Managed presence indication system"
  (when (and org-queue-mode 
	       (not (active-minibuffer-window))
	       (not (minibufferp)))
    (force-mode-line-update)
    (unless cursor-in-non-selected-windows
	(setq cursor-in-non-selected-windows t))
    (run-with-idle-timer org-queue--idle-delay nil
	(lambda ()
	  (when org-queue-mode
	    (message "%s" (propertize "[Active] Maintained focus (press e to exit)"
				     'face 'font-lock-doc-face)))))))

;; The second argument, t, makes the timer repeat.
(run-with-idle-timer 0.847 t
  (lambda ()
    ;; Check if org-queue-mode is not currently enabled.
    (unless org-queue-mode
	;; If it's disabled, enable org-queue-mode.
	(org-queue-mode 1)
	;; Display a message to notify that org-queue-mode was activated.
	(message "org-queue-mode enabled due to inactivity."))))

(defun my-enable-org-queue-mode ()
  (interactive)
  (org-queue-mode 1))

;; Bind C-c q to enable org-queue-mode.
(global-set-key (kbd "C-c q") 'my-enable-org-queue-mode)

;; Bind <escape> to enable org-queue-mode
(global-set-key (kbd "<escape>") 'my-enable-org-queue-mode)

(require 'my-srs-integration)

(defun org-srs-entry-p (pos)
  "Determine if and where the Org entry at POS or its immediate parent contains
the specified log drawer (org-srs-log-drawer-name).

Returns:
- 'current : If the drawer is found directly under the current entry.
- 'parent  : If the drawer is found directly under the immediate parent entry 
            (and not under the current entry).
- nil      : If the drawer is not found in either location."
  (interactive (list (point)))

  ;; Ensure the required variable is defined and not empty
  (unless (boundp 'org-srs-log-drawer-name)
    (error "Variable 'org-srs-log-drawer-name' is not defined. Set it to your drawer name"))
  (when (string-empty-p org-srs-log-drawer-name)
    (error "Variable 'org-srs-log-drawer-name' is empty. Set it to your drawer name"))

  ;; Save current buffer state
  (save-excursion
    ;; Ensure we're at a heading
    (goto-char pos)
    (unless (or (org-at-heading-p) (org-back-to-heading t))
      (message "org-srs-entry-p: Not within an Org entry")
      (cl-return-from org-srs-entry-p nil))

    (let* ((drawer-regexp (concat "^[ \t]*:" 
                                 (regexp-quote org-srs-log-drawer-name)
                                 ":[ \t]*$"))
           (location nil)
           (current-heading-pos (point))
           (current-level (org-outline-level)))
      
      ;; Check current entry first
      (let ((next-heading-pos (save-excursion
                               (outline-next-heading)
                               (point))))
        (save-excursion
          (forward-line 1) ;; Move past the heading
          (when (re-search-forward drawer-regexp next-heading-pos t)
            (setq location 'current))))
      
      ;; If not found in current entry and not at top level, check parent
      (unless location
        (when (> current-level 1)
          (save-excursion
            (goto-char current-heading-pos)
            (when (org-up-heading-safe)
              (let ((parent-pos (point))
                    (next-heading-pos (save-excursion
                                       (outline-next-heading)
                                       (point))))
                (forward-line 1) ;; Move past the parent heading
                (when (re-search-forward drawer-regexp next-heading-pos t)
                  (setq location 'parent)))))))
      
      ;; Output debug message and return result
      (message "org-srs-entry-p: Result (for drawer '%s') = %s" 
               org-srs-log-drawer-name location)
      location)))

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

(defcustom my-random-schedule-default-months 3
  "Default number of months to schedule if none is specified."
  :type 'integer
  :group 'org-queue)

(defun my-round-to-decimals (number decimals)
  "Round NUMBER to DECIMALS decimal places."
  (/ (float (round (* number (expt 10 decimals))))
     (expt 10 decimals)))

(defun my-find-schedule-weight ()
  "Calculate schedule weight based on SCHEDULED date in org header.
			    Returns:
			    - 0 for past dates and today
			    - Number of months ahead (days/30.0) for future dates
			    - `my-random-schedule-default-months` if no SCHEDULED date exists."
  (let* ((scheduled-time (org-get-scheduled-time (point)))  ; Get the SCHEDULED time
	   (current-time (current-time))                      ; Get the current time
	   (days-difference
	    (when scheduled-time
	      ;; Calculate the difference in days, then convert to months
	      (/ (float (- (time-to-days scheduled-time)
			   (time-to-days current-time)))
		 30.0))))
    (cond
     ((null scheduled-time)
	;; If no SCHEDULED time, return the default months
	(or (bound-and-true-p my-random-schedule-default-months) 0))
     ((<= days-difference 0)
	;; If the difference is 0 or negative, return 0
	0)
     (t
	;; Otherwise, return the rounded difference in months to 2 decimal places
	(my-round-to-decimals days-difference 2)))))

(defcustom my-random-schedule-exponent 1
  "Exponent n controlling the bias of the scheduling distribution.
									    - n = 0: Uniform distribution (no bias).
									    - n = 1: Quadratic distribution (default).
									    - n = 2: Cubic distribution (stronger bias towards later dates)."
  :type 'integer
  :group 'org-queue)

(defun my-random-schedule (months &optional n)
  "Schedules an Org heading MONTHS months in the future using a mathematically elegant distribution.
If N is provided, use that as the exponent. If it's not provided, fallback to `my-random-schedule-exponent'.
Skips scheduling if the current heading or its parent has an SRS drawer."
  (when (and (not noninteractive)
             (eq major-mode 'org-mode))
    ;; Check if heading or parent has SRS drawer
    (let ((srs-result (org-srs-entry-p (point))))
      (if (or (eq srs-result 'current) (eq srs-result 'parent))
          ;; Skip scheduling if entry or parent has SRS drawer
          (message "Skipping scheduling for entry with SRS drawer")
        ;; Otherwise, proceed with normal scheduling
        (let* ((today (current-time))
               (total-days (* months 30))
               ;; If `n` is not passed in, use our existing defcustom value
               (n (or n my-random-schedule-exponent))
               (u (/ (float (random 1000000)) 1000000.0))
               (exponent (/ 1.0 (+ n 1)))  ; compute 1/(n+1)
               (x (expt u exponent))
               (days-ahead (floor (* total-days x)))
               (random-date (time-add today (days-to-time days-ahead))))
          (org-schedule nil (format-time-string "%Y-%m-%d" random-date)))))))

(defun my-random-schedule-command (&optional months)
  "Interactive command to schedule MONTHS months in the future.
				If MONTHS is not provided, uses the result of my-find-schedule-weight."
  (interactive
   (list (read-number
	    "Enter the upper month limit: "
	    (my-find-schedule-weight))))
  (save-excursion
    ;; Schedule the current heading
    (my-random-schedule (or months (my-find-schedule-weight)))))

(defun my-advance-schedule ()
  "Advance the current Org heading by a mathematically adjusted number of months.
Uses a function that decreases with increasing current schedule weight,
ensuring that tasks scheduled further in the future are advanced less.
Does not schedule tasks to dates before today.
Skips scheduling if the current heading or its parent is an SRS entry
(contains the org-srs-log-drawer-name drawer)."
  (interactive)
  (when (and (not noninteractive)
             (eq major-mode 'org-mode))
    ;; Check if this is an SRS entry
    (unless (org-srs-entry-p (point)) ; Only continue if NOT an SRS entry
      (let* ((e (exp 1))  ; e ≈ 2.71828
             ;; Get the current scheduled months ahead
             (current-weight (max 0 (my-find-schedule-weight)))  ; Ensure non-negative value
             ;; Calculate the adjustment using f(x) = x - 1 / ln(x + e)
             (adjusted-months (max 0 (- current-weight
                                        (/ 1 (log (+ current-weight e))))))
             ;; Generate a random value between current-weight and adjusted-months
             (min-months (min current-weight adjusted-months))
             (max-months (max current-weight adjusted-months))
             (random-months (random-float min-months max-months))
             ;; Convert random-months to days
             (adjusted-days (* random-months 30)))
        ;; Schedule the task to the adjusted date, ensuring it is not before today
        (org-schedule nil (format-time-string "%Y-%m-%d"
                                             (time-add (current-time)
                                                       (days-to-time adjusted-days))))))))

(defun my-postpone-schedule ()
  "Postpone the current Org heading by a mathematically adjusted number of months.
Calculates the postponement using a function that increases while its derivative decreases,
to ensure that tasks with larger weights are postponed by relatively smaller amounts.
Skip postponing if the current entry or its parent contains an SRS drawer."
  (interactive)
  (when (and (not noninteractive)
             (eq major-mode 'org-mode))
    ;; Check if this is an SRS entry (has SRS drawer in current or parent heading)
    (let ((srs-status (org-srs-entry-p (point))))
      ;; Only proceed if not an SRS entry (i.e., org-srs-entry-p returns nil)
      (unless srs-status
        (let* ((e (exp 1))  ; e ≈ 2.71828
               (current-weight (max 0 (my-find-schedule-weight)))  ; Ensure non-negative value
               ;; Adjusted months using f(x) = x + 1 / ln(x + e)
               (adjusted-months (+ current-weight
                                   (/ 1 (log (+ current-weight e)))))
               ;; Generate a random value between current-weight and adjusted-months
               (min-months (min current-weight adjusted-months))
               (max-months (max current-weight adjusted-months))
               (random-months (random-float min-months max-months))
               ;; Convert random-months to days
               (adjusted-days (* random-months 30))
               (now (current-time))
               (proposed-new-time (time-add now (days-to-time adjusted-days)))
               ;; Calculate tomorrow's midnight
               (now-decoded (decode-time now))
               (year (nth 5 now-decoded))
               (month (nth 4 now-decoded))
               (day (nth 3 now-decoded))
               (tomorrow-midnight (encode-time 0 0 0 (1+ day) month year))
               ;; Ensure new-time is at least tomorrow-midnight
               (new-time (if (time-less-p proposed-new-time tomorrow-midnight)
                             tomorrow-midnight
                           proposed-new-time)))
          ;; Schedule the task to the adjusted date
          (org-schedule nil (format-time-string "%Y-%m-%d" new-time)))))))

(defun my-custom-shuffle (list)
  "Fisher-Yates shuffle implementation for Emacs Lisp."
  (let ((vec (vconcat list)) (i (length list)))
    (while (> i 1)
	(let* ((j (random i))
	       (temp (aref vec (setq i (1- i)))))
	  (aset vec i (aref vec j))
	  (aset vec j temp)))
    (append vec nil)))

(defun my-auto-advance-schedules (&optional power)
  "Advance 2^POWER random tasks (default:64) with proper loop control."
  (interactive "P")
  (let* ((n (or power 6))
	   (limit (expt 2 n))
	   (candidates (org-map-entries #'point-marker nil 'agenda))
	   (shuffled (my-custom-shuffle candidates))
	   (total (length shuffled))
	   (processed 0)
	   (count 0))
    (save-some-buffers t)
    (catch 'break
	(dolist (m shuffled)
	  (when (>= count limit) (throw 'break nil))
	  (setq count (1+ count))
	  (org-with-point-at m
	    (when (my-advance-schedule)
	      (setq processed (1+ processed))))))
    (save-some-buffers t)
    (message "Advanced %d/%d (2^%d=%d)" processed total n limit)))

(defun my-auto-postpone-schedules (&optional power)
  "Postpone 2^POWER random tasks (default:64) with safe iteration."
  (interactive "P")
  (let* ((n (or power 6))
	   (limit (expt 2 n))
	   (candidates (org-map-entries #'point-marker nil 'agenda))
	   (shuffled (my-custom-shuffle candidates))
	   (total (length shuffled))
	   (processed 0)
	   (count 0))
    (save-excursion
	(save-some-buffers t)
	(catch 'break
	  (dolist (m shuffled)
	    (when (>= count limit) (throw 'break nil))
	    (setq count (1+ count))
	    (org-with-point-at m
	      (when (my-postpone-schedule)
		(setq processed (1+ processed))))))
	(save-some-buffers t)
	(message "Postponed %d/%d (2^%d=%d)" processed total n limit))))

(defun my-schedule-command (&optional months)
  "Interactive command that schedules MONTHS months in the future and prompts for priority."
  (interactive
   (list (read-number
	    "Enter the upper month limit: "
	    (my-find-schedule-weight))))
  ;; Schedule the current heading
  (my-random-schedule (or months (my-find-schedule-weight)))
  (my-ensure-priority-set))

(defun my-ensure-priorities-and-schedules-for-all-headings (&optional max-attempts)
  "Ensure priorities and schedules are set for all headings across Org agenda files.
Repeatedly processes headings until all have priorities and schedules, or max-attempts is reached.
MAX-ATTEMPTS: Maximum number of retry attempts (defaults to 15)."
  (interactive)
  (let ((max-attempts (or max-attempts 15))
        (attempt 0)
        (all-complete nil)
        (processed-headings (make-hash-table :test 'equal)))

    (while (and (not all-complete) (< attempt max-attempts))
      (setq attempt (1+ attempt))
      (save-some-buffers t)

      ;; First pass: Count total entries and incomplete entries
      (let ((total-entries 0)
            (incomplete-entries 0))
        (org-map-entries
         (lambda ()
           (setq total-entries (1+ total-entries))
           ;; First check if entry has missing priority or schedule
           (when (or (not (org-entry-get nil "PRIORITY"))
                     (string= (org-entry-get nil "PRIORITY") " ")
                     (not (org-entry-get nil "SCHEDULED")))
             ;; Only now check if it's an SRS entry (more expensive operation)
             (let* ((marker (point-marker))
                    (file (buffer-file-name))
                    (position (point))
                    (heading-id (concat file ":" (number-to-string position)))
                    (srs-status (or (gethash heading-id processed-headings)
                                    (puthash heading-id (org-srs-entry-p (point)) processed-headings))))
               
               ;; Count as incomplete based on SRS status:
               ;; - 'parent: skip both priority and schedule (never incomplete)
               ;; - 'current: only incomplete if missing priority (we skip scheduling)
               ;; - nil: incomplete if missing either priority or schedule
               (cond
                ((eq srs-status 'parent)
                 nil) ; Skip entirely
                ((eq srs-status 'current)
                 ;; Only check priority for 'current entries
                 (when (or (not (org-entry-get nil "PRIORITY"))
                           (string= (org-entry-get nil "PRIORITY") " "))
                   (setq incomplete-entries (1+ incomplete-entries))))
                (t
                 ;; For non-SRS entries, count if missing either
                 (setq incomplete-entries (1+ incomplete-entries)))))))
         nil 'agenda)

        ;; Process entries if there are incomplete ones
        (when (> incomplete-entries 0)
          (org-map-entries
           (lambda ()
             ;; First check if entry has missing priority or schedule
             (when (or (not (org-entry-get nil "PRIORITY"))
                       (string= (org-entry-get nil "PRIORITY") " ")
                       (not (org-entry-get nil "SCHEDULED")))
               ;; Only check SRS status if needed
               (let* ((file (buffer-file-name))
                      (position (point))
                      (heading-id (concat file ":" (number-to-string position)))
                      (srs-status (or (gethash heading-id processed-headings)
                                      (puthash heading-id (org-srs-entry-p (point)) processed-headings))))
                 
                 (cond
                  ;; For 'parent entries, skip entirely
                  ((eq srs-status 'parent)
                   nil)
                  
                  ;; For 'current entries, only set priority if needed
                  ((eq srs-status 'current)
                   (condition-case err
                       (let ((current-priority (org-entry-get nil "PRIORITY")))
                         (when (or (not current-priority) 
                                   (string= current-priority " "))
                           (my-ensure-priority-set)))
                     (error
                      (message "Error processing priority for 'current entry: %s" 
                               (error-message-string err)))))
                  
                  ;; For non-SRS entries, process both priority and schedule
                  (t
                   (condition-case err
                       (progn
                         ;; Ensure priority is set only if missing
                         (let ((current-priority (org-entry-get nil "PRIORITY")))
                           (when (or (not current-priority) 
                                     (string= current-priority " "))
                             (my-ensure-priority-set)))
                         ;; Ensure schedule is set only if missing
                         (unless (org-entry-get nil "SCHEDULED")
                           (my-random-schedule (my-find-schedule-weight) 0)))
                     (error
                      (message "Error processing entry: %s" 
                               (error-message-string err)))))))))
           nil 'agenda))

        ;; Set all-complete if no incomplete entries found
        (setq all-complete (zerop incomplete-entries)))

      (save-some-buffers t)

      (message "Attempt %d/%d completed. %s"
               attempt 
               max-attempts
               (if all-complete
                   "All entries processed successfully!"
                 "Some entries are still incomplete.")))

    (when (and (not all-complete) (>= attempt max-attempts))
      (message "Warning: Reached maximum attempts (%d). Some entries may still be incomplete." 
               max-attempts))))

(defun my-post-org-insert-heading (&rest _args)
    "Run after `org-insert-heading` to assign priority and schedule."
  
    (when (and (not noninteractive)
		 (eq major-mode 'org-mode)
		 (bound-and-true-p org-queue-mode))  ;; 🙌 Only trigger in org-queue-mode
	(my-random-schedule (my-find-schedule-weight) 0)
	(call-interactively #'my-set-priority-with-heuristics)
	(end-of-line)))

(defun my-is-overdue-task ()
  "Return non-nil if the current task is overdue."
  (let ((scheduled-time (org-get-scheduled-time nil)))
    (and scheduled-time
	   (< (time-to-days scheduled-time) (time-to-days (current-time))))))

(defun my-is-outstanding-task ()
  "Return non-nil if the current task is overdue or due today."
  (let ((scheduled-time (org-get-scheduled-time nil)))
    (and scheduled-time
	   (<= (time-to-days scheduled-time) (time-to-days (current-time))))))

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

(setq org-agenda-sorting-strategy
	'((agenda priority-down category-down)
	  (todo priority-down)
	  (tags priority-down)
	  (search category-keep)))

(defun my-get-raw-priority-value ()
  "Get the priority value of the current point without using task list."
  (let ((priority-str (org-entry-get nil "PRIORITY")))
    (if priority-str
	  (string-to-number priority-str)
	(+ org-priority-default
	   (random (+ 1 (- org-priority-lowest org-priority-default)))))))

(defun my-get-priority-value ()
  "Get the priority value of the current task from the task list."
  (let* ((marker (nth my-outstanding-tasks-index my-outstanding-tasks-list))
	   (priority-str (when marker
			   (org-with-point-at marker
			     (org-entry-get nil "PRIORITY")))))
    (if priority-str
	  (string-to-number priority-str)
	(+ org-priority-default
	   (random (+ 1 (- org-priority-lowest org-priority-default)))))))

;; Define a customizable variable for the base directory.
(defcustom org-queue-directory nil
  "Base directory for task files for Org Queue.
If nil, a safe default directory will be used and created automatically."
  :type 'directory
  :group 'org-queue)

;; Define a customizable variable for the cache file.
(defcustom my-outstanding-tasks-cache-file
  (expand-file-name "org-queue-outstanding-tasks.cache" cache-dir)
  "File path to store the cached outstanding tasks list along with its date stamp.
  By default, this file will be inside the cache directory (cache-dir)."
  :type 'string
  :group 'org-queue)

;; Variable to hold the list of outstanding tasks.
(defvar my-outstanding-tasks-list nil
  "List of outstanding tasks, sorted by priority.")

(defun my-save-outstanding-tasks-to-file ()
  "Save `my-outstanding-tasks-list` to `my-outstanding-tasks-cache-file` with a date stamp.
Each task is stored as a cons cell (FILE-PATH . POSITION). When FILE-PATH is inside
`org-queue-directory`, the path is saved relative to that directory."
  (with-temp-file my-outstanding-tasks-cache-file
    (let* ((today (format-time-string "%Y-%m-%d"))
	     (tasks-saved
	      (delq nil
		    (mapcar
		     (lambda (marker)
		       (when (and (marker-buffer marker)
				  (buffer-file-name (marker-buffer marker)))
			 (with-current-buffer (marker-buffer marker)
			   (let* ((full (file-truename (buffer-file-name)))
				  (path (if (file-in-directory-p full org-queue-directory)
					    (file-relative-name full org-queue-directory)
					  full)))
			     (cons path (marker-position marker))))))
		     my-outstanding-tasks-list))))
	(insert (prin1-to-string (list :date today :tasks tasks-saved))))))

(defun my-load-outstanding-tasks-from-file ()
  "Load cached tasks from `my-outstanding-tasks-cache-file`.
If the saved date matches today, each stored (FILE-PATH . POSITION) pair is converted back
to a marker and stored in `my-outstanding-tasks-list'. If the stored file path is relative,
it is expanded using `org-queue-directory'."
  (if (file-exists-p my-outstanding-tasks-cache-file)
	(let* ((data (with-temp-buffer
		       (insert-file-contents my-outstanding-tasks-cache-file)
		       (read (buffer-string))))
	       (saved-date (plist-get data :date))
	       (saved-tasks (plist-get data :tasks))
	       (today (format-time-string "%Y-%m-%d")))
	  (if (string= saved-date today)
	      (progn
		(setq my-outstanding-tasks-list
		      (mapcar
		       (lambda (task)
			 (let* ((stored-path (car task))
				(abs-path (if (or (file-name-absolute-p stored-path)
						  (string-match-p "^[A-Za-z]:[/\\\\]" stored-path))
					      stored-path
					    (expand-file-name stored-path org-queue-directory))))
			   (with-current-buffer (find-file-noselect abs-path)
			     (save-excursion
			       (goto-char (cdr task))
			       (point-marker)))))
		       saved-tasks))
		t)
	    nil))
    nil))

(defvar my-outstanding-tasks-index 0
  "Current index in the outstanding tasks list.")

(defun my-get-outstanding-tasks ()
  "Populate `my-outstanding-tasks-list` with outstanding tasks, sorted by priority.
   Skips tasks that are already part of an SRS system (where org-srs-entry-p returns non-nil)."
  (setq my-outstanding-tasks-list nil)
  (org-map-entries
   (lambda ()
     (when (and (my-is-outstanding-task)
                (not (org-srs-entry-p (point))))
       (let* ((priority (my-get-raw-priority-value))
              (marker (point-marker)))
         (push (cons priority marker) my-outstanding-tasks-list))))
   nil
   'agenda)
  (setq my-outstanding-tasks-list
        (sort my-outstanding-tasks-list (lambda (a b) (< (car a) (car b)))))
  (setq my-outstanding-tasks-list (mapcar #'cdr my-outstanding-tasks-list))
  (setq my-outstanding-tasks-index 0))

(defun my-auto-postpone-overdue-tasks ()
  "Auto-postpone all overdue tasks using linear interpolation for priorities.
							 If a task's priority is not set, use `org-priority-default` to `org-priority-lowest`
							 as the basis for linear interpolation. The calculated `months` is passed to
							 `my-random-schedule` for randomness. Save all modified files before and after processing."
  (interactive)
  (my-reset-outstanding-tasks-index)
  ;; Save all modified buffers before processing
  (save-some-buffers t) ;; Save all modified buffers without prompting
  (let* ((highest-priority org-priority-highest)  ; Highest priority value (e.g., 1)
	   (lowest-priority org-priority-lowest)    ; Lowest priority value (e.g., 64)
	   (default-priority org-priority-default)  ; Default priority value (e.g., 32)
	   (max-months (my-find-schedule-weight))) ; Max months for scheduling
    ;; Iterate over all headings in the agenda files
    (org-map-entries
     (lambda ()
	 (when (my-is-overdue-task)
	   ;; Ensure priority is set
	   (my-ensure-priority-set)
	   ;; Get the priority value or fallback to the default range
	   (let* ((priority-string (org-entry-get nil "PRIORITY"))
		  (priority (if priority-string
				(string-to-number priority-string)
			      ;; Fallback: Use default to lowest priority range
			      (random (+ 1 (- lowest-priority default-priority)))))
		  ;; Linearly interpolate months based on priority
		  (months (* max-months
			     (/ (float (- priority highest-priority))
				(float (- lowest-priority highest-priority))))))
	     ;; Use `my-random-schedule` to schedule the task
	     (my-random-schedule months)
	     (message "Task postponed with priority %d (months: %.2f)." priority months))))
     nil 'agenda))
  ;; Save all modified buffers after processing
  (save-some-buffers t)) ;; Save all modified buffers without prompting

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
		current-max))))

(defun my-postpone-consecutive-same-file-tasks ()
  "Postpone consecutive tasks from the same file, keeping only the first.
Saves buffers and regenerates the task list for consistency."
  (interactive)
  (my-reset-outstanding-tasks-index)
  (when my-outstanding-tasks-list
	(let ((prev-file nil)
	      (modified-buffers nil)
	      (original-count (length my-outstanding-tasks-list)))
	  ;; Iterate through tasks in order
	  (dolist (marker my-outstanding-tasks-list)
	    (let* ((buffer (marker-buffer marker))
		   (file (when (buffer-live-p buffer)
			   (expand-file-name (buffer-file-name buffer)))))
	      (if (and file (equal file prev-file))
		  (progn
		    (org-with-point-at marker
		      (my-postpone-schedule)
		      (add-to-list 'modified-buffers buffer))
		    (message "Postponed: %s (File: %s)" 
			     (org-get-heading t t)
			     (file-name-nondirectory file)))
		(setq prev-file file))))
	  ;; Save modified buffers to disk
	  (dolist (buf modified-buffers)
	    (when (buffer-live-p buf)
	      (with-current-buffer buf
		(save-buffer))))
	  ;; Regenerate task list if changes occurred
	  (when modified-buffers
	    (my-get-outstanding-tasks)
	    (message "%d consecutive task(s) postponed. List regenerated." 
		     (- original-count (length my-outstanding-tasks-list))))))
  (my-reset-outstanding-tasks-index))

(require 'pulse)

  ;; Display a message in a full-screen error window.
  (defun display-fullscreen-error (message)
    "Display MESSAGE in a full-screen buffer named *Anki Error*."
    (let ((buf (get-buffer-create "*Anki Error*")))
	(with-current-buffer buf
	  (read-only-mode -1)      ; Remove read-only so we can update the buffer.
	  (erase-buffer)
	  (insert message)
	  (read-only-mode 1)
	  (goto-char (point-min)))
	(delete-other-windows)      ; Make the error buffer full screen.
	(switch-to-buffer buf)))

  ;; Display learning instructions in a full-screen info window.
  (defun display-fullscreen-info ()
    "Display optimal topic-to-item ratio learning instructions in a full-screen window."
    (let ((buf (get-buffer-create "*Anki Learning Info*"))
	    (content (concat
  "* Optimal Topic-to-Item Ratio in Learning\n"
  "================================================\n\n"
  "                [ New Topic ]\n"
  "                        |\n"
  "  --------------┼---------------------------------------------\n"
  "                        |\n"
  "  [ Item Review ] - [ Item Review ] - [ Item Review ] - [ Item Review ] - ...\n\n"
  "(1 New Topic balanced with 4 or more Item Reviews)\n\n"
  "** Key Points\n"
  "- *Aim for a 1:4 or lower ratio*: For each new topic introduced, aim to review at least four items or more.\n"
  "- Maintaining this balance helps prevent overload, supporting effective retention and steady learning progress.\n\n"
  "** Open *Anki*\n"
  "Click on *Anki* to launch it directly and start practicing this balanced approach in your reviews.\n")))
	(with-current-buffer buf
	  (read-only-mode -1)
	  (erase-buffer)
	  (insert content)
	  (org-mode)
	  (org-fold-show-all) 
	  (read-only-mode 1)
	  (goto-char (point-min)))
	(delete-other-windows)      ; Make the info buffer take up the whole frame.
	(switch-to-buffer buf)))

;; Define a customizable variable to control whether Anki should actually launch
(defcustom my-actually-launch-anki t
  "When non-nil, actually launch the Anki executable.
When nil, skip Anki execution but still display the learning instructions."
  :type 'boolean
  :group 'my-learning-tools)

(defun my-launch-anki ()
  "Launch the Anki application on Windows if it exists and if enabled.
Regardless of whether Anki launches successfully, is disabled, or an error occurs,
always display the full-screen learning instructions."
  (when (and (eq system-type 'windows-nt) my-actually-launch-anki)
    (let* ((user-profile (getenv "USERPROFILE"))
	     (anki-path (expand-file-name "AppData/Local/Programs/Anki/anki.exe" user-profile)))
	(if (file-exists-p anki-path)
	    (condition-case err
		(start-process "Anki" nil anki-path)
	      (error (display-fullscreen-error
		      (format "Failed to launch Anki: %s" (error-message-string err)))))
	  (display-fullscreen-error
	   (format "Anki executable not found at: %s" anki-path)))))

  ;; Ensure the list exists, refresh if empty.
  (unless my-outstanding-tasks-list (my-get-outstanding-tasks))
  (if my-outstanding-tasks-list
	(let* ((total (length my-outstanding-tasks-list))
	       ;; Wrap index using modulo (supports negative numbers)
	       (new-index (mod (- my-outstanding-tasks-index 2) total))
	       (adjusted-index (if (>= new-index 0)
				  new-index
				(+ new-index total))))
	  ;; Update index and counter
	  (setq my-outstanding-tasks-index (1+ adjusted-index)
		my-anki-task-counter (1- my-anki-task-counter))
	  ;; Display the full-screen learning instructions
	  (display-fullscreen-info))))

(defcustom my-anki-task-ratio 1
  "Ratio of Anki launches to tasks displayed. Default is 1:1 (Anki launched every task).
									       Should be a positive integer."
  :type 'integer
  :group 'org-queue)

(defvar my-anki-task-counter 0
  "Counter of tasks displayed since last Anki launch.")

(defun my-set-anki-task-ratio (ratio)
  "Set the ratio of Anki launches to tasks displayed.
									       For example, if RATIO is 3, Anki will be launched once every 3 tasks. RATIO should be a positive integer."
  (interactive "nSet Anki:Task ratio (positive integer): ")
  (setq my-anki-task-ratio (max 1 ratio))
  (setq my-anki-ratio-interpolation nil)
  ;; Reset the counter whenever the ratio is changed
  (setq my-anki-task-counter 0)
  (message "Anki will be launched once every %d task(s)." my-anki-task-ratio))

(defcustom my-anki-ratio-interpolation t
  "Whether to use priority-based linear interpolation for Anki task ratio.
			If nil, uses fixed ratio specified by my-anki-task-ratio."
  :type 'boolean
  :group 'org-queue)

(defun my-calculate-interpolated-anki-ratio (priority)
  "Calculate interpolated Anki ratio based on priority."
  (if (not my-anki-ratio-interpolation)
	my-anki-task-ratio
    (let* ((highest-priority org-priority-highest)    ; e.g., 1
	     (lowest-priority org-priority-lowest)      ; e.g., 64
	     (max-ratio my-anki-task-ratio)            ; e.g., 8
	     (min-ratio 1)
	     ;; Linear interpolation
	     (ratio (* (+ min-ratio
			  (* (- max-ratio min-ratio)
			     (/ (float (- lowest-priority priority))
				(- lowest-priority highest-priority))))
		       1.0)))
	(max 1 (round ratio)))))

(defun my-maybe-launch-anki ()
  "Launch Anki according to priority-based interpolated ratio."
  (let* ((current-priority (my-get-priority-value))
	   (interpolated-ratio (my-calculate-interpolated-anki-ratio current-priority)))
    (when (>= my-anki-task-counter interpolated-ratio)
	(setq my-anki-task-counter 0)
	(my-launch-anki))))

(defun my-pulse-highlight-current-line (&optional time)
  "Temporarily pulse-highlight the current line.

Optional argument TIME specifies the delay between pulse iterations in seconds.
Defaults to 0.2 seconds."
  (let ((pulse-iterations 3)
	  (pulse-delay (or time 0.2)))
    (pulse-momentary-highlight-one-line (point))))

(defun widen-and-recenter ()
  "Widen buffer, reset folding, show top-level children, and recenter point.

This function widens the buffer to remove narrowing, resets the global
folding state with `org-overview', expands the top-level headings with
`org-fold-show-children', then returns point to its original position,
revealing it clearly at the center of the screen."
  (interactive)
  (let ((marker (point-marker))) ;; Store original position
    (widen)                      ;; Remove narrowing
    (ignore-errors (outline-up-heading 1)) ;; Move up heading (safely handle errors)
    (org-overview)               ;; Collapse all headings to overview state
    (org-fold-show-children)     ;; Expand top-level headings only
    (goto-char marker)           ;; Return to original position
    (org-reveal t)               ;; Fully reveal original point
    (org-show-children)          ;; Expand current subtree's direct children
    (recenter)))                 ;; Center point visually

(defun org-show-parent-heading-cleanly ()
  "Move up to the parent heading, widen the buffer, and then reveal the parent heading along with its children."
  (interactive)
  (widen-and-recenter)
  (ignore-errors (outline-up-heading 1)) ;; Move up heading (safely handle errors)
  (org-narrow-to-subtree))

(defun my-show-next-outstanding-task ()
  "Show the next outstanding task in priority order.
If the list is exhausted, it refreshes the list."
  (interactive)
  (unless (and my-outstanding-tasks-list
               (< my-outstanding-tasks-index (length my-outstanding-tasks-list)))
    (my-get-outstanding-tasks))
  (if (and my-outstanding-tasks-list
           (< my-outstanding-tasks-index (length my-outstanding-tasks-list)))
      (let ((marker (nth my-outstanding-tasks-index my-outstanding-tasks-list)))
        (widen-and-recenter)
        (switch-to-buffer (marker-buffer marker))
        (revert-buffer t t t)
        (widen-and-recenter)
        ;; Unfold all content and then perform two global cycles to refold.
        (org-fold-show-all)
        (org-global-cycle)
        (org-global-cycle)
        (goto-char (marker-position marker))
        ;; Ensure the entire entry is visible
        (org-show-entry)
        (recenter)
        (org-narrow-to-subtree)
        (org-overview)
        (org-reveal t)
        (org-show-entry)
        (show-children)
        ;; Increment the index after showing the task
        (setq my-outstanding-tasks-index (1+ my-outstanding-tasks-index))
        (setq my-anki-task-counter (1+ my-anki-task-counter))
        ;; Launch Anki according to the user-defined ratio
        (my-maybe-launch-anki)
        
        ;; First quit any existing SRS session
        (my-srs-quit-reviews)
        
        ;; Only try to start reviews if not exhausted
        (unless my-srs-reviews-exhausted
          (my-srs-start-reviews)))
    (message "No more outstanding tasks.")))

(defun my-show-current-outstanding-task ()
  "Show the current outstanding task, or call my-show-next-outstanding-task if no valid task exists."
  (interactive)
  (if (and my-outstanding-tasks-list
	     (> my-outstanding-tasks-index 0)
	     (<= my-outstanding-tasks-index (length my-outstanding-tasks-list)))
	(let ((marker (nth (1- my-outstanding-tasks-index) my-outstanding-tasks-list)))
	  (widen-and-recenter)
	  (switch-to-buffer (marker-buffer marker))
	  (revert-buffer t t t)
	  (widen-and-recenter)
	  ;; Unfold all content and then perform two global cycles to refold.
	  (org-fold-show-all)
	  (org-global-cycle)
	  (org-global-cycle)
	  (goto-char (marker-position marker))
	  ;; Ensure the entire entry is visible
	  (org-show-entry)
	  (recenter)
	  (org-narrow-to-subtree)
	  (org-overview)
	  (org-reveal t)
	  (org-show-entry)
	  (show-children)
	  ;; Highlight the entry temporarily
	  (my-pulse-highlight-current-line))
    ;; If no current outstanding task, call my-show-next-outstanding-task to move forward.
    (my-show-next-outstanding-task)))

(defun my-show-previous-outstanding-task ()
  "Show the previous outstanding task in priority order, cycling if needed."
  (interactive)
  ;; Ensure the list exists, refresh if empty.
  (unless my-outstanding-tasks-list (my-get-outstanding-tasks))
  (if my-outstanding-tasks-list
	(let* ((total (length my-outstanding-tasks-list))
	       ;; Wrap index using modulo (supports negative numbers)
	       (new-index (mod (- my-outstanding-tasks-index 2) total))
	       (adjusted-index (if (>= new-index 0) new-index (+ new-index total))))
	  ;; Update index and counter
	  (setq my-outstanding-tasks-index (1+ adjusted-index)
		my-anki-task-counter (max (1- my-anki-task-counter) 0))  ; Prevent negative
	  ;; Display
	  (let ((marker (nth adjusted-index my-outstanding-tasks-list)))
	    (widen-and-recenter)
	    (switch-to-buffer (marker-buffer marker))
	    (revert-buffer t t t)
	    (widen-and-recenter)
	    ;; Unfold all content and then perform two global cycles to refold.
	    (org-fold-show-all)
	    (org-global-cycle)
	    (org-global-cycle)
	    (goto-char (marker-position marker))
	    ;; Ensure the entire entry is visible
	    (org-show-entry)
	    (recenter)
	    (org-narrow-to-subtree)
	    (org-overview)
	    (org-reveal t)
	    (org-show-entry)
	    (show-children)))
    (message "No outstanding tasks to navigate.")))

(defun my-reset-outstanding-tasks-index ()
  "Reset the outstanding tasks index to start from the first task."
  (interactive)
  (my-get-outstanding-tasks)
  (setq my-outstanding-tasks-index 0)
  (setq my-anki-task-counter 0)
  (my-save-outstanding-tasks-to-file)
  (message "Outstanding tasks index reset."))

(defun my-reset-and-show-current-outstanding-task ()
  "Reset the outstanding tasks index and then show the current outstanding task."
  (interactive)  ;; Allows the function to be executed via M-x in Emacs
  (my-launch-anki)
  (my-reset-outstanding-tasks-index)  ;; Call function to reset tasks index
  (my-show-current-outstanding-task))  ;; Call function to show the first/current task

;; block needs to be executed. If a valid cache exists (i.e. saved today),
;; then skip the following block; otherwise, run it and update the cache.
(defun my-auto-task-setup ()
  "Initialize and set up automatic task management processes upon Emacs startup.
If an outstanding tasks cache exists from today, skip running the full maintenance block.
Otherwise, run the maintenance operations and then update the cache."
  (condition-case err
	(progn
	  (if (my-load-outstanding-tasks-from-file)
	      (message "Loaded outstanding tasks cache for today. Skipping auto setup maintenance block.")
	    (progn
	      (message "No valid cache for today found. Running full auto-setup maintenance block.")
	      (when (require 'org-roam nil t)
		;; Enable org-roam-db-autosync-mode
		(org-roam-db-autosync-mode)
		;; Update org-id locations for agenda files
		(org-id-update-id-locations (org-agenda-files)))
	      ;; Maintenance Block: run these only once per day.
	      (my-ensure-priorities-and-schedules-for-all-headings)
	      (my-auto-advance-schedules 8)
	      (my-auto-postpone-overdue-tasks)
	      (my-postpone-duplicate-priority-tasks)
	      (my-enforce-priority-constraints)
	      (my-ensure-priorities-and-schedules-for-all-headings)
	      (my-postpone-consecutive-same-file-tasks)
	      ;; After processing, save the current outstanding tasks list to cache.
	      (my-save-outstanding-tasks-to-file)))
	  ;; Regardless of whether the maintenance block ran, schedule the display of the current outstanding task.
	  (run-at-time "0.3 sec" nil 'my-show-current-outstanding-task)
	  (message "Automatic task setup completed successfully.")
	  (org-queue-mode 1))
    (error
     (message "Error during automatic task setup: %s" (error-message-string err)))))

(add-hook 'emacs-startup-hook #'my-auto-task-setup 100)

(provide 'org-queue)
