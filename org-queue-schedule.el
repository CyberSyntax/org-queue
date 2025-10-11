;;; org-queue-schedule.el --- Scheduling functions for org-queue -*- lexical-binding: t -*-

;;; Code:

(require 'org-queue-config)
(require 'org-queue-utils)
(require 'org-queue-srs-bridge)
(require 'org-queue-tasks)

(defcustom my-random-schedule-default-months 3
  "Default number of months to schedule if none is specified."
  :type 'integer
  :group 'org-queue)

(defcustom my-random-schedule-exponent 1
  "Exponent n controlling the bias of the scheduling distribution.
									    - n = 0: Uniform distribution (no bias).
									    - n = 1: Quadratic distribution (default).
									    - n = 2: Cubic distribution (stronger bias towards later dates)."
  :type 'integer
  :group 'org-queue)

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

(defun my-random-schedule (months &optional n)
  "Schedules an Org heading MONTHS months in the future using a mathematically elegant distribution.
Enhanced with priority-based bias while maintaining existing power-law distribution.
- Existing logic: Uses power-law distribution with exponent n
- Priority enhancement: High priority tasks get bias toward earlier dates
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
               
               ;; Get priority for bias calculation
               (priority-str (org-entry-get nil "PRIORITY"))
               (priority (if priority-str 
                            (string-to-number priority-str)
                          org-priority-default))
               
               ;; Priority-based bias: high priority increases exponent for earlier bias
               (priority-ratio (/ (float (- org-priority-lowest priority))
                                 (float (- org-priority-lowest org-priority-highest))))
               (priority-bias (+ 1.0 (* 2.0 priority-ratio)))  ; Range: 1.0 to 3.0
               
               ;; EXISTING mathematical logic with priority enhancement
               ;; If `n` is not passed in, use our existing defcustom value
               (base-n (or n my-random-schedule-exponent))
               (enhanced-n (* base-n priority-bias))  ; Priority enhances the exponent
               
               (u (/ (float (random 1000000)) 1000000.0))
               (exponent (/ 1.0 (+ enhanced-n 1)))  ; compute 1/(enhanced_n+1)
               (x (expt u exponent))
               (days-ahead (floor (* total-days x)))
               (random-date (time-add today (days-to-time days-ahead))))
          (org-schedule nil (format-time-string "%Y-%m-%d" random-date))
          (message "Scheduled: Priority %d (bias %.2fx) → %d days" 
                   priority priority-bias days-ahead))))))

(defun my-random-schedule-command (&optional months)
  "Interactive command to schedule MONTHS months in the future.
				If MONTHS is not provided, uses the result of my-find-schedule-weight."
  (interactive
   (list (read-number
	    "Enter the upper month limit: "
	    (my-find-schedule-weight))))
  (save-excursion
    ;; Schedule the current heading
    (my-random-schedule (or months (my-find-schedule-weight))))
  (save-buffer))

(defun my-advance-schedule ()
  "Advance the current Org heading by a mathematically adjusted number of months.
Uses priority-based factor multiplied with existing mathematical logic.
Does not schedule tasks to dates before today.
Skips scheduling if the current heading or its parent is an SRS entry."
  (interactive)
  (when (and (not noninteractive)
             (eq major-mode 'org-mode))
    (unless (org-srs-entry-p (point)) ; Only continue if NOT an SRS entry
      (let* ((orig-buf (current-buffer))
             (e (exp 1))
             (current-weight (max 0 (my-find-schedule-weight)))
             (priority-str (org-entry-get nil "PRIORITY"))
             (priority (if priority-str 
                           (string-to-number priority-str)
                         org-priority-default))
             (priority-ratio (/ (float (- org-priority-lowest priority))
                                (float (- org-priority-lowest org-priority-highest))))
             (priority-factor (+ 0.1 (* 0.9 priority-ratio)))
             (base-adjusted-months (max 0 (- current-weight
                                             (/ 1 (log (+ current-weight e))))))
             (priority-adjusted-months (* base-adjusted-months priority-factor))
             (min-months (min current-weight priority-adjusted-months))
             (max-months (max current-weight priority-adjusted-months))
             (random-months (random-float min-months max-months))
             (adjusted-days (* random-months 30.4375)))
        (org-schedule nil (format-time-string "%Y-%m-%d"
                                              (time-add (current-time)
                                                        (days-to-time adjusted-days))))
        (message "Advanced: Priority %d (factor %.2f) → %.2f months" 
                 priority priority-factor random-months)
        (save-buffer)
        (ignore-errors (org-queue--micro-update-current! 'advance))))))

(defun my-postpone-schedule ()
  "Postpone the current Org heading by a mathematically adjusted number of months.
Uses priority-based factor multiplied with existing mathematical logic.
Skip postponing if the current entry or its parent contains an SRS drawer."
  (interactive)
  (when (and (not noninteractive)
             (eq major-mode 'org-mode))
    (let ((srs-status (org-srs-entry-p (point))))
      (unless srs-status
        (let* ((orig-buf (current-buffer))
               (e (exp 1))
               (current-weight (max 0 (my-find-schedule-weight)))
               (is-overdue (my-is-overdue-task))
               (priority-str (org-entry-get nil "PRIORITY"))
               (priority (if priority-str 
                             (string-to-number priority-str)
                           org-priority-default))
               (priority-ratio (/ (float (- priority org-priority-highest))
                                  (float (- org-priority-lowest org-priority-highest))))
               (priority-factor (+ 0.1 (* 0.9 priority-ratio)))
               (base-adjusted-months (+ current-weight
                                        (/ 1 (log (+ current-weight e)))))
               (priority-adjusted-months (* base-adjusted-months priority-factor))
               (min-months (min current-weight priority-adjusted-months))
               (max-months (max current-weight priority-adjusted-months))
               (random-months (random-float min-months max-months))
               (adjusted-days (* random-months 30.4375))
               (now (current-time))
               (proposed-new-time (time-add now (days-to-time adjusted-days)))
               (minimum-time (if is-overdue
                                 now
                               (let* ((now-decoded (decode-time now))
                                      (year (nth 5 now-decoded))
                                      (month (nth 4 now-decoded))
                                      (day (nth 3 now-decoded)))
                                 (encode-time 0 0 0 (1+ day) month year))))
               (new-time (if (time-less-p proposed-new-time minimum-time)
                             minimum-time
                           proposed-new-time)))
          (org-schedule nil (format-time-string "%Y-%m-%d" new-time))
          (message "Postponed: Priority %d (factor %.2f) → %.2f months%s" 
                   priority priority-factor random-months
                   (if is-overdue " (overdue→today allowed)" ""))
          (save-buffer)
          (ignore-errors (org-queue--micro-update-current! 'postpone)))))))

(defun my-auto-advance-schedules (&optional power)
  "Advance 2^POWER random tasks (default: 64) across org-queue files."
  (interactive "P")
  (let* ((n (or power 6))
         (limit (expt 2 n))
         (candidates (org-queue-collect-markers))
         (shuffled (my-custom-shuffle candidates))
         (total (length shuffled))
         (processed 0)
         (count 0))
    (save-some-buffers t)
    (let ((org-queue--suppress-ui t))
      (catch 'break
        (dolist (m shuffled)
          (when (>= count limit) (throw 'break nil))
          (setq count (1+ count))
          (org-with-point-at m
            (my-advance-schedule)
            (setq processed (1+ processed))))))
    (save-some-buffers t)
    (message "Advanced %d/%d (2^%d=%d)" processed total n limit)))

(defun my-auto-postpone-schedules (&optional power)
  "Postpone 2^POWER random tasks (default: 64) across org-queue files."
  (interactive "P")
  (let* ((n (or power 6))
         (limit (expt 2 n))
         (candidates (org-queue-collect-markers))
         (shuffled (my-custom-shuffle candidates))
         (total (length shuffled))
         (processed 0)
         (count 0))
    (save-some-buffers t)
    (let ((org-queue--suppress-ui t))
      (catch 'break
        (dolist (m shuffled)
          (when (>= count limit) (throw 'break nil))
          (setq count (1+ count))
          (org-with-point-at m
            (my-postpone-schedule)
            (setq processed (1+ processed))))))
    (save-some-buffers t)
    (message "Postponed %d/%d (2^%d=%d)" processed total n limit)))

(defun my-schedule-command (&optional months)
  "Interactive command that schedules MONTHS months in the future and prompts for priority."
  (interactive
   (list (read-number
          "Enter the upper month limit: "
          (my-find-schedule-weight))))
  (let ((orig-buf (current-buffer)))
    (my-random-schedule (or months (my-find-schedule-weight)))
    (my-set-priority-with-heuristics)
    (my-ensure-priority-set)
    (save-buffer)
    (ignore-errors (org-queue--micro-update-current! 'schedule))))

(defun org-queue-stamp-last-repeat-current ()
  "Stamp :LAST_REPEAT: on the current heading only if it’s a pure non-SRS entry.
Returns a status symbol: :stamped, :skipped-not-org, or :skipped-srs.
Never signals on those skip cases."
  (interactive)
  (if (not (derived-mode-p 'org-mode))
      (progn (message "Not in an org-mode buffer; skipping LAST_REPEAT stamp")
             :skipped-not-org)
    (save-excursion
      (org-back-to-heading t)
      ;; Disallow when SRS-related (current or parent)
      (let ((where (org-srs-entry-p (point))))
        (when where
          (message "SRS-related entry (%s): LAST_REPEAT stamping is disabled here"
                   (symbol-name where))
          (cl-return-from org-queue-stamp-last-repeat-current :skipped-srs)))
      ;; Stamp LAST_REPEAT on this heading
      (let* ((now     (current-time))
             (now-str (format-time-string "[%Y-%m-%d %a %H:%M]" now)))
        (org-entry-put nil "LAST_REPEAT" now-str)
        (let ((verify (org-entry-get nil "LAST_REPEAT")))
          (unless (and verify (string= (string-trim verify) now-str))
            (org-set-property "LAST_REPEAT" now-str)))
        (message "Stamped LAST_REPEAT %s on current heading" now-str)
        (save-buffer)
        :stamped))))

(defun my-ensure-priorities-and-schedules-for-all-headings (&optional max-attempts)
  "Ensure priorities and schedules are set for all headings across Org agenda files.
Repeatedly processes headings until all have priorities and schedules, or max-attempts is reached.
Skip DONE tasks entirely.
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
        (org-queue-map-entries
         (lambda ()
           (setq total-entries (1+ total-entries))
           ;; Skip DONE tasks entirely
           (unless (my-is-done-task)
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
                   (setq incomplete-entries (1+ incomplete-entries))))))))
         nil)

        ;; Process entries if there are incomplete ones
        (when (> incomplete-entries 0)
          (org-queue-map-entries
           (lambda ()
             ;; Skip DONE tasks entirely
             (unless (my-is-done-task)
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
                                 (error-message-string err))))))))))
           nil))

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

(defun org-queue-stamp-and-show-top ()
  "Stamp LAST_REPEAT on current; micro-update that item; then show top."
  (interactive)
  (let ((status (ignore-errors (org-queue-stamp-last-repeat-current))))
    (ignore-errors (org-queue--micro-update-current!
                    (if (eq status :stamped) 'stamp 'skip))))
  (org-queue-show-top t))

(provide 'org-queue-schedule)
;;; org-queue-schedule.el ends here
