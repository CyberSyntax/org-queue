;; Ensure Org Agenda and cl-lib are loaded
(require 'org-agenda)
(require 'cl-lib)  ;; Required for cl-find-if and cl-remove-if-not

;; Ensure the random number generator is seeded once
(random t)

;; Set extended numerical priority range
(setq org-priority-highest 1)
(setq org-priority-default 32)
(setq org-priority-lowest 64)

(defcustom my-priority-ranges
  '((0 . (1 . 2))
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

(defun my-set-priority-with-heuristics (&optional specific-range)
  "Set a random priority within a user-defined heuristic range.

If called interactively:
  - If SPECIFIC-RANGE is provided, use that range identifier without prompting.
  - If SPECIFIC-RANGE is not provided, prompt the user to select a priority range,
    defaulting to the current heading's range or 9.

If called programmatically (non-interactively):
  - If SPECIFIC-RANGE is provided, use that range identifier without prompting.
  - If SPECIFIC-RANGE is not provided, use the current heading's range or 9 without prompting."
  (interactive)
  (let* ((priority-ranges my-priority-ranges)
         (range
          (cond
           ;; If specific-range is provided, use it directly
           (specific-range
            (cdr (assoc specific-range priority-ranges)))

           ;; If called interactively without specific-range, prompt the user
           ((and (called-interactively-p 'any))
            (let* ((default-range (or (my-get-current-priority-range) 9))
                   (user-choice (read-number
                                 "Select a priority range (0-9): "
                                 default-range)))
              (cdr (assoc user-choice priority-ranges))))

           ;; If called programmatically without specific-range, use default
           (t
            (cdr (assoc (or (my-get-current-priority-range) 9) priority-ranges))))))
    (if range
        (let* ((min-priority (car range))
               (max-priority (cdr range))
               (random-priority (+ min-priority
                                   (random (1+ (- max-priority min-priority))))))
          (org-priority random-priority)
          (message "Priority set to: %d" random-priority))
      (message "Invalid range."))))

(defun my-ensure-priority-set ()
  "Ensure the current heading has a priority set.
If PRIORITY is not set, assign one within the appropriate range.
If PRIORITY is set, reassign a priority within the same range."
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
                  (message "Priority reassigned within range %d." current-range))
              (message "Current priority %d does not fall within any defined range."
                       priority-value)))
        ;; PRIORITY is not set; assign a random priority within appropriate ranges
        (let* ((matching-ranges
                (cl-remove-if-not
                 (lambda (range)
                   (let ((min (car (cdr range)))
                         (max (cdr (cdr range))))
                     ;; Updated condition: min <= org-priority-lowest AND max >= org-priority-default
                     (and (<= min org-priority-lowest)
                          (>= max org-priority-default))))
                 my-priority-ranges))
               (range-ids (mapcar #'car matching-ranges)))
          (if range-ids
              (let ((selected-range (nth (random (length range-ids)) range-ids)))
                (my-set-priority-with-heuristics selected-range)
                (message "Priority was not set. Assigned random priority within range %d."
                         selected-range))
            ;; Fallback in case no ranges match the criteria
            (message "No valid range found for default priority settings. Check configurations.")))))))

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

(defun my-random-schedule (months &optional n)
  "Schedules an Org heading MONTHS months in the future using a mathematically elegant distribution.
If N is provided, use that as the exponent. If it's not provided, fallback to `my-random-schedule-exponent'."
  (when (and (not noninteractive)
             (eq major-mode 'org-mode))
    (let* ((today (current-time))
           (total-days (* months 30))
           ;; If `n` is not passed in, use our existing defcustom value
           (n (or n my-random-schedule-exponent))
           (u (/ (float (random 1000000)) 1000000.0))
           (exponent (/ 1.0 (+ n 1)))  ; compute 1/(n+1)
           (x (expt u exponent))
           (days-ahead (floor (* total-days x)))
           (random-date (time-add today (days-to-time days-ahead))))
      (org-schedule nil (format-time-string "%Y-%m-%d" random-date)))))

(defun my-random-schedule-command (&optional months)
  "Interactive command to schedule MONTHS months in the future (defaults to `my-random-schedule-default-months`).
Previously, this function would also ensure the heading has a priority set, but that functionality has been removed per your request."
  (interactive
   (list (read-number
          "Enter the upper month limit: "
          my-random-schedule-default-months)))
  (save-excursion
    ;; Schedule the current heading
    (my-random-schedule (or months my-random-schedule-default-months))))

(defun my-schedule-and-set-priority-command (&optional months)
  "Interactive command that schedules MONTHS months in the future and prompts for priority."
  (interactive
   (list (read-number
          "Enter the upper month limit: "
          my-random-schedule-default-months)))
  ;; Schedule the current heading
  (my-random-schedule (or months my-random-schedule-default-months))
  (my-ensure-priority-set)
  ;; Call 'my-set-priority-with-heuristics' interactively
  (call-interactively 'my-set-priority-with-heuristics))

(defun my-post-org-insert-heading (&rest _args)
  "Run after `org-insert-heading` to assign priority and schedule."
  (when (and (not noninteractive)  ;; Avoid running in batch mode
             (eq major-mode 'org-mode)) ;; Ensure it's only in Org mode
    ;; Ensure priority is set (handled inside `my-ensure-priority-set`)
    (my-ensure-priority-set)
    ;; Call the lower-level function directly to schedule default months out with no interactive prompt.
    (my-random-schedule my-random-schedule-default-months 0)
    (end-of-line)))

;; Advise the function that `C-RET` calls, typically `org-insert-heading`
(advice-add 'org-insert-heading :after #'my-post-org-insert-heading)

;; Ensure org-agenda-files is set (adjust the path as needed)
;; (setq org-agenda-files
;;       (directory-files-recursively "~/org/" "\\.org$"))

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

;; Set the default agenda sorting strategy
(setq org-agenda-sorting-strategy
      '((agenda priority-down time-up category-keep)
        (todo priority-down category-keep)
        (tags priority-down)
        (search category-keep)))

(defun my-get-priority-value ()
  "Get the numerical priority value of the current task.
If PRIORITY is not set, return a random value between `org-priority-default` and `org-priority-lowest`."
  (let ((priority-str (org-entry-get nil "PRIORITY")))
    (if priority-str
        ;; If PRIORITY is set, return its value as a number
        (string-to-number priority-str)
      ;; If PRIORITY is not set, return a random value within the range
      (+ org-priority-default
         (random (1+ (- org-priority-lowest org-priority-default)))))))

;; Define variables for outstanding tasks list and index
(defvar my-outstanding-tasks-list nil
  "List of outstanding tasks, sorted by priority.")

(defvar my-outstanding-tasks-index 0
  "Current index in the outstanding tasks list.")

(defun my-get-outstanding-tasks ()
  "Populate `my-outstanding-tasks-list` with outstanding tasks, sorted by priority."
  (setq my-outstanding-tasks-list nil)
  (org-map-entries
   (lambda ()
     (when (my-is-outstanding-task)
       (let* ((priority (my-get-priority-value))
              (marker (point-marker)))
         (push (cons priority marker) my-outstanding-tasks-list))))
   nil
   'agenda)
  ;; Sort the list based on priority (lower numbers indicate higher priority)
  (setq my-outstanding-tasks-list
        (sort my-outstanding-tasks-list (lambda (a b) (< (car a) (car b)))))
  ;; Extract markers only
  (setq my-outstanding-tasks-list (mapcar #'cdr my-outstanding-tasks-list))
  (setq my-outstanding-tasks-index 0))

(require 'pulse)

(defun my-launch-anki ()
  "Launch Anki application if it exists."
  (let* ((user-profile (getenv "USERPROFILE"))  ; Get the user's home directory on Windows
         (anki-path (expand-file-name "AppData/Local/Programs/Anki/anki.exe" user-profile)))
    (if (file-exists-p anki-path)
        (condition-case err
            (start-process "Anki" nil anki-path)
          (error (message "Failed to launch Anki: %s" (error-message-string err))))
      (message "Anki executable not found at: %s" anki-path))))

   (defcustom my-anki-task-ratio 1
     "Ratio of Anki launches to tasks displayed. Default is 1:1 (Anki launched every task).
   Should be a positive integer."
     :type 'integer
     :group 'org-queue)

   (defvar my-anki-task-counter 0
     "Counter of tasks displayed since last Anki launch.")

   ;; Function to set the Anki:Task ratio
   (defun my-set-anki-task-ratio (ratio)
     "Set the ratio of Anki launches to tasks displayed.

   For example, if RATIO is 3, Anki will be launched once every 3 tasks. RATIO should be a positive integer."
     (interactive "nSet Anki:Task ratio (positive integer): ")
     (setq my-anki-task-ratio (max 1 ratio))
     ;; Reset the counter whenever the ratio is changed
     (setq my-anki-task-counter 0)
     (message "Anki will be launched once every %d task(s)." my-anki-task-ratio))

   ;; Helper function to launch Anki according to the ratio
   (defun my-maybe-launch-anki ()
     "Launch Anki according to the set ratio."
     (setq my-anki-task-counter (1+ my-anki-task-counter))
     (when (= (mod my-anki-task-counter my-anki-task-ratio) 0)
       (my-launch-anki)))

(defun my-show-next-outstanding-task ()
  "Show the next outstanding task in priority order.
If the list is exhausted, it refreshes the list."
  (interactive)
  ;; Launch Anki according to the user-defined ratio
  (my-maybe-launch-anki)
  (unless (and my-outstanding-tasks-list
               (< my-outstanding-tasks-index (length my-outstanding-tasks-list)))
    (my-get-outstanding-tasks))
  (if (and my-outstanding-tasks-list
           (< my-outstanding-tasks-index (length my-outstanding-tasks-list)))
      (let ((marker (nth my-outstanding-tasks-index my-outstanding-tasks-list)))
        (switch-to-buffer (marker-buffer marker))
        (goto-char (marker-position marker))
        ;; Ensure the entire entry is visible
        (org-show-entry)
        (org-show-subtree)
        ;; Highlight the entry temporarily
        (let ((pulse-iterations 3)
              (pulse-delay 0.2))
          (pulse-momentary-highlight-one-line (point)))
        ;; Center the entry in the window
        (recenter)
        ;; Increment the index after showing the task
        (setq my-outstanding-tasks-index (1+ my-outstanding-tasks-index)))
    (message "No more outstanding tasks.")))

(defun my-show-current-outstanding-task ()
  "Show the current outstanding task."
  (interactive)
  (if (and my-outstanding-tasks-list
           (> my-outstanding-tasks-index 0)
           (<= my-outstanding-tasks-index (length my-outstanding-tasks-list)))
      (let ((marker (nth (1- my-outstanding-tasks-index) my-outstanding-tasks-list)))
        (switch-to-buffer (marker-buffer marker))
        (goto-char (marker-position marker))
        ;; Ensure the entire entry is visible
        (org-show-entry)
        (org-show-subtree)
        ;; Highlight the entry temporarily
        (let ((pulse-iterations 3)
              (pulse-delay 0.2))
          (pulse-momentary-highlight-one-line (point)))
        ;; Center the entry in the window
        (recenter))
    (message "No current outstanding task to show.")))

(defun my-show-previous-outstanding-task ()
  "Show the previous outstanding task."
  (interactive)
  (if (and my-outstanding-tasks-list
           (> my-outstanding-tasks-index 1))
      (let ((marker (nth (- my-outstanding-tasks-index 2) my-outstanding-tasks-list)))
        ;; Decrement the index before displaying the task
        (setq my-outstanding-tasks-index (1- my-outstanding-tasks-index))
        (switch-to-buffer (marker-buffer marker))
        (goto-char (marker-position marker))
        ;; Ensure the entire entry is visible
        (org-show-entry)
        (org-show-subtree)
        ;; Highlight the entry temporarily
        (let ((pulse-iterations 3)
              (pulse-delay 0.2))
          (pulse-momentary-highlight-one-line (point)))
        ;; Center the entry in the window
        (recenter))
    (message "No previous outstanding task.")))

(defun my-reset-outstanding-tasks-index ()
  "Reset the outstanding tasks index to start from the first task."
  (interactive)
  (my-get-outstanding-tasks)
  (setq my-outstanding-tasks-index 0)
  (message "Outstanding tasks index reset."))

;; Define a prefix command for your tasks
(define-prefix-command 'my-tasks-map)
(global-set-key (kbd "C-c q") 'my-tasks-map)

;; Bind your functions to keys under the prefix
(define-key my-tasks-map (kbd ",") 'my-set-priority-with-heuristics)
(define-key my-tasks-map (kbd "s") 'my-schedule-and-set-priority-command)
(define-key my-tasks-map (kbd "n") 'my-show-next-outstanding-task)
(define-key my-tasks-map (kbd "p") 'my-show-previous-outstanding-task)
(define-key my-tasks-map (kbd "c") 'my-show-current-outstanding-task)
(define-key my-tasks-map (kbd "r") 'my-reset-outstanding-tasks-index)

(provide 'org-queue) ;; Provide the 'org-queue' feature to make it available for require statements
