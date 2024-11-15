;;; org-queue.el

;; Ensure Org Agenda is loaded
(require 'org-agenda)

;; Set extended numerical priority range
(setq org-priority-highest 1)
(setq org-priority-lowest 64)
(setq org-priority-default 50)

;; Ensure org-agenda-files is set (adjust the path as needed)
;; (setq org-agenda-files
;;       (directory-files-recursively "~/org/" "\\.org$"))

;; Define a function to determine if a task is outstanding (overdue or due today)
(defun my-is-outstanding-task ()
  "Return non-nil if the current task is overdue or due today."
  (let ((scheduled-time (org-get-scheduled-time nil)))
    (and scheduled-time
         (<= (time-to-days scheduled-time) (time-to-days (current-time))))))

;; Define a function to skip non-outstanding tasks (for use in agenda)
(defun my-org-agenda-skip-non-outstanding-tasks ()
  "Skip tasks that are not outstanding."
  (unless (my-is-outstanding-task)
    (org-end-of-subtree t)))

;; Define functions to skip future and scheduled tasks (as before)
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

;; Define custom commands with explicit sorting strategies
(setq org-agenda-custom-commands
      '(("o" "Outstanding Tasks (Overdue or Due Today)"
         ((agenda ""
                  ((org-agenda-span 'day)
                   (org-agenda-skip-function 'my-org-agenda-skip-non-outstanding-tasks)
                   (org-agenda-overriding-header "\nOutstanding Tasks (Overdue or Due Today)\n")
                   (org-agenda-sorting-strategy '(priority-down time-up)))))
         )
        ("f" "Future Tasks (Scheduled for Tomorrow and Beyond)"
         ((agenda ""
                  ((org-agenda-start-day "+1d")
                   (org-agenda-span 14)
                   (org-agenda-skip-function 'my-org-agenda-skip-past-and-today-tasks)
                   (org-agenda-overriding-header "\nFuture Tasks (Scheduled for Tomorrow and Beyond)\n")
                   (org-agenda-sorting-strategy '(priority-down time-up)))))
         )
        ("u" "Unscheduled Tasks (No SCHEDULED Date)"
         ((todo ""
                ((org-agenda-skip-function 'my-org-agenda-skip-scheduled-tasks)
                 (org-agenda-overriding-header "\nUnscheduled Tasks (No SCHEDULED Date)\n")
                 (org-agenda-sorting-strategy '(priority-down)))))
         )
        ("c" "Combined Agenda View"
         ((agenda ""
                  ((org-agenda-span 'day)
                   (org-agenda-skip-function 'my-org-agenda-skip-non-outstanding-tasks)
                   (org-agenda-overriding-header "\nOutstanding Tasks (Overdue or Due Today)\n")
                   (org-agenda-sorting-strategy '(priority-down time-up))))
          (agenda ""
                  ((org-agenda-start-day "+1d")
                   (org-agenda-span 14)
                   (org-agenda-skip-function 'my-org-agenda-skip-past-and-today-tasks)
                   (org-agenda-overriding-header "\nFuture Tasks (Scheduled for Tomorrow and Beyond)\n")
                   (org-agenda-sorting-strategy '(priority-down time-up))))
          (todo ""
                ((org-agenda-skip-function 'my-org-agenda-skip-scheduled-tasks)
                 (org-agenda-overriding-header "\nUnscheduled Tasks (No SCHEDULED Date)\n")
                 (org-agenda-sorting-strategy '(priority-down)))))
         )))

;; Define a function to get the priority value considering numerical priorities
(defun my-get-priority-value ()
  "Get the numerical priority value of the current task."
  (let ((priority-str (org-entry-get nil "PRIORITY")))
    (if priority-str
        (string-to-number priority-str)
      org-priority-default)))

;; Define variables for outstanding tasks list and index
(defvar my-outstanding-tasks-list nil
  "List of outstanding tasks, sorted by priority.")

(defvar my-outstanding-tasks-index 0
  "Current index in the outstanding tasks list.")

;; Define function to get outstanding tasks, sorted by priority
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

(defun my-show-next-outstanding-task ()
  "Show the next outstanding task in priority order.
If the list is exhausted, it refreshes the list."
  (my-launch-anki)
  (interactive)
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
  (setq my-outstanding-tasks-index 0)
  (message "Outstanding tasks index reset."))

;; Define a prefix command for your tasks
(define-prefix-command 'my-tasks-map)
(global-set-key (kbd "C-c t") 'my-tasks-map)

;; Bind your functions to keys under the prefix
(define-key my-tasks-map (kbd "n") 'my-show-next-outstanding-task)
(define-key my-tasks-map (kbd "p") 'my-show-previous-outstanding-task)
(define-key my-tasks-map (kbd "c") 'my-show-current-outstanding-task)
(define-key my-tasks-map (kbd "r") 'my-reset-outstanding-tasks-index)

(provide 'org-queue) ;; Provide the 'org-queue' feature to make it available for require statements

;;; End of org-queue.el
