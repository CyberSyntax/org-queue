;;; org-queue-config.el --- Configuration and utilities for org-queue -*- lexical-binding: t -*-
;;; Code:
(require 'org-agenda)
(require 'cl-lib)  ;; Required for cl-find-if and cl-remove-if-not
(defgroup org-queue nil
  "Task queue management for Org mode."
  :group 'org
  :prefix "org-queue-")
;; Disable large file warning
(setq large-file-warning-threshold nil)
(random t)
(defcustom org-queue-verbose t
  "If non-nil, print progress messages."
  :type 'boolean
  :group 'org-queue)
;;; System Detection
(defvar my-android-p 
  (eq system-type 'android)
  "Non-nil if running on Android.")
;;; Priority Configuration
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
;;; Cache Directory Configuration
(defvar cache-dir (expand-file-name "org-queue-cache/" user-emacs-directory)
  "Directory path for cache files.")
(unless (file-directory-p cache-dir)
  (make-directory cache-dir t))

;;; Directory Configuration - ADD THIS NEW SECTION HERE
(defcustom org-queue-directory 
  (expand-file-name "~/org/")  
  "Base directory for org-queue files (searched recursively)."
  :type 'directory
  :group 'org-queue)

(defun org-queue-setup-agenda-files ()
  "Set org-agenda-files to all .org files found recursively in org-queue-directory."
  (when (and org-queue-directory 
             (file-directory-p org-queue-directory))
    (setq org-agenda-files 
          (directory-files-recursively org-queue-directory "\\.org$"))))

;; Auto-setup when loaded
(org-queue-setup-agenda-files)

;; Hook to refresh on startup
(add-hook 'after-init-hook #'org-queue-setup-agenda-files)

;;; Utility Functions
(defun random-float (min max)
  "Return a random float between MIN and MAX."
  (+ min (* (- max min) (/ (float (random 1000000)) 1000000))))
;;; Priority Helper Functions
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
(provide 'org-queue-config)
;;; org-queue-config.el ends here
