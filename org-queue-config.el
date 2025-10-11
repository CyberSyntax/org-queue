;;; org-queue-config.el --- Configuration and utilities for org-queue -*- lexical-binding: t -*-
;;; Code:
(require 'cl-lib)  ;; Required for cl-find-if and cl-remove-if-not
(eval-when-compile (require 'org))  ;; silence compiler; no agenda dependency

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

;; Night shift configuration
(defcustom org-queue-night-shift-enabled t
  "If non-nil, night shift rules are enforced."
  :type 'boolean
  :group 'org-queue)

(defcustom org-queue-non-srs-snooze-base-minutes 0
  "Base minutes added to non-SRS deferral when snoozed.
Used by the non-SRS Next Repetition action to compute available-at."
  :type 'integer
  :group 'org-queue)

(defcustom org-queue-non-srs-snooze-slope-minutes 1
  "Slope minutes multiplied by numeric priority for non-SRS snooze.
Deferral is computed as f(p) = BASE + SLOPE * p, where p is the numeric priority (1..64)."
  :type 'integer
  :group 'org-queue)

(defcustom org-queue-qforce-ignores-last-repeat nil
  "If non-nil, QFORCE entries ignore LAST_REPEAT deferral and are always available immediately.
When enabled, QFORCE tasks set available-at = now regardless of LAST_REPEAT."
  :type 'boolean
  :group 'org-queue)

(defvar org-queue--suppress-ui nil
  "When non-nil (dynamically bound), suppresses queue UI updates like
reassign/show-top during batch operations (e.g., chooser bulk actions).")

(defvar org-queue--suppress-save nil
  "When non-nil (dynamically bound), suppress opportunistic saves during
org-queue operations. Bind with (let ((org-queue--suppress-save t)) ...)
to batch changes and save once at the end.")

(defcustom org-queue-auto-show-top-after-change nil
  "If non-nil, automatically call `org-queue-show-top` after operations
that change schedule/priority (advance, postpone, schedule, stamp)."
  :type 'boolean
  :group 'org-queue)

(defcustom org-queue-night-shift-start "22:00"
  "Local time-of-day when night shift begins (HH:MM, 24h)."
  :type 'string
  :group 'org-queue)

(defcustom org-queue-night-shift-end "06:00"
  "Local time-of-day when night shift ends (HH:MM, 24h)."
  :type 'string
  :group 'org-queue)

(defun org-queue--parse-hhmm (s)
  "Return minutes since midnight for string S = \"HH:MM\"."
  (when (and (stringp s) (string-match "\\`\\([0-2][0-9]\\):\\([0-5][0-9]\\)\\'" s))
    (+ (* 60 (string-to-number (match-string 1 s)))
       (string-to-number (match-string 2 s)))))

(defun org-queue-night-shift-p (&optional at-time)
  "Return non-nil if local time AT-TIME is within night-shift window.
Uses `org-queue-night-shift-enabled', `org-queue-night-shift-start', and
`org-queue-night-shift-end'. If AT-TIME is nil, use current time."
  (when org-queue-night-shift-enabled
    (let* ((now (or at-time (current-time)))
           (hm (+ (* 60 (string-to-number (format-time-string "%H" now)))
                  (string-to-number (format-time-string "%M" now)))))
      (let ((start (org-queue--parse-hhmm org-queue-night-shift-start))
            (end   (org-queue--parse-hhmm org-queue-night-shift-end)))
        (when (and start end)
          (if (<= start end)
              (and (>= hm start) (< hm end))
            (or (>= hm start) (< hm end))))))))

;;; Priority Configuration
(setq org-priority-highest 1)
(setq org-priority-default 32)
(setq org-priority-lowest 64)

(defcustom org-queue-srs-mix-ratio '(1 . 4)
  "Interleave ratio (NON-SRS . SRS) for the queue, e.g., (1 . 4)."
  :type '(cons (integer :tag "Non-SRS") (integer :tag "SRS"))
  :group 'org-queue)

;; Head start policy for interleaving
(defcustom org-queue-mix-start 'rotate
  "Which pool starts the interleave when building the head of the queue.
- 'non-srs  : start with non-SRS block
- 'srs      : start with SRS block
- 'rotate   : alternate the start across head consumptions, keeping the ratio
- 'auto     : pick the pool whose earliest item is due earlier"
  :type '(choice (const non-srs) (const srs) (const rotate) (const auto))
  :group 'org-queue)

(defvar org-queue--mix-phase 0
  "Phase counter used by 'rotate to decide which pool starts at the head.")
(defcustom org-queue-srs-conceal-answer t
  "If non-nil, conceal the answer (Back) when visiting SRS items."
  :type 'boolean
  :group 'org-queue)

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

;;; Directory Configuration
(defcustom org-queue-cache-dir
  (let* ((base (cond
                ;; Respect no-littering if present
                ((boundp 'no-littering-var-directory) (file-name-as-directory no-littering-var-directory))
                ;; Fallback under user-emacs-directory/var/
                (t (file-name-as-directory (expand-file-name "var/" user-emacs-directory)))))
         (dir (expand-file-name "org-queue/" base)))
    dir)
  "Directory where org-queue stores its cache files."
  :type 'directory
  :group 'org-queue)

;; Ensure the cache directory exists at load time (best-effort).
(ignore-errors
  (unless (file-directory-p org-queue-cache-dir)
    (make-directory org-queue-cache-dir t)))

;; Keep the global org-id DB under our var/ directory
(setq org-id-locations-file (expand-file-name "org-id-locations" org-queue-cache-dir)
      org-id-locations-file-relative t)

(defcustom org-queue-directory nil
  "Base directory for org-queue files (searched recursively). Set this or `org-queue-file-roots`."
  :type '(choice (const :tag "Unset" nil) directory)
  :group 'org-queue)

;; === File indexing ===
(defun org-queue-reindex-files (&optional silent)
  "Return the absolute list of .org files scanned by org-queue.
It searches recursively under `org-queue-directory`.
Returns an empty list if the directory is unset or does not exist.
Does not read or modify `org-agenda-files`."
  (let* ((root (and org-queue-directory
                    (file-directory-p org-queue-directory)
                    (file-truename org-queue-directory)))
         (files (if root
                    (mapcar #'file-truename
                            (directory-files-recursively root "\\.org\\'"))
                  '())))
    (unless silent
      (message "org-queue: indexed %d file(s)" (length files)))
    files))

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
