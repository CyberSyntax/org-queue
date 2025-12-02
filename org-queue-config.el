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

(defcustom org-queue-non-srs-snooze-slope-minutes 10
  "Slope minutes multiplied by numeric priority for non-SRS snooze.
Deferral is computed as f(p) = BASE + SLOPE * p, where p is the numeric priority (1..64)."
  :type 'integer
  :group 'org-queue)

(defcustom org-queue-qforce-ignores-last-repeat nil
  "If non-nil, QFORCE entries ignore LAST_REPEAT deferral and are always available immediately.
When enabled, QFORCE tasks set available-at = now regardless of LAST_REPEAT."
  :type 'boolean
  :group 'org-queue)

(defcustom org-queue-strict-id-resolution t
  "When non-nil, a task plist that contains :id must resolve to a heading
with the *same* :ID: in its recorded :file. If the :id cannot be found or
the heading at the fallback position has a different :ID:, resolution fails
(nil) instead of snapping to the nearest heading.

When nil, resolution may fall back to :file + :pos even if the :id is missing
or mismatched."
  :type 'boolean
  :group 'org-queue)

(defvar org-queue--suppress-ui nil
  "When non-nil (dynamically bound), suppresses queue UI updates like
reassign/show-top during batch operations (e.g., chooser bulk actions).")

(defvar org-queue--suppress-save nil
  "When non-nil (dynamically bound), suppress opportunistic saves during
org-queue operations. Bind with (let ((org-queue--suppress-save t)) ...)
to batch changes and save once at the end.")

(defvar org-queue--suppress-micro-update nil
  "When non-nil (dynamically bound), skip queue micro-updates triggered by
advice and change hooks. Bind around bursty edits to avoid thrashing.")

(defcustom org-queue-auto-show-top-after-change nil
  "If non-nil, automatically call `org-queue-show-top` after operations
that change schedule/priority (advance, postpone, schedule, stamp)."
  :type 'boolean
  :group 'org-queue)

(defcustom org-queue-autosave-on-change nil
  "If non-nil, org-queue automatically saves Org buffers after queue
operations that touch priority, schedule, clozes, extracts, etc.

Set this to nil if you prefer manual saving (for example via `C-; c`)."
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

(defcustom org-queue-directory "~/org-queue"
  "Base directory for org-queue files (searched recursively). Set this or `org-queue-file-roots`."
  :type '(choice (const :tag "Unset" nil) directory)
  :group 'org-queue)

;;; Per-day file list cache (lives inside `org-queue-directory`)

(defun org-queue--file-list-cache-path ()
  "Return the path of the per-day Org file list cache, or nil if unset.
The cache lives inside `org-queue-directory` so it can be synced across machines."
  (when org-queue-directory
    (expand-file-name ".org-queue-files-cache.el"
                      (file-name-as-directory org-queue-directory))))

(defun org-queue--read-file-list-cache (root)
  "If there is a valid cache for ROOT for today, return an absolute file list.
Otherwise return nil. ROOT must be the truename of `org-queue-directory`."
  (let ((cache (org-queue--file-list-cache-path)))
    (when (and cache (file-exists-p cache))
      (condition-case _err
          (with-temp-buffer
            (insert-file-contents cache)
            (let* ((data  (read (current-buffer)))
                   (date  (plist-get data :date))
                   (files (plist-get data :files)))
              (when (and (stringp date)
                         (string= date (format-time-string "%Y-%m-%d"))
                         (listp files))
                (let ((root* (file-truename root)))
                  ;; Rebuild absolute paths from relative ones and drop missing files.
                  (delq nil
                        (mapcar
                         (lambda (rel)
                           (let* ((abs (expand-file-name rel root*)))
                             (when (file-exists-p abs)
                               (file-truename abs))))
                         files))))))
        (error nil)))))

(defun org-queue--write-file-list-cache (root files)
  "Write FILES (absolute paths) as today's cache rooted at ROOT.
Files are stored as paths relative to ROOT so the cache is portable across machines."
  (let ((cache (org-queue--file-list-cache-path)))
    (when cache
      (let* ((dir      (file-name-directory cache))
             (root*    (file-truename root))
             (relfiles (mapcar (lambda (f)
                                 (file-relative-name (file-truename f) root*))
                               files))
             (data     (list :date  (format-time-string "%Y-%m-%d")
                             :files relfiles)))
        (when (and dir (not (file-directory-p dir)))
          (ignore-errors (make-directory dir t)))
        (with-temp-file cache
          (insert (prin1-to-string data)))))))

;; === File indexing ===
(defun org-queue-reindex-files (&optional silent)
  "Return the absolute list of .org files scanned by org-queue.

It searches recursively under `org-queue-directory`.

Optimization:
- First, try to read today's cached file list from
  `org-queue-directory/.org-queue-files-cache.el`.
- If the cache is missing or stale, rescan and refresh it.

Returns an empty list if the directory is unset or does not exist.
Does not read or modify `org-agenda-files`."
  (let* ((root (and org-queue-directory
                    (file-directory-p org-queue-directory)
                    (file-truename org-queue-directory))))
    (if (not root)
        (progn
          (unless silent
            (message "org-queue: no valid org-queue-directory; nothing to index"))
          '())
      ;; Try cache first.
      (or
       (let ((cached (org-queue--read-file-list-cache root)))
         (when cached
           (unless silent
             (message "org-queue: using cached file list (%d file(s))"
                      (length cached)))
           cached))
       ;; Cache miss or stale → rescan and refresh cache.
       (let* ((files (mapcar #'file-truename
                             (directory-files-recursively root "\\.org\\'"))))
         (org-queue--write-file-list-cache root files)
         (unless silent
           (message "org-queue: indexed %d file(s) (cache refreshed)"
                    (length files)))
         files)))))

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
