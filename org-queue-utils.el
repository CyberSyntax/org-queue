;;; org-queue-utils.el --- Task utilities for org-queue -*- lexical-binding: t -*-

;;; Code:

(require 'org-queue-config)
(require 'cl-lib)

;; Bulk-save control for maintenance and other batch operations
(defmacro org-queue--without-autosave (&rest body)
  "Run BODY with all explicit saves suppressed.
Disables save-buffer/save-some-buffers and sets org-queue--suppress-save."
  (declare (indent 0) (debug t))
  `(let ((org-queue--suppress-save t))
     (cl-letf (((symbol-function 'save-buffer) (lambda (&rest _args) nil))
               ((symbol-function 'save-some-buffers) (lambda (&rest _args) nil)))
       ,@body)))

(defmacro org-queue--with-batched-saves (&rest body)
  "Disable saves inside BODY; then perform one real save at the end."
  (declare (indent 0) (debug t))
  `(let ((org-queue--suppress-save t))
     (unwind-protect
         ;; Disable saves only while BODY runs:
         (org-queue--without-autosave ,@body)
       ;; Final flush after the suppression scope ended:
       (let ((org-queue--suppress-save nil))
         (save-some-buffers t)))))

(defvar org-queue--file-cache nil)
(defvar org-queue--file-cache-ts nil)

(defcustom org-queue-file-cache-ttl 10
  "Seconds to cache the file roster before re-scanning."
  :type 'integer :group 'org-queue)

(defun org-queue-file-list ()
  (let ((fresh (and org-queue--file-cache
                    org-queue--file-cache-ts
                    (< (float-time (time-subtract (current-time) org-queue--file-cache-ts))
                       org-queue-file-cache-ttl))))
    (if fresh
        org-queue--file-cache
      (setq org-queue--file-cache (org-queue-reindex-files t)
            org-queue--file-cache-ts (current-time))
      org-queue--file-cache)))

(defun org-queue-map-entries (fn &optional matcher)
  "Run FN on each entry matching MATCHER across org-queue files."
  (dolist (file (org-queue-file-list))
    (when (file-exists-p file)
      (with-current-buffer (find-file-noselect file)
        (org-map-entries fn matcher 'file)))))

(defun org-queue-collect-markers (&optional matcher)
  "Collect heading markers across org-queue files."
  (let (xs)
    (org-queue-map-entries (lambda () (push (point-marker) xs)) matcher)
    (nreverse xs)))

;;; Task State Functions

(defun my-is-todo-task ()
  "Return non-nil if the current task is in a TODO state (not DONE)."
  (let ((todo-state (org-get-todo-state)))
    (and todo-state
         (not (member todo-state org-done-keywords)))))

(defun my-is-done-task ()
  "Return non-nil if the current task is in a DONE state."
  (let ((todo-state (org-get-todo-state)))
    (and todo-state
         (member todo-state org-done-keywords))))

(defun my-cleanup-done-task ()
  "Remove SCHEDULED and PRIORITY properties from DONE tasks."
  (when (my-is-done-task)
    (let ((had-schedule (org-get-scheduled-time nil))
          (had-priority (org-entry-get nil "PRIORITY"))
          (cleaned nil))
      ;; Remove SCHEDULED
      (when had-schedule
        (org-schedule '(4))  ; C-u C-c C-s (remove schedule)
        (setq cleaned t))
      ;; Remove PRIORITY - correct method
      (when (and had-priority (not (string= had-priority " ")))
        (org-priority ?\ )  ; Set priority to space (removes it)
        (setq cleaned t))
      ;; Return t if we cleaned anything
      cleaned)))

(defun my-cleanup-all-done-tasks ()
  "Remove SCHEDULED and PRIORITY properties from all DONE tasks across agenda files."
  (interactive)
  (let ((cleaned-count 0)
        (total-done 0))
    (org-queue-map-entries
     (lambda ()
       (when (my-is-done-task)
         (setq total-done (1+ total-done))
         (when (my-cleanup-done-task)
           (setq cleaned-count (1+ cleaned-count)))))
     nil)
    (message "✓ Cleaned %d of %d DONE tasks (removed SCHEDULED/PRIORITY)"
             cleaned-count total-done)))

;;; Mathematical Utilities

(defun my-round-to-decimals (number decimals)
  "Round NUMBER to DECIMALS decimal places."
  (/ (float (round (* number (expt 10 decimals))))
     (expt 10 decimals)))

(defun my-custom-shuffle (list)
  "Fisher-Yates shuffle implementation for Emacs Lisp."
  (let ((vec (vconcat list)) (i (length list)))
    (while (> i 1)
	(let* ((j (random i))
	       (temp (aref vec (setq i (1- i)))))
	  (aset vec i (aref vec j))
	  (aset vec j temp)))
    (append vec nil)))

(provide 'org-queue-utils)
;;; org-queue-utils.el ends here
