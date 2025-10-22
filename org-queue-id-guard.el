;;; org-queue-id-guard.el --- Automatic, single-path ID guarding for org-queue -*- lexical-binding: t; -*-

(require 'org)
(require 'cl-lib)
(require 'subr-x)
(require 'org-id)
(require 'seq)

(require 'org-queue-config)
(require 'org-queue-utils)
(require 'org-queue-tasks)
(require 'org-queue-display)

;; ======================================================================
;; Behavior (no options):
;; - Timebox each ID resolution: 0.12s
;; - Cooldown after failure:     300s
;; - Always skip remote/TRAMP files for ID resolution
;; - Automatically prune unresolved items during maintenance/hard refresh/midnight
;; - Visiting an unresolved row auto-removes it and refreshes
;; ======================================================================

(defconst org-queue-id-guard--timeout 0.12)
(defconst org-queue-id-guard--cooldown 300)

(defvar org-queue--id-fail-cache (make-hash-table :test 'equal))

(defun org-queue--id-recently-failed-p (id)
  (let ((ts (and id (gethash id org-queue--id-fail-cache))))
    (and ts (< (float-time (time-subtract (current-time) ts))
               org-queue-id-guard--cooldown))))

(defun org-queue--note-id-failure (id)
  (when id (puthash id (current-time) org-queue--id-fail-cache)))

;; ----------------------------------------------------------------------
;; Time-box my-extract-marker so Emacs never hangs on bad IDs.
;; Always skip remote/TRAMP paths (fast fail).
;; ----------------------------------------------------------------------

(defun org-queue--timebox-id-resolution (orig-fn task-or-marker)
  (let* ((id   (and (listp task-or-marker) (plist-get task-or-marker :id)))
         (file (and (listp task-or-marker) (plist-get task-or-marker :file))))
    (cond
     ((and id (org-queue--id-recently-failed-p id)) nil)
     ((and file (file-remote-p file))
      (org-queue--note-id-failure id)
      nil)
     (t
      (let ((res (with-timeout (org-queue-id-guard--timeout
                                (progn (org-queue--note-id-failure id) nil))
                   (funcall orig-fn task-or-marker))))
        (when (and id (not res))
          (org-queue--note-id-failure id))
        res)))))

(unless (advice-member-p #'org-queue--timebox-id-resolution 'my-extract-marker)
  (advice-add 'my-extract-marker :around #'org-queue--timebox-id-resolution))

;; ----------------------------------------------------------------------
;; Auto-drop helpers (no prompts)
;; ----------------------------------------------------------------------

(defun org-queue-id-guard--task-key (task)
  (or (plist-get task :id)
      (let* ((f (plist-get task :file))
             (p (plist-get task :pos)))
        (when (and f p) (format "%s@%d" (file-truename f) p)))))

(defun org-queue-id-guard--auto-drop-unresolved (&optional silent)
  "Remove unresolved/bad-ID entries from the global queue automatically.
Fast path only: never performs lookups or re-resolution.
Rules:
- KEEP if :marker exists and its buffer is live.
- DROP if ID is in the recent-failure cache.
- DROP otherwise (we don't attempt to resolve IDs here).

Saves once at end; refreshes chooser buffers."
  (let ((dropped 0))
    (when (and (boundp 'my-outstanding-tasks-list) my-outstanding-tasks-list)
      (let ((victims '()))
        (cl-loop for i from 0 below (length my-outstanding-tasks-list) do
                 (let* ((task (nth i my-outstanding-tasks-list))
                        (mk   (plist-get task :marker))
                        (tid  (plist-get task :id)))
                   (cond
                    ;; Keep if live marker is present
                    ((and (markerp mk) (marker-buffer mk) (buffer-live-p (marker-buffer mk))))
                    ;; Drop if we already know this ID fails
                    ((and tid (org-queue--id-recently-failed-p tid)) (push i victims))
                    ;; Drop anything else without attempting lookups/resolution
                    (t (push i victims)))))
        ;; Remove from highest index down to avoid shifting
        (dolist (i (sort victims #'>))
          (ignore-errors
            (my-queue--remove-index i t t)
            (setq dropped (1+ dropped))))))
    (when (> dropped 0)
      (unless silent
        (message "org-queue: auto-dropped %d unresolved item(s)" dropped))
      ;; Re‑shape after deletions if both pools exist
      (when (and (boundp 'my-outstanding-tasks-list)
                 (seq-some (lambda (t) (plist-get t :srs))     my-outstanding-tasks-list)
                 (seq-some (lambda (t) (not (plist-get t :srs))) my-outstanding-tasks-list))
        (org-queue--reinterleave-outstanding!)))))

;; Hook auto-drop into maintenance, and midnight refresh.
(unless (advice-member-p #'org-queue-id-guard--auto-drop-unresolved 'org-queue-maintenance)
  (advice-add 'org-queue-maintenance :after (lambda (&rest _) (org-queue-id-guard--auto-drop-unresolved t))))
(unless (advice-member-p #'org-queue-id-guard--auto-drop-unresolved 'org-queue--midnight-refresh)
  (advice-add 'org-queue--midnight-refresh :after (lambda (&rest _) (org-queue-id-guard--auto-drop-unresolved t))))

(defconst org-queue-id-guard-idle-secs 2
  "Idle seconds before running a coalesced file-local queue refresh.")

(defconst org-queue-id-guard-max-files-per-flush 8
  "Upper bound of files to refresh per idle flush.")

(defvar org-queue-id-guard--pending-files (make-hash-table :test 'equal))
(defvar org-queue-id-guard--flush-timer nil)

(defun org-queue-id-guard--queue-id-update ()
  "Queue current file for a coalesced, silent org-id update."
  (when (and buffer-file-name (string-match-p "\\.org\\'" buffer-file-name))
    (puthash (file-truename buffer-file-name) t org-queue-id-guard--pending-files)
    (when org-queue-id-guard--flush-timer
      (cancel-timer org-queue-id-guard--flush-timer))
    (setq org-queue-id-guard--flush-timer
          (run-with-idle-timer org-queue-id-guard-idle-secs nil
                               #'org-queue-id-guard--flush-id-updates))))

(defun org-queue-id-guard--refresh-task-marker! (task)
  "Re-resolve TASK's marker strictly inside its recorded :file.
On success, update :marker/:pos/:heading/:available-at and return non-nil.
On failure, return nil (caller may drop the task from the queue)."
  (cl-block org-queue-id-guard--refresh-task-marker!
    (let* ((id   (plist-get task :id))
           (file (plist-get task :file))
           (pos0 (plist-get task :pos)))
      (when (and id file (file-exists-p file) (not (file-remote-p file)))
        (let* ((cands (my--candidates-for-id-in-file id file))
               (chosen
                (or
                 ;; Prefer candidate whose heading matches cached heading
                 (and (plist-get task :heading)
                      (cl-find-if
                       (lambda (mk)
                         (with-current-buffer (marker-buffer mk)
                           (save-excursion
                             (goto-char (marker-position mk))
                             (string= (or (org-get-heading t t t t) "")
                                      (or (plist-get task :heading) "")))))
                       cands))
                 ;; Else closest to cached :pos
                 (and (numberp pos0)
                      (car (sort (copy-sequence cands)
                                 (lambda (a b)
                                   (< (abs (- (marker-position a) pos0))
                                      (abs (- (marker-position b) pos0)))))))
                 ;; Else first candidate
                 (car cands))))
          (when (and chosen (marker-buffer chosen) (buffer-live-p (marker-buffer chosen)))
            (my--task-sync-metadata task chosen)
            (org-queue--update-available-at! task)
            (cl-return-from org-queue-id-guard--refresh-task-marker! t))))
      nil)))

(defun org-queue-id-guard--refresh-queue-markers-for-file (file &optional silent)
  "For FILE: re-resolve markers for queue entries (outstanding + pending).
Drops entries that cannot be resolved inside FILE.
Returns (REFRESHED . DROPPED). Skips remote/TRAMP files entirely."
  (let* ((tru (and file (file-truename file)))
         (remote (and tru (file-remote-p tru)))
         (refreshed 0)
         (dropped   0))
    (when (and tru (file-exists-p tru))
      (dolist (list-sym '(my-outstanding-tasks-list my-today-pending-tasks))
        (let ((acc '()))
          (dolist (task (symbol-value list-sym))
            (if (and (plist-get task :file)
                     (file-exists-p (plist-get task :file))
                     (file-equal-p tru (file-truename (plist-get task :file))))
                (cond
                 (remote (push task acc)) ;; never resolve TRAMP
                 ((org-queue-id-guard--refresh-task-marker! task)
                  (setq refreshed (1+ refreshed))
                  (push task acc))
                 (t
                  (setq dropped (1+ dropped))))
              (push task acc)))
          (set list-sym (nreverse acc)))))
    (unless silent
      (message "org-queue: %s → refreshed %d, dropped %d"
               (and file (file-name-nondirectory file)) refreshed dropped))
    ;; If anything was dropped from outstanding/pending, keep the ratio honest for outstanding.
    (when (> dropped 0)
      (when (and (boundp 'my-outstanding-tasks-list)
                 (seq-some (lambda (t) (plist-get t :srs))     my-outstanding-tasks-list)
                 (seq-some (lambda (t) (not (plist-get t :srs))) my-outstanding-tasks-list))
        (org-queue--reinterleave-outstanding!)))
    (cons refreshed dropped)))

(defun org-queue-id-guard--flush-id-updates ()
  "Idle flush: for up to `org-queue-id-guard-max-files-per-flush` pending files,
refresh queue entries strictly inside those files (no global org-id updates)."
  (let (files)
    (maphash (lambda (k _v) (push k files)) org-queue-id-guard--pending-files)
    (when files
      (let* ((cap (max 1 org-queue-id-guard-max-files-per-flush))
             (batch (seq-take (delete-dups (mapcar #'file-truename files)) cap))
             (changed nil))
        ;; consume the batch from the pending set
        (dolist (f batch) (remhash f org-queue-id-guard--pending-files))
        ;; refresh per file
        (dolist (f batch)
          (pcase-let ((`(,refreshed . ,dropped)
                       (ignore-errors
                         (org-queue-id-guard--refresh-queue-markers-for-file f t))))
            (when (and refreshed dropped (> (+ refreshed dropped) 0))
              (setq changed t))))
        ;; reflect changes
        (when changed
          (my-queue-limit-visible-buffers))))))

(add-hook 'org-mode-hook
          (lambda ()
            (add-hook 'after-save-hook #'org-queue-id-guard--queue-id-update nil t)))

(provide 'org-queue-id-guard)
;;; org-queue-id-guard.el ends here
