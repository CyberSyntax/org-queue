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
(require 'org-queue-chooser)

;; ======================================================================
;; Fixed behavior (no options):
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
      (with-timeout (org-queue-id-guard--timeout
                     (progn (org-queue--note-id-failure id) nil))
        (funcall orig-fn task-or-marker))))))

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
      (my-save-outstanding-tasks-to-file)
      (condition-case _ (org-queue-chooser-refresh) (error nil))
      (unless silent
        (message "org-queue: auto-dropped %d unresolved item(s)" dropped)))))

;; Hook auto-drop into maintenance, hard refresh, and midnight refresh.
(unless (advice-member-p #'org-queue-id-guard--auto-drop-unresolved 'org-queue-maintenance)
  (advice-add 'org-queue-maintenance :after (lambda (&rest _) (org-queue-id-guard--auto-drop-unresolved t))))
(unless (advice-member-p #'org-queue-id-guard--auto-drop-unresolved 'org-queue-hard-refresh)
  (advice-add 'org-queue-hard-refresh :after (lambda (&rest _) (org-queue-id-guard--auto-drop-unresolved t))))
(unless (advice-member-p #'org-queue-id-guard--auto-drop-unresolved 'org-queue--midnight-refresh)
  (advice-add 'org-queue--midnight-refresh :after (lambda (&rest _) (org-queue-id-guard--auto-drop-unresolved t))))
;; Also prune after chooser-level hard refresh (G)
(unless (advice-member-p #'org-queue-id-guard--auto-drop-unresolved 'org-queue-chooser-hard-refresh)
  (advice-add 'org-queue-chooser-hard-refresh :after (lambda (&rest _) (org-queue-id-guard--auto-drop-unresolved t))))

;; ----------------------------------------------------------------------
;; Chooser: neutral placeholders for unresolved rows (no user action).
;; Visiting a bad row auto-removes it and refreshes.
;; ----------------------------------------------------------------------

(defun org-queue-id-guard--postprocess-entry-vector (vec title preview)
  ;; Columns: 0 Mark, 1 Index, 2 Pri, 3 Title, 4 Preview, 5 Sched, 6 File
  (let* ((title-face 'org-queue-chooser-title-face)
         (preview-face 'org-queue-chooser-preview-face)
         (tw org-queue-chooser-title-width)
         (pw org-queue-chooser-preview-width)
         (tstr (org-queue-chooser--truncate-pad
                (org-queue-chooser--asciiize (org-queue-chooser--heading-as-plain (or title ""))) tw))
         (pstr (org-queue-chooser--truncate-pad
                (org-queue-chooser--asciiize (or preview "")) pw)))
    (aset vec 3 (propertize tstr 'face title-face))
    (aset vec 4 (propertize pstr 'face preview-face))
    vec))

(defun org-queue-id-guard--entries-around (orig-fn)
  (let* ((entries (funcall orig-fn))
         (lst (ignore-errors (org-queue-chooser--task-list))))
    (when (and (listp entries) (listp lst))
      (dolist (cell entries)
        (let* ((row-id (car cell))
               (vec (cadr cell))
               (task (and (numberp row-id) (nth row-id lst)))
               (tid (and (listp task) (plist-get task :id))))
          (when (and tid (org-queue--id-recently-failed-p tid))
            (org-queue-id-guard--postprocess-entry-vector
             vec "(unresolved ID)" "[will be auto-dropped by maintenance]")))))
    entries))

(unless (advice-member-p #'org-queue-id-guard--entries-around 'org-queue-chooser--entries)
  (advice-add 'org-queue-chooser--entries :around #'org-queue-id-guard--entries-around))

(defun org-queue-id-guard--visit-around (orig-fn)
  (let* ((row-id (tabulated-list-get-id))
         (lst (ignore-errors (org-queue-chooser--task-list)))
         (subset-p (and (boundp 'org-queue-chooser--subset-p) org-queue-chooser--subset-p)))
    (if (not (and (numberp row-id) (listp lst) (< row-id (length lst))))
        (funcall orig-fn)
      (let* ((task (nth row-id lst))
             (tid  (plist-get task :id))
             (bad  (or (and tid (org-queue--id-recently-failed-p tid))
                       (null (my-extract-marker task)))))
        (if (not bad)
            (funcall orig-fn)
          ;; Auto-remove & refresh silently.
          (if (not subset-p)
              (progn
                (ignore-errors
                  (let* ((key (org-queue-id-guard--task-key task))
                         (idx (and key
                                   (cl-position-if
                                    (lambda (t1)
                                      (equal key (org-queue-id-guard--task-key t1)))
                                    my-outstanding-tasks-list))))
                    (when (numberp idx)
                      (my-queue--remove-index idx t t))))
                (my-save-outstanding-tasks-to-file)
                (condition-case _ (org-queue-chooser-refresh) (error nil))
                (message "Removed unresolved item."))
            ;; subset: remove from local list only
            (setq org-queue-chooser--tasks
                  (append (seq-take org-queue-chooser--tasks row-id)
                          (seq-drop  org-queue-chooser--tasks (1+ row-id))))
            (condition-case _ (org-queue-chooser-refresh) (error nil))
            (message "Removed unresolved item (subset).")))))))

(unless (advice-member-p #'org-queue-id-guard--visit-around 'org-queue-chooser-visit)
  (advice-add 'org-queue-chooser-visit :around #'org-queue-id-guard--visit-around))
;; Apply the same guard to other-window visits (o)
(unless (advice-member-p #'org-queue-id-guard--visit-around 'org-queue-chooser-visit-other-window)
  (advice-add 'org-queue-chooser-visit-other-window :around #'org-queue-id-guard--visit-around))

;; ----------------------------------------------------------------------
;; Keep org-id DB warm (startup + after saving an .org file)
;; ----------------------------------------------------------------------

(add-hook 'emacs-startup-hook
          (lambda ()
            (ignore-errors
              (org-id-update-id-locations (org-queue-file-list) t))))

;; Coalesced, silent ID updates after save (debounced): no per-save scans.
(defcustom org-queue-id-guard-idle-secs 2
  "Idle seconds before running a coalesced org-id update."
  :type 'number :group 'org-queue)

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

(defun org-queue-id-guard--flush-id-updates ()
  "Flush the pending file set into a single silent org-id update."
  (let (files)
    (maphash (lambda (k _v) (push k files)) org-queue-id-guard--pending-files)
    (setq org-queue-id-guard--pending-files (make-hash-table :test 'equal))
    (when files
      (ignore-errors
        (org-id-update-id-locations files t)))))  ;; `t` = silent

(add-hook 'org-mode-hook
          (lambda ()
            (add-hook 'after-save-hook #'org-queue-id-guard--queue-id-update nil t)))

(provide 'org-queue-id-guard)
;;; org-queue-id-guard.el ends here