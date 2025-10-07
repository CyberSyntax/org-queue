;;; org-queue-srs-bridge.el --- Integration between org-queue tasks and org-srs reviews -*- lexical-binding: t -*-

;; This package provides seamless integration between tasks and spaced repetition.

            ;;; Code:

(require 'org)
(require 'cl-lib)
(require 'org-queue-utils)

(defcustom org-queue-preinit-srs nil
  "If non-nil, attempt to pre-initialize org-srs during startup."
  :type 'boolean
  :group 'org-queue)

(defvar my-srs-reviews-per-task 4)
(defvar my-srs-review-count 0)

(defun my-srs--ensure-loaded ()
  "Try to load org-srs and org-srs-review. Return non-nil on success."
  (and
   (or (featurep 'org-srs)     (require 'org-srs nil t))
   (or (featurep 'org-srs-log) (require 'org-srs-log nil t))
   (or (featurep 'org-srs-review) (require 'org-srs-review nil t))))

(defun my-srs--on-review-done (&rest _)
  (save-some-buffers t (lambda () (and buffer-file-name (string-match-p "\\.org$" buffer-file-name))))
  (run-at-time 0.05 nil
               (lambda ()
                 (when (and (boundp 'my-outstanding-tasks-list)
                            my-outstanding-tasks-list)
                   (my-show-current-outstanding-task-no-srs t))))
  (message "No more cards to review in this session."))

(defun my-srs-start-reviews ()
  "Start SRS review session; do not call task display or Anki here."
  (interactive)
  (unless (my-srs--ensure-loaded)
    (message "org-srs not available; skipping SRS.")
    (cl-return-from my-srs-start-reviews))
  (condition-case err
      (progn
        (setq my-srs-review-count 0)
        (dolist (fn '(org-srs-review-rate-easy
                      org-srs-review-rate-good
                      org-srs-review-rate-hard
                      org-srs-review-rate-again))
          (when (and (fboundp fn)
                     (not (advice-member-p #'my-srs-count-review fn)))
            (advice-add fn :after #'my-srs-count-review)))
        (when (and (fboundp 'org-srs-review-message-review-done)
                   (not (advice-member-p #'my-srs--on-review-done
                                         'org-srs-review-message-review-done)))
          (advice-add 'org-srs-review-message-review-done :before #'my-srs--on-review-done))
        (if (fboundp 'org-srs-review-start)
            (progn
              (org-srs-review-start default-directory)
              (run-at-time 0.02 nil
                           (lambda ()
                             (when (and (fboundp 'org-srs-reviewing-p)
                                        (not (org-srs-reviewing-p)))
                               (message "No SRS cards to review right now.")))))
          (message "org-srs-review-start not found; cannot start SRS.")))
    (error
     (my-srs-remove-advice)
     (message "SRS error: %s" (error-message-string err)))))

(defun my-srs-count-review (&rest _)
  (setq my-srs-review-count (1+ my-srs-review-count))
  (when (>= my-srs-review-count my-srs-reviews-per-task)
    (message "Completed target of %d reviews." my-srs-reviews-per-task)
    (my-srs-exit-reviews)))

(defun my-srs-exit-reviews ()
  (my-srs-remove-advice)
  (save-some-buffers t (lambda () (and buffer-file-name (string-match-p "\\.org$" buffer-file-name))))
  (condition-case nil
      (when (and (fboundp 'org-srs-reviewing-p)
                 (org-srs-reviewing-p)
                 (fboundp 'org-srs-review-quit))
        (org-srs-review-quit))
    (error nil)))

(defun my-srs-remove-advice ()
  (dolist (fn '(org-srs-review-rate-easy
                org-srs-review-rate-good
                org-srs-review-rate-hard
                org-srs-review-rate-again))
    (ignore-errors (advice-remove fn #'my-srs-count-review)))
  (ignore-errors
    (advice-remove 'org-srs-review-message-review-done #'my-srs--on-review-done)))

(defun my-srs-force-reset () (interactive)
  (setq my-srs-review-count 0)
  (my-srs-remove-advice)
  (condition-case nil
      (when (fboundp 'org-srs-review-quit) (org-srs-review-quit))
    (error nil))
  (message "SRS integration state completely reset"))

(defun my-srs-quit-reviews () (interactive)
  (my-srs-remove-advice)
  (condition-case nil
      (when (and (fboundp 'org-srs-reviewing-p)
                 (org-srs-reviewing-p)
                 (fboundp 'org-srs-review-quit))
        (org-srs-review-quit))
    (error nil)))

(defun org-srs-entry-p (pos)
  ;; unchanged logic from your file, but keep requiring org-srs-log if available
  (interactive (list (point)))
  (unless (boundp 'org-srs-log-drawer-name)
    (require 'org-srs-log nil t)
    (unless (boundp 'org-srs-log-drawer-name)
      (defvar org-srs-log-drawer-name "SRSITEMS")))

  (save-excursion
    (goto-char pos)
    (when (or (org-at-heading-p) (org-back-to-heading t))
      (let* ((drawer-regexp (concat "^[ \t]*:" (regexp-quote org-srs-log-drawer-name) ":[ \t]*$"))
             (location nil)
             (current-heading-pos (point))
             (current-level (org-outline-level)))
        (let ((next-heading-pos (save-excursion (outline-next-heading) (point))))
          (save-excursion
            (forward-line 1)
            (when (re-search-forward drawer-regexp next-heading-pos t)
              (setq location 'current))))
        (unless location
          (when (> current-level 1)
            (save-excursion
              (goto-char current-heading-pos)
              (when (org-up-heading-safe)
                (let ((next-heading-pos (save-excursion (outline-next-heading) (point))))
                  (forward-line 1)
                  (when (re-search-forward drawer-regexp next-heading-pos t)
                    (setq location 'parent)))))))
        location))))

;; Modern SRS rating functions for use outside review sessions
;; Based on your suggested org-srs-review-rate-entry function
(defun org-srs-review-rate-entry (rating)
  "Rate entry at point using modern org-srs API."
  (org-srs-property-let ((org-srs-review-cache-p nil))
    (apply
     #'org-srs-review-rate
     rating
     (save-excursion
       (org-srs-log-beginning-of-drawer)
       (forward-line)
       (org-srs-item-at-point)))))

(defun org-queue-srs-rate-again ()
  "Rate current entry as 'again' using org-srs-review-rate-entry."
  (interactive)
  (if (not (require 'org-srs nil t))
      (message "org-srs package not available")
    (condition-case err
        (progn
          (org-srs-review-rate-entry :again)
          (message "Rated as 'again'"))
      (error 
       (message "Error rating entry: %s" (error-message-string err))))))

(defun org-queue-srs-rate-good ()
  "Rate current entry as 'good' using org-srs-review-rate-entry."
  (interactive)
  (if (not (require 'org-srs nil t))
      (message "org-srs package not available")
    (condition-case err
        (progn
          (org-srs-review-rate-entry :good)
          (message "Rated as 'good'"))
      (error 
       (message "Error rating entry: %s" (error-message-string err))))))

(defun org-queue-srs-item-create-card ()
  "Create an org-srs review item of type 'card' at the current entry without prompting.
Ensures the entry has an ID. Gracefully degrades if org-srs is not available."
  (interactive)
  (if (not (require 'org-srs nil t))
      (message "org-srs package not available")
    (require 'org-id)
    (condition-case err
        (progn
          (org-id-get-create)
          (cond
           ((fboundp 'org-srs-item-new)
            (org-srs-item-new 'card))
           ((fboundp 'org-srs-item-new-interactively)
            ;; Keep the spirit of org-srs-item-create's prog1 form:
            ;; ensure ID, then pass 'card as the chosen type.
            (org-srs-item-new-interactively 'card))
           (t
            (user-error "org-srs: no item creation API found")))
          (message "Created org-srs card"))
      (error
       (user-error "Failed to create SRS card: %s" (error-message-string err))))))

(defun org-queue-srs--drawer-bounds (&optional pos)
  "Return cons (beg . end) of :SRSITEMS: drawer in entry at POS (default point), or nil."
  (save-excursion
    (when pos (goto-char pos))
    (org-back-to-heading t)
    (let* ((limit (save-excursion (outline-next-heading) (point)))
           (drawer (or (and (boundp 'org-srs-log-drawer-name) org-srs-log-drawer-name) "SRSITEMS"))
           (beg (save-excursion
                  (forward-line 1)
                  (when (re-search-forward
                         (concat "^[ \t]*:" (regexp-quote drawer) ":[ \t]*$")
                         limit t)
                    (match-beginning 0))))
           (end (and beg (save-excursion
                           (goto-char beg)
                           (re-search-forward "^[ \t]*:END:[ \t]*$" limit t)))))
      (when (and beg end) (cons beg end)))))

(defun org-queue-srs--parse-table-rows (beg end)
  "Return a list of alists for each data row in the SRS table between BEG and END.
Each alist has keys: :mark (string), :timestamp (string), :time (Emacs time or nil), :raw."
  (let ((rows '()))
    (save-excursion
      (goto-char beg)
      (while (re-search-forward "^\\s-*|.*|.*|.*$" end t)
        (let ((line (buffer-substring-no-properties (line-beginning-position) (line-end-position))))
          (when (and (string-match-p "^\\s-*|\\([^|]*\\)|\\([^|]*\\)|" line)
                     (not (string-match-p "^\\s-*|-[+-]" line)) ; skip table rule rows
                     (not (string-match-p "^\\s-*|\\s*!\\s*|\\s*timestamp\\s*|" line))) ; skip header
            (let* ((cells (mapcar #'string-trim (split-string line "|")))
                   ;; cells[0] is "", so real columns start at 1
                   (mark (nth 1 cells))
                   (ts   (nth 2 cells))
                   (tm   (ignore-errors (and ts (> (length ts) 0) (date-to-time ts)))))
              (push (list :mark mark :timestamp ts :time tm :raw line) rows)))))
      (nreverse rows))))

(defun org-queue-srs-next-due-time (&optional pos)
  "Heuristically return next-due time from SRSITEMS drawer at POS (or point).
Prefer the row with mark \"*\" in the first column; else the earliest valid timestamp."
  (let* ((bounds (org-queue-srs--drawer-bounds pos)))
    (when bounds
      (let* ((rows (org-queue-srs--parse-table-rows (car bounds) (cdr bounds)))
             (star (cl-find-if (lambda (r) (string= (string-trim (or (plist-get r :mark) "")) "*")) rows))
             (times (delq nil (mapcar (lambda (r) (plist-get r :time)) rows))))
        (or (and star (plist-get star :time))
            (car (sort (copy-sequence times) #'time-less-p)))))))

(defun org-queue-collect-srs-due-items ()
  "Collect SRS entries due now (timestamp <= now) as task plists.
Each plist contains: :id :marker :priority :flag :file :heading :pos :srs t :srs-due <time>."
  (let ((now (current-time))
        (items '()))
    (org-queue-map-entries
     (lambda ()
       (when (eq (org-srs-entry-p (point)) 'current)
         (let ((due (org-queue-srs-next-due-time (point))))
           (when (and due (not (time-less-p now due))) ; due <= now
             (let* ((marker (point-marker))
                    (id (or (org-entry-get nil "ID") (org-id-get-create)))
                    (priority (or (and (fboundp 'my-get-raw-priority-value)
                                       (my-get-raw-priority-value))
                                  (let ((ps (org-entry-get nil "PRIORITY")))
                                    (if ps (string-to-number ps) org-priority-default))))
                    (flag (and (fboundp 'my-priority-flag) (my-priority-flag priority)))
                    (file (buffer-file-name))
                    (heading (org-get-heading t t t t))
                    (pos (point))
                    (task (list :id id :marker marker :priority priority
                                :flag flag :file file :is-todo nil
                                :heading heading :pos pos
                                :srs t :srs-due due)))
               (push task items)))))) nil)
    (sort items (lambda (a b) (time-less-p (plist-get a :srs-due)
                                           (plist-get b :srs-due))))))

(provide 'org-queue-srs-bridge)
;;; org-queue-srs-bridge.el ends here
