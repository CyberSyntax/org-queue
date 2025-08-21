;;; org-queue-srs-bridge.el --- Integration between org-queue tasks and org-srs reviews -*- lexical-binding: t -*-

;; This package provides seamless integration between tasks and spaced repetition.

            ;;; Code:

(require 'org)
(require 'cl-lib)

(defcustom org-queue-preinit-srs nil
  "If non-nil, attempt to pre-initialize org-srs during startup."
  :type 'boolean
  :group 'org-queue)

(defvar my-srs-reviews-exhausted nil)
(defvar my-srs-reviews-per-task 4)
(defvar my-srs-review-count 0)

(defun my-srs--ensure-loaded ()
  "Try to load org-srs and org-srs-review. Return non-nil on success."
  (and
   (or (featurep 'org-srs)     (require 'org-srs nil t))
   (or (featurep 'org-srs-log) (require 'org-srs-log nil t))
   (or (featurep 'org-srs-review) (require 'org-srs-review nil t))))

(defun my-srs--on-review-done (&rest _)
  "Called when org-srs signals that reviews are done."
  (save-some-buffers t (lambda () (and buffer-file-name (string-match-p "\\.org$" buffer-file-name))))
  (setq my-srs-reviews-exhausted t)
  (message "No more cards to review in this session.")
  (when (fboundp 'my-show-current-outstanding-task)
    (my-show-current-outstanding-task))
  ;; Optionally jump straight to Anki now that SRS is exhausted.
  (when (fboundp 'my-launch-anki)
    (my-launch-anki)))

(defun my-srs-start-reviews ()
  "Start SRS review session with robust loading and failure handling."
  (interactive)
  (if my-srs-reviews-exhausted
      (progn
        (message "Reviews exhausted for this session. Skipping.")
        (when (fboundp 'my-show-current-outstanding-task)
          (my-show-current-outstanding-task)))
    (unless (my-srs--ensure-loaded)
      ;; org-srs is not available at all. Mark exhausted and jump to Anki.
      (setq my-srs-reviews-exhausted t)
      (message "org-srs not available; skipping SRS and launching Anki.")
      (when (fboundp 'my-launch-anki)
        (my-launch-anki))
      (cl-return-from my-srs-start-reviews))

    (condition-case err
        (progn
          (setq my-srs-review-count 0)

          ;; Add counting advice once
          (dolist (fn '(org-srs-review-rate-easy
                        org-srs-review-rate-good
                        org-srs-review-rate-hard
                        org-srs-review-rate-again))
            (when (and (fboundp fn)
                       (not (advice-member-p #'my-srs-count-review fn)))
              (advice-add fn :after #'my-srs-count-review)))

          ;; Add "done" detector once
          (when (and (fboundp 'org-srs-review-message-review-done)
                     (not (advice-member-p #'my-srs--on-review-done 'org-srs-review-message-review-done)))
            (advice-add 'org-srs-review-message-review-done :before #'my-srs--on-review-done))

          ;; Start review session
          (if (fboundp 'org-srs-review-start)
              (progn
                (org-srs-review-start default-directory)
                ;; If nothing started (no items due), mark exhausted right away
                (run-at-time 0.02 nil
                             (lambda ()
                               (when (and (fboundp 'org-srs-reviewing-p)
                                          (not (org-srs-reviewing-p)))
                                 (setq my-srs-reviews-exhausted t)
                                 (message "No SRS cards to review right now.")
                                 (when (fboundp 'my-launch-anki)
                                   (my-launch-anki))))))
            (message "org-srs-review-start not found; cannot start SRS.")
            (setq my-srs-reviews-exhausted t)
            (when (fboundp 'my-launch-anki) (my-launch-anki))))
      (error
       (my-srs-remove-advice)
       (setq my-srs-reviews-exhausted t)
       (message "SRS error: %s" (error-message-string err))
       (when (fboundp 'my-launch-anki) (my-launch-anki))))))

(defun my-srs-count-review (&rest _)
  (setq my-srs-review-count (1+ my-srs-review-count))
  (when (>= my-srs-review-count my-srs-reviews-per-task)
    (message "Completed target of %d reviews." my-srs-reviews-per-task)
    (my-srs-exit-reviews)
    (when (fboundp 'my-show-current-outstanding-task)
      (my-show-current-outstanding-task))))

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

(defun my-srs-reset-exhausted-flag () (interactive)
  (setq my-srs-reviews-exhausted nil)
  (message "SRS reviews reset and available again."))

(defun my-srs-force-reset () (interactive)
  (setq my-srs-reviews-exhausted nil)
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
    (error nil))
  (when (and (current-message)
             (string-match-p "No more cards to review" (current-message)))
    (setq my-srs-reviews-exhausted t)
    (message "Review session complete.")))

(run-at-time "00:00" 86400 #'my-srs-reset-exhausted-flag)

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

(provide 'org-queue-srs-bridge)
;;; org-queue-srs-bridge.el ends here
