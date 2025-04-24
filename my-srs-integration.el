;;; my-srs-integration.el --- Integration between tasks and SRS reviews -*- lexical-binding: t -*-

;; This package provides seamless integration between tasks and spaced repetition.

  ;;; Code:

(require 'org)

;; State variables
(defvar my-srs-reviews-exhausted nil
  "Flag indicating if org-srs reviews are exhausted for this session.")

(defvar my-srs-reviews-per-task 4
  "Number of SRS reviews to do after each task.")

(defvar my-srs-review-count 0
  "Counter for completed reviews in current session.")

;; Main entry point for starting SRS reviews
(defun my-srs-start-reviews ()
  "Start SRS review session with graceful handling of exhausted reviews."
  (interactive)
  
  ;; Skip if we already know reviews are exhausted
  (if my-srs-reviews-exhausted
      (progn
        (message "Reviews exhausted for this session. Skipping.")
        ;; Return to current task
        (when (fboundp 'my-show-current-outstanding-task)
          (my-show-current-outstanding-task)))
    
    ;; Try to start reviews
    (condition-case err
        (progn
          ;; Reset counter for this review batch
          (setq my-srs-review-count 0)
          
          ;; Add advice to exit reviews after reaching target count
          (advice-add 'org-srs-review-rate-easy :after #'my-srs-count-review)
          (advice-add 'org-srs-review-rate-good :after #'my-srs-count-review)
          (advice-add 'org-srs-review-rate-hard :after #'my-srs-count-review)
          (advice-add 'org-srs-review-rate-again :after #'my-srs-count-review)
          
          ;; Detect when reviews are finished
          (advice-add 'org-srs-review-message-review-done :before 
                      (lambda (&rest _)
                        (setq my-srs-reviews-exhausted t)
                        (message "No more cards to review in this session.")
                        ;; Return to current task
                        (when (fboundp 'my-show-current-outstanding-task)
                          (my-show-current-outstanding-task))))
          
          ;; Start the review session
          (when (fboundp 'org-srs-review-start)
            (org-srs-review-start default-directory)))
      
      ;; Handle errors gracefully
      (error
       ;; Clean up advice
       (my-srs-remove-advice)
       
       ;; Check if this is the "review done" assertion error from org-srs
       (when (string-match-p "assertion.*nil" (error-message-string err))
         (setq my-srs-reviews-exhausted t)
         (message "No more cards to review in this session.")
         ;; Return to current task
         (when (fboundp 'my-show-current-outstanding-task)
           (my-show-current-outstanding-task)))))))

;; Count reviews and exit after reaching target
(defun my-srs-count-review (&rest _)
  "Count reviews and handle reaching the target count."
  (setq my-srs-review-count (1+ my-srs-review-count))
  
  (when (>= my-srs-review-count my-srs-reviews-per-task)
    (message "Completed target of %d reviews." my-srs-reviews-per-task)
    (my-srs-exit-reviews)
    
    ;; Return to current task after reaching target
    (when (fboundp 'my-show-current-outstanding-task)
      (my-show-current-outstanding-task))))

(defun my-srs-exit-reviews ()
  "Exit review mode and clean up advice."
  (my-srs-remove-advice)
  
  ;; Save any modified org buffers
  (save-some-buffers t (lambda ()
                         (and buffer-file-name
                              (string-match-p "\\.org$" buffer-file-name))))
  
  ;; Try to quit reviews properly
  (condition-case nil
      (when (fboundp 'org-srs-review-quit)
        (org-srs-review-quit))
    (error nil)))

;; Clean up advice
(defun my-srs-remove-advice ()
  "Remove all advice from org-srs functions."
  (advice-remove 'org-srs-review-rate-easy #'my-srs-count-review)
  (advice-remove 'org-srs-review-rate-good #'my-srs-count-review)
  (advice-remove 'org-srs-review-rate-hard #'my-srs-count-review)
  (advice-remove 'org-srs-review-rate-again #'my-srs-count-review)
  (advice-remove 'org-srs-review-message-review-done 
                 (lambda (&rest _)
                   (setq my-srs-reviews-exhausted t)
                   (message "No more cards to review in this session."))))

;; Reset the exhausted flag (e.g., at midnight or when restarting Emacs)
(defun my-srs-reset-exhausted-flag ()
  "Reset the flag indicating reviews are exhausted."
  (interactive)
  (setq my-srs-reviews-exhausted nil)
  (message "SRS reviews reset and available again."))

;; Manually quit reviews
(defun my-srs-quit-reviews ()
  "Manually quit SRS review session."
  (interactive)
  (condition-case nil
      (when (fboundp 'org-srs-review-quit)
        (org-srs-review-quit))
    (error nil))
  (my-srs-remove-advice))

;; Set reviews per task
(defun my-srs-set-reviews-per-task (n)
  "Set how many SRS reviews to do after each task."
  (interactive "nHow many reviews per task: ")
  (setq my-srs-reviews-per-task (max 1 n))
  (message "Will now do %d SRS reviews after each task" my-srs-reviews-per-task))

;; Reset exhausted flag at midnight
(run-at-time "00:00" 86400 #'my-srs-reset-exhausted-flag)

(provide 'my-srs-integration)
  ;;; my-srs-integration.el ends here
