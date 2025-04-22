;;; my-srs-integration.el --- Immediate SRS integration with advice approach

    ;;; Commentary:
;; This package provides immediate transition from SRS reviews to tasks
;; after reaching the goal count.

    ;;; Code:

(require 'org)

;; Customizable options
(defgroup my-srs-integration nil
  "Integration between task management and SRS reviews."
  :group 'org
  :prefix "my-srs-")

(defcustom my-srs-reviews-per-task 4
  "Target number of SRS reviews to do after each task."
  :type 'integer
  :group 'my-srs-integration)

;; Internal variables
(defvar my-srs-review-count 0
  "Counter for completed reviews in current session.")

(defvar my-srs-reached-goal nil
  "Flag indicating we've reached our review goal.")

;; Helper function for symbols
(defun my-srs--get-symbol (type)
  "Get symbol for given TYPE."
  (pcase type
    ('complete "✓")
    ('progress "•")
    ('info "→")
    ('warn "⚠")
    (_ "")))

;; Count reviews and handle goal completion
(defun my-srs-count-review (&rest _)
  "Count reviews and handle goal completion."
  (setq my-srs-review-count (1+ my-srs-review-count))
  
  (if (>= my-srs-review-count my-srs-reviews-per-task)
      (progn
        ;; We've reached our goal
        (setq my-srs-reached-goal t)
        (message "%s Goal of %d reviews reached!" 
                 (my-srs--get-symbol 'complete)
                 my-srs-reviews-per-task)
        
        ;; Exit immediately after this card
        (my-srs-exit-and-show-task))
    
    ;; Not yet reached goal, show progress
    (message "%s Review %d/%d completed" 
             (my-srs--get-symbol 'progress)
             my-srs-review-count 
             my-srs-reviews-per-task)))

;; Exit review mode and show task immediately
(defun my-srs-exit-and-show-task ()
  "Exit review mode and immediately show current task."
  ;; Remove all advice first
  (my-srs-remove-advice)
  
  ;; Block the next card if one is about to be shown
  (when (fboundp 'org-srs-review-show)
    (advice-add 'org-srs-review-show :before-until 
                (lambda (&rest _) 
                  (my-srs-remove-advice)
                  t)))
  
  ;; Save any modified SRS buffers
  (save-some-buffers t (lambda ()
                         (and buffer-file-name
                              (or (string-match-p "\.org$" buffer-file-name)
                                  (string-match-p "srs" buffer-file-name)))))
  
  ;; Exit review mode
  (condition-case nil
      (when (fboundp 'org-srs-review-quit)
        (org-srs-review-quit))
    (error nil))
  
  ;; Reset state
  (setq my-srs-reached-goal nil)
  
  ;; Immediately show the current task
  (when (fboundp 'my-show-current-outstanding-task)
    (my-show-current-outstanding-task)))

;; Set up advice for rating functions
(defun my-srs-add-advice ()
  "Add advice to org-srs rating functions."
  ;; Reset state
  (setq my-srs-review-count 0)
  (setq my-srs-reached-goal nil)
  
  ;; First remove any existing advice
  (my-srs-remove-advice)
  
  ;; Add advice to all rating functions
  (dolist (rating-func '(org-srs-review-rate-easy
                         org-srs-review-rate-good
                         org-srs-review-rate-hard
                         org-srs-review-rate-again))
    (when (fboundp rating-func)
      (advice-add rating-func :after #'my-srs-count-review))))

;; Remove all advice
(defun my-srs-remove-advice ()
  "Remove all advice from org-srs functions."
  ;; Remove counting advice
  (dolist (rating-func '(org-srs-review-rate-easy
                         org-srs-review-rate-good
                         org-srs-review-rate-hard
                         org-srs-review-rate-again))
    (when (fboundp rating-func)
      (advice-remove rating-func #'my-srs-count-review)))
  
  ;; Remove any blocking advice
  (when (fboundp 'org-srs-review-show)
    (advice-remove 'org-srs-review-show #'my-srs-prevent-next-card)
    (advice-remove 'org-srs-review-show (lambda (&rest _) t))))

;; Start reviews with advice - always restart if one exists
(defun my-srs-start-reviews ()
  "Start SRS review session with counting and immediate exit at goal."
  (interactive)
  
  ;; Check if there's already a review session active
  (when (and (boundp 'org-srs-review-item-marker)
             (local-variable-p 'org-srs-review-item-marker))
    ;; There's an existing review - always exit it first
    (message "%s Existing review session detected. Restarting..." 
             (my-srs--get-symbol 'info))
    
    ;; Try to exit the current session cleanly
    (condition-case nil
        (when (fboundp 'org-srs-review-quit)
          (org-srs-review-quit))
      (error nil))
    
    ;; Wait a moment to ensure everything is cleaned up
    (sit-for 0.2))
  
  ;; Start a fresh review session
  (condition-case err
      (progn
        (require 'org-srs)
        
        ;; Add advice to control the review flow
        (my-srs-add-advice)
        
        ;; Start the review session
        (when (fboundp 'org-srs-review-start)
          (let ((dir (cond ((and (boundp 'org-agenda-directory)
                                 (stringp org-agenda-directory)
                                 (file-directory-p org-agenda-directory))
                            org-agenda-directory)
                           ((file-directory-p default-directory)
                            default-directory)
                           (t
                            (user-error "No suitable directory for SRS reviews")))))
            (message "%s Starting SRS reviews (will exit after %d)..." 
                     (my-srs--get-symbol 'info)
                     my-srs-reviews-per-task)
            (org-srs-review-start dir))))
    (error
     ;; Remove advice on error
     (my-srs-remove-advice)
     (message "Error starting SRS reviews: %s" (error-message-string err)))))

;; Quit reviews manually
(defun my-srs-quit-reviews ()
  "Manually quit SRS review session and immediately show task."
  (interactive)
  
  ;; Capture review count
  (let ((count my-srs-review-count))
    
    ;; Remove all advice
    (my-srs-remove-advice)
    
    ;; Try to exit org-srs review mode
    (condition-case nil
        (when (fboundp 'org-srs-review-quit)
          (org-srs-review-quit))
      (error nil))
    
    ;; Reset flags
    (setq my-srs-reached-goal nil)
    
    ;; Show summary
    (message "%s Completed %d/%d reviews" 
             (my-srs--get-symbol 'complete)
             count my-srs-reviews-per-task)
    
    ;; Immediately show the next task 
    (when (fboundp 'my-show-current-outstanding-task)
      (my-show-current-outstanding-task))))

;; Set review target
(defun my-srs-set-target (n)
  "Set target number of reviews per task."
  (interactive "nTarget reviews per task: ")
  (setq my-srs-reviews-per-task (max 1 n))
  (message "Target set to %d reviews per task" my-srs-reviews-per-task))

;; Setup keybindings
(defun my-srs-setup-keybindings ()
  "Set up keybindings for SRS integration."
  (interactive)
  (when (boundp 'org-queue-mode-map)
    (define-key org-queue-mode-map (kbd "r") #'my-srs-start-reviews)
    (define-key org-queue-mode-map (kbd "q") #'my-srs-quit-reviews)
    (define-key org-queue-mode-map (kbd "C-c t") #'my-srs-set-target)))

(my-srs-setup-keybindings)

(provide 'my-srs-integration)
    ;;; my-srs-integration.el ends here
