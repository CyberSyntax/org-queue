;;; org-queue-srs-bridge.el --- Integration between org-queue tasks and org-srs reviews -*- lexical-binding: t -*-

;; This package provides seamless integration between tasks and spaced repetition.

            ;;; Code:

(require 'org)
(require 'cl-lib)

(defcustom org-queue-preinit-srs nil
  "If non-nil, attempt to pre-initialize org-srs during startup."
  :type 'boolean
  :group 'org-queue)

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
        		;; Save any modified org buffers when reviews are exhausted
        		(save-some-buffers t (lambda ()
        				       (and buffer-file-name
        					    (string-match-p "\\.org$" buffer-file-name))))
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
  
  ;; Only try to quit if we're actually in a review session
  (condition-case nil
      (when (and (fboundp 'org-srs-reviewing-p) 
                 (org-srs-reviewing-p)
                 (fboundp 'org-srs-review-quit))
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

(defun my-srs-force-reset ()
  "Force a complete reset of the SRS integration state."
  (interactive)
  (setq my-srs-reviews-exhausted nil)
  (setq my-srs-review-count 0)
  (my-srs-remove-advice)
  (condition-case nil
      (when (fboundp 'org-srs-review-quit)
        (org-srs-review-quit))
    (error nil))
  (message "SRS integration state completely reset"))

;; Manually quit reviews
(defun my-srs-quit-reviews ()
  "Manually quit SRS review session with better state management."
  (interactive)
  ;; Remove advice first to prevent callback loops
  (my-srs-remove-advice)
  
  ;; Only try to quit if we're actually in a review session
  (condition-case nil
      (when (and (fboundp 'org-srs-reviewing-p) 
                 (org-srs-reviewing-p)
                 (fboundp 'org-srs-review-quit))
        (org-srs-review-quit))
    (error nil))
  
  ;; Check for "No more cards" message
  (when (and (current-message)
             (string-match-p "No more cards to review" (current-message)))
    (setq my-srs-reviews-exhausted t)
    (message "Review session complete.")))

;; Set reviews per task
(defun my-srs-set-reviews-per-task (n)
  "Set how many SRS reviews to do after each task."
  (interactive "nHow many reviews per task: ")
  (setq my-srs-reviews-per-task (max 1 n))
  (message "Will now do %d SRS reviews after each task" my-srs-reviews-per-task))

;; Reset exhausted flag at midnight
(run-at-time "00:00" 86400 #'my-srs-reset-exhausted-flag)

;; SRS entry detection function
(defun org-srs-entry-p (pos)
  "Determine if and where the Org entry at POS or its immediate parent contains
the specified log drawer (org-srs-log-drawer-name).

Returns:
- 'current : If the drawer is found directly under the current entry.
- 'parent  : If the drawer is found directly under the immediate parent entry 
            (and not under the current entry).
- nil      : If the drawer is not found in either location."
  (interactive (list (point)))

  ;; Try to load org-srs if not already loaded to get org-srs-log-drawer-name
  (unless (boundp 'org-srs-log-drawer-name)
    (if (require 'org-srs-log nil t)
        (message "Loaded org-srs-log for drawer name")
      ;; Fallback to known default if org-srs is not available
      (defvar org-srs-log-drawer-name "SRSITEMS"
        "Default SRS log drawer name when org-srs is not available.")))
  
  ;; Ensure the required variable is defined and not empty
  (unless (boundp 'org-srs-log-drawer-name)
    (error "Variable 'org-srs-log-drawer-name' is not defined. Set it to your drawer name"))
  (when (string-empty-p org-srs-log-drawer-name)
    (error "Variable 'org-srs-log-drawer-name' is empty. Set it to your drawer name"))

  ;; Save current buffer state
  (save-excursion
    ;; Ensure we're at a heading
    (goto-char pos)
    (when (or (org-at-heading-p) (org-back-to-heading t))
      (let* ((drawer-regexp (concat "^[ \t]*:" 
                                   (regexp-quote org-srs-log-drawer-name)
                                   ":[ \t]*$"))
             (location nil)
             (current-heading-pos (point))
             (current-level (org-outline-level)))
        
        ;; Check current entry first
        (let ((next-heading-pos (save-excursion
                                 (outline-next-heading)
                                 (point))))
          (save-excursion
            (forward-line 1) ;; Move past the heading
            (when (re-search-forward drawer-regexp next-heading-pos t)
              (setq location 'current))))
        
        ;; If not found in current entry and not at top level, check parent
        (unless location
          (when (> current-level 1)
            (save-excursion
              (goto-char current-heading-pos)
              (when (org-up-heading-safe)
                (let ((parent-pos (point))
                      (next-heading-pos (save-excursion
                                         (outline-next-heading)
                                         (point))))
                  (forward-line 1) ;; Move past the parent heading
                  (when (re-search-forward drawer-regexp next-heading-pos t)
                    (setq location 'parent)))))))
        
        (message "org-srs-entry-p: Result (for drawer '%s') = %s" 
                 org-srs-log-drawer-name location)
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

(provide 'org-queue-srs-bridge)
;;; org-queue-srs-bridge.el ends here
