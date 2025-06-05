(require 'org-agenda)
(require 'cl-lib)  ;; Required for cl-find-if and cl-remove-if-not

(defgroup org-queue nil
  "Task queue management for Org mode."
  :group 'org
  :prefix "org-queue-")

;; Disable large file warning
(setq large-file-warning-threshold nil)

(random t)

(defun random-float (min max)
  "Return a random float between MIN and MAX."
  (+ min (* (- max min) (/ (float (random 1000000)) 1000000))))

(setq org-priority-highest 1)
(setq org-priority-default 32)
(setq org-priority-lowest 64)

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

(defvar my-android-p 
  (eq system-type 'android)
  "Non-nil if running on Android.")

(defvar org-queue-mode-map (make-sparse-keymap)
  "Keymap for org-queue-mode.")

;; Create prefix keymap for Android compatibility
(defvar org-queue-prefix-map (make-sparse-keymap)
  "Prefix keymap for org-queue commands.")

(progn
  ;; Define all commands in BOTH keymaps (no redundancy - single definition)
  (dolist (binding '(("t" org-queue-toggle-auto-enable)
                     ("," my-set-priority-with-heuristics)
                     ("s" my-schedule-command)
                     ("f" my-show-next-outstanding-task)
                     ("b" my-show-previous-outstanding-task)
                     ("c" my-show-current-outstanding-task)
                     ("r" my-remove-current-task)
                     ("R" my-reset-and-show-current-outstanding-task)
                     ("i" my-increase-priority-range)
                     ("d" my-decrease-priority-range)
                     ("D" org-demote-subtree)
                     ("a" my-advance-schedule)
                     ("p" my-postpone-schedule)
                     ("P" org-promote-subtree)
                     ("n" org-narrow-to-subtree)
                     ("w" widen-and-recenter)
                     ("W" org-cut-subtree)
                     ("Y" org-paste-subtree)
                     ("u" org-show-parent-heading-cleanly)
                     ("x" org-interactive-extract)
                     ("X" org-remove-all-extract-blocks)))
    (let ((key (car binding))
          (command (cadr binding)))
      (define-key org-queue-mode-map (kbd key) command)
      (define-key org-queue-prefix-map (kbd key) command)))
  
  ;; Optional commands (only if packages exist)
  (when (require 'org-web-tools nil t)
    (define-key org-queue-mode-map (kbd "l") #'org-web-tools-insert-link-for-url)
    (define-key org-queue-prefix-map (kbd "l") #'org-web-tools-insert-link-for-url)
    (define-key org-queue-mode-map (kbd "I") #'org-web-tools-insert-web-page-as-entry)
    (define-key org-queue-prefix-map (kbd "I") #'org-web-tools-insert-web-page-as-entry))
  (when (require 'gptel nil t)
    (define-key org-queue-mode-map (kbd "g") #'gptel)
    (define-key org-queue-prefix-map (kbd "g") #'gptel))
  (when (require 'org-srs nil t)
    (define-key org-queue-mode-map (kbd "1") #'org-srs-review-rate-again)
    (define-key org-queue-prefix-map (kbd "1") #'org-srs-review-rate-again)
    (define-key org-queue-mode-map (kbd "3") #'org-srs-review-rate-good)
    (define-key org-queue-prefix-map (kbd "3") #'org-srs-review-rate-good)
    (define-key org-queue-mode-map (kbd "z") #'org-interactive-cloze)
    (define-key org-queue-prefix-map (kbd "z") #'org-interactive-cloze))
  
  ;; Exit key
  (let ((exit-cmd (lambda () (interactive) (org-queue-mode -1))))
    (define-key org-queue-mode-map (kbd "e") exit-cmd)
    (define-key org-queue-prefix-map (kbd "e") exit-cmd)))

;; Bind the prefix globally to C-;
(global-set-key (kbd "C-;") org-queue-prefix-map)

;; Only block keys if NOT on Android
(unless my-android-p
  ;; Define a simple ignore function - no conditionals
  (defun org-queue-ignore ()
    "Ignore key presses unconditionally."
    (interactive)
    (message "Key blocked by org-queue-mode (press e to exit)")
    (ignore))

  ;; Block standard typing keys (ASCII 32-126)
  (dolist (char (number-sequence 32 126))
    (let ((key (char-to-string char)))
      (unless (lookup-key org-queue-mode-map (kbd key))
        (define-key org-queue-mode-map (kbd key) #'org-queue-ignore))))

  ;; Block common editing keys
  (dolist (key-binding '("<backspace>" "<delete>" "<deletechar>"
                        "<return>" "RET" "DEL"))
    (unless (lookup-key org-queue-mode-map (kbd key-binding))
      (define-key org-queue-mode-map (kbd key-binding) #'org-queue-ignore)))

  ;; Block pasting and other input-adding commands
  (dolist (input-key '("C-y" "C-d" "C-k" "C-o" "C-j" "C-m"))
    (define-key org-queue-mode-map (kbd input-key) #'org-queue-ignore))

  ;; Explicitly allow navigation keys and copy operations
  (dolist (allowed-key '("<tab>" "TAB" "<up>" "<down>" "<left>" "<right>"
                        "<prior>" "<next>" "<home>" "<end>" 
                        "C-v" "M-v" "C-l" "C-n" "C-p"
                        "C-c" "M-w" "C-w"))  ;; allow copying and cutting
    (define-key org-queue-mode-map (kbd allowed-key) nil)))

;; State tracking variables
(defvar org-queue--original-cursor nil
  "Persistent cursor state storage")

;; Define custom face for the lighter
(defface org-queue-mode-line-face
  '((t :inherit font-lock-builtin-face
       :weight bold
       :foreground "#B71C1C"
       :background "#FFCDD2"))
  "Face for org-queue mode lighter.")

;; The core minor mode with colored lighter
(define-minor-mode org-queue-mode
  "Global minor mode for task queue management."
  :init-value nil
  :global t
  :keymap org-queue-mode-map
  :lighter (:propertize " OrgQ" face org-queue-mode-line-face)
  (if org-queue-mode
      (progn
        (setq org-queue--original-cursor cursor-type
              cursor-type '(box . 3)  ; Thicker box cursor
              blink-cursor-blinks 0
              blink-cursor-interval 0.7))
    ;; Restore cursor on disable
    (setq cursor-type org-queue--original-cursor
          blink-cursor-blinks 40
          blink-cursor-interval 0.5)))

(defcustom org-queue-auto-enable nil
  "When non-nil, org-queue-mode will automatically activate after idle time.
Set this to nil to prevent automatic activation."
  :type 'boolean
  :group 'org-queue)

;; Auto-enable timer (if needed)
(defvar org-queue--auto-timer nil)

(defun org-queue-setup-auto-timer ()
  (when org-queue--auto-timer
    (cancel-timer org-queue--auto-timer)
    (setq org-queue--auto-timer nil))
  
  (when org-queue-auto-enable
    (setq org-queue--auto-timer
          (run-with-idle-timer 0.618 t
            (lambda (&rest _)
              (when (and org-queue-auto-enable (not org-queue-mode))
                (org-queue-mode 1)))))))

(org-queue-setup-auto-timer)

(defun org-queue-toggle-auto-enable ()
  "Toggle automatic activation of org-queue-mode.
When enabling auto-activation, immediately activates org-queue-mode.
When disabling auto-activation, immediately deactivates org-queue-mode."
  (interactive)
  (setq org-queue-auto-enable (not org-queue-auto-enable))
  (org-queue-setup-auto-timer)
  
  ;; Immediately apply the new state
  (if org-queue-auto-enable
      (progn
        (org-queue-mode 1)  ; Enable org-queue-mode
        (message "Org-queue auto-activation enabled and mode activated"))
    (progn
      (org-queue-mode -1)  ; Disable org-queue-mode
      (message "Org-queue auto-activation disabled and mode deactivated"))))

(defun my-enable-org-queue-mode ()
  (interactive)
  (org-queue-mode 1))

;; Bind C-c q to enable org-queue-mode.
(global-set-key (kbd "C-c q") 'my-enable-org-queue-mode)

;; Bind <escape> to enable org-queue-mode
(global-set-key (kbd "<escape>") 'my-enable-org-queue-mode)

(require 'my-srs-integration)

(defun org-interactive-cloze ()
  "Create a cloze deletion from selected text and generate a proper child heading with org-srs."
  (interactive)
  (if (not (use-region-p))
      (message "Please select text to create a cloze")
    (let* ((start (region-beginning))
           (end (region-end))
           (selected-text (buffer-substring-no-properties start end))
           ;; Get current heading position and level
           (heading-pos (save-excursion (org-back-to-heading) (point)))
           (heading-level (save-excursion (goto-char heading-pos) (org-outline-level)))
           ;; Get exact line where cursor is for content
           (content-line (buffer-substring-no-properties
                         (line-beginning-position)
                         (line-end-position))))
      
      ;; Replace selected text with cloze
      (delete-region start end)
      (insert (format "{{clozed:%s}}" selected-text))
      
      ;; Get updated content line
      (setq content-line (buffer-substring-no-properties
                         (line-beginning-position)
                         (line-end-position)))
      
      ;; Create child heading with cloze placeholder
      (save-excursion
        (goto-char heading-pos)
        (org-end-of-subtree)
        
        ;; Insert position for the new heading
        (let ((new-heading-pos (point)))
          ;; Create the child heading with proper level
          (insert "\n" (make-string (1+ heading-level) ?*) " ")
          
          ;; Create child content - replace the current cloze with [...]
          (let ((child-content 
                 (replace-regexp-in-string 
                  (format "{{clozed:%s}}" selected-text)
                  "[...]"
                  content-line)))
            ;; Replace all other clozes with their content
            (let ((processed-content child-content))
              (while (string-match "{{clozed:\\([^}]+\\)}}" processed-content)
                (setq processed-content 
                      (replace-match "\\1" t nil processed-content)))
              (insert processed-content))
            (insert "\n" selected-text))
          
          ;; Now set up org-srs for this child heading
          (goto-char (1+ new-heading-pos))  ;; Go to the new heading
          (org-id-get-create)               ;; Create ID for org-srs
          (org-srs-item-new 'card)          ;; Set up as org-srs card
        ))
      
      (message "Created cloze deletion with org-srs card"))))

(defun org-interactive-extract ()
  "Create a SuperMemo-style extract from selected text and generate a child heading."
  (interactive)
  (if (not (use-region-p))
      (message "Please select text to extract")
    (let* ((start (region-beginning))
           (end (region-end))
           (selected-text (buffer-substring-no-properties start end))
           ;; Find the parent heading for the selection start point
           (heading-pos (save-excursion 
                          (goto-char start)
                          (org-back-to-heading t) 
                          (point)))
           (heading-level (save-excursion 
                            (goto-char heading-pos) 
                            (org-current-level)))
           (parent-priority-range (save-excursion 
                                   (goto-char heading-pos)
                                   (my-get-current-priority-range)))
           (cleaned-text selected-text)
           ;; Check if selection ends with a newline
           (ends-with-newline (and (> end start)
                                  (= (char-before end) ?\n))))
      
      ;; Remove ONLY citations (like ^{[[#cite_note-192][[192 ]]]} and ^{: 4 })
      (setq cleaned-text (replace-regexp-in-string "\\^{[^}]*}" "" cleaned-text))
      
      ;; Fix double spaces caused by deletions
      (setq cleaned-text (replace-regexp-in-string "  +" " " cleaned-text))
      
      ;; Replace original text with extract marker
      (delete-region start end)
      
      (if ends-with-newline
          ;; If selection ends with newline, place }} before the newline
          (let ((text-without-newline (substring selected-text 0 -1)))
            (insert (format "{{extract:%s}}\n" text-without-newline)))
        ;; Normal case
        (insert (format "{{extract:%s}}" selected-text)))
      
      ;; Create child heading with cleaned text
      (save-excursion
        (goto-char heading-pos)
        (org-end-of-subtree)
        (let ((new-heading-pos (point)))
          (insert "\n" (make-string (1+ heading-level) ?*) " ")
          (insert "\n" cleaned-text)
          (when parent-priority-range
            (goto-char (1+ new-heading-pos))
            (my-set-priority-with-heuristics parent-priority-range))))
      (message "Created extract from selected text"))))

(defface org-clozed-face
  '((t (:background "#E67300" :foreground "black")))
  "Face for clozed text in org-mode.")

(defface org-extract-face
  '((t (:background "#44C2FF" :foreground "black")))
  "Face for extracted text in org-mode.")

(defvar-local org-custom-overlays nil
  "List of overlays for custom syntax highlighting.")

(defun org-clear-custom-overlays ()
  "Clear all custom syntax highlighting overlays."
  (interactive)
  (while org-custom-overlays
    (delete-overlay (pop org-custom-overlays))))

(defun org-highlight-custom-syntax ()
  "Highlight custom syntax patterns like {{extract:...}} with proper handling of nested structures."
  (interactive)
  (when (eq major-mode 'org-mode)
    (org-clear-custom-overlays)
    (save-excursion
      (goto-char (point-min))
      (while (re-search-forward "{{\\(extract\\|clozed\\|[^}:]*\\):" nil t)
        (let ((start (match-beginning 0))
              (type (match-string-no-properties 1))
              (content-start (point))
              (brace-level 1)
              (found-end nil)
              ;; Stack to track nested structures (LaTeX, citations, etc.)
              (context-stack '())
              ;; Point where we need to track from
              (search-start-pos (point)))
          
          ;; Process character by character to properly handle all nested structures
          (while (and (not found-end) (not (eobp)))
            (let ((char-at-point (char-after)))
              (cond
               ;; Track superscript citation patterns
               ((and (= char-at-point ?^) (looking-at "\\^{"))
                (push 'citation context-stack)
                (forward-char 2)  ; Skip ^{
                (setq search-start-pos (point)))
               
               ;; Handle square brackets in citations
               ((and (eq (car-safe context-stack) 'citation)
                     (= char-at-point ?\[))
                (push 'bracket context-stack)
                (forward-char 1))
               
               ;; Handle closing square brackets in citations
               ((and (eq (car-safe context-stack) 'bracket)
                     (= char-at-point ?\]))
                (pop context-stack)  ; Remove the bracket context
                (forward-char 1))
               
               ;; Handle closing brace for citations
               ((and (eq (car-safe context-stack) 'citation)
                     (= char-at-point ?}))
                (pop context-stack)  ; Remove the citation context
                (forward-char 1))
               
               ;; Handle LaTeX inline math \( ... \)
               ((looking-at "\\\\(")
                (push 'latex-inline context-stack)
                (forward-char 2))
               
               ((and (eq (car-safe context-stack) 'latex-inline)
                     (looking-at "\\\\)"))
                (pop context-stack)  ; End of inline math
                (forward-char 2))
               
               ;; Handle LaTeX environments
               ((looking-at "\\\\begin{\\([^}]+\\)}")
                (push (cons 'latex-env (match-string-no-properties 1)) context-stack)
                (goto-char (match-end 0)))
               
               ((and (looking-at "\\\\end{\\([^}]+\\)}")
                     (eq (car-safe (car-safe context-stack)) 'latex-env))
                (let ((env (match-string-no-properties 1)))
                  (when (string= env (cdr (car context-stack)))
                    (pop context-stack)))
                (goto-char (match-end 0)))
               
               ;; Handle dollar sign math delimiters
               ((= char-at-point ?$)
                (if (eq (car-safe context-stack) 'latex-dollar)
                    (pop context-stack)
                  (push 'latex-dollar context-stack))
                (forward-char 1))
               
               ;; Handle double dollar sign math delimiters
               ((looking-at "\\$\\$")
                (if (eq (car-safe context-stack) 'latex-double-dollar)
                    (pop context-stack)
                  (push 'latex-double-dollar context-stack))
                (forward-char 2))
               
               ;; Handle opening braces - only count when not in a special context
               ((and (= char-at-point ?{) (looking-at "{{")
                     (not context-stack))
                (setq brace-level (1+ brace-level))
                (forward-char 2))
               
               ;; Handle closing braces - only count when not in a special context
               ((and (= char-at-point ?}) (looking-at "}}")
                     (not context-stack))
                (setq brace-level (1- brace-level))
                (when (= brace-level 0)
                  (setq found-end t))
                (forward-char 2))
               
               ;; Handle single braces that are part of the content
               ((or (= char-at-point ?{) (= char-at-point ?}))
                (forward-char 1))
               
               ;; Default case: move forward one character
               (t (forward-char 1)))))
          
          ;; If we found a proper ending, create the overlays
          (when found-end
            ;; Back up to get content-end position
            (backward-char 2)
            (let* ((content-end (point))
                   (full-end (+ content-end 2))
                   (face (cond
                          ((string= type "clozed") 'org-clozed-face)
                          ((string= type "extract") 'org-extract-face)
                          (t 'org-default-custom-face))))
              
              ;; Main overlay for the content
              (let ((ov (make-overlay start full-end)))
                (overlay-put ov 'face face)
                (overlay-put ov 'priority 100)
                (push ov org-custom-overlays))
              
              ;; Hide opening marker
              (let ((ov-start (make-overlay start content-start)))
                (overlay-put ov-start 'display "")
                (overlay-put ov-start 'invisible t)
                (overlay-put ov-start 'priority 101)
                (push ov-start org-custom-overlays))
              
              ;; Hide closing marker
              (let ((ov-end (make-overlay content-end full-end)))
                (overlay-put ov-end 'display "")
                (overlay-put ov-end 'invisible t)
                (overlay-put ov-end 'priority 101)
                (push ov-end org-custom-overlays))
              
              ;; Move past this block
              (goto-char full-end))
            
            ;; If no end was found, just move forward
            (unless found-end
              (goto-char search-start-pos)
              (forward-char 1)))))))
  
  (setq org-custom-overlays (delete-dups org-custom-overlays)))

;; Variable to store buffer-local timers
(defvar-local org-custom-syntax-timer nil
  "Timer for updating syntax highlighting.")

;; Function to update after changes
(defun org-update-custom-syntax-after-change (_beg _end _len)
  "Update custom syntax highlighting after buffer changes."
  (when (eq major-mode 'org-mode)
    (when org-custom-syntax-timer
      (cancel-timer org-custom-syntax-timer))
    (setq org-custom-syntax-timer
          (run-with-idle-timer 0.2 nil
                               (lambda ()
                                 (when (buffer-live-p (current-buffer))
                                   (with-current-buffer (current-buffer)
                                     (org-highlight-custom-syntax))))))))

;; For debugging
(defun org-print-context-debug-info ()
  "Print debugging information about the current context."
  (interactive)
  (let ((char-at-point (char-after))
        (next-chars (buffer-substring-no-properties
                     (point)
                     (min (+ (point) 10) (point-max)))))
    (message "Character: %c, Next: %s, Context: looking at citation? %s"
             char-at-point next-chars
             (looking-at "\\^{\\[\\["))))

;; Set up for org-mode
(add-hook 'org-mode-hook 'org-highlight-custom-syntax)
(add-hook 'org-mode-hook
          (lambda ()
            (add-hook 'after-change-functions 
                      'org-update-custom-syntax-after-change nil t)))

;; Toggle visibility of markers
(defvar-local org-syntax-markers-visible nil
  "Whether syntax markers are visible.")

(defun org-toggle-syntax-markers ()
  "Toggle visibility of custom syntax markers."
  (interactive)
  (setq-local org-syntax-markers-visible 
              (not org-syntax-markers-visible))
  (dolist (ov org-custom-overlays)
    (when (overlay-get ov 'invisible)
      (overlay-put ov 'invisible 
                   (not org-syntax-markers-visible))))
  (message "Syntax markers now %s" 
           (if org-syntax-markers-visible "visible" "hidden")))

;; Refresh all open org buffers
(defun org-refresh-all-custom-highlighting ()
  "Apply custom highlighting to all org-mode buffers."
  (interactive)
  (dolist (buf (buffer-list))
    (with-current-buffer buf
      (when (eq major-mode 'org-mode)
        (org-highlight-custom-syntax)))))

;; Command to manually refresh the current buffer
(defun org-refresh-custom-highlighting ()
  "Refresh custom syntax highlighting in current buffer."
  (interactive)
  (when (eq major-mode 'org-mode)
    (org-highlight-custom-syntax)
    (message "Custom syntax highlighting refreshed")))

;; Run initially when loading
(when (featurep 'org)
  (org-refresh-all-custom-highlighting))

(defun org-remove-all-extract-blocks ()
  "Remove all {{extract:...}} blocks from the current buffer."
  (interactive)
  (when (eq major-mode 'org-mode)
    ;; Ensure highlighting is up-to-date
    (org-highlight-custom-syntax)
    
    ;; Process removal
    (let ((count 0))
      ;; Find all blue highlights (extract blocks only)
      (dolist (ov (overlays-in (point-min) (point-max)))
        ;; Only target the blue 'extract' highlights
        (when (eq (overlay-get ov 'face) 'org-extract-face)
          ;; Track how many we've found
          (setq count (1+ count))
          ;; Remove this extract block
          (delete-region (overlay-start ov) (overlay-end ov))))

      (message "Removed %d extract blocks" count))))

(defun org-srs-entry-p (pos)
  "Determine if and where the Org entry at POS or its immediate parent contains
the specified log drawer (org-srs-log-drawer-name).

Returns:
- 'current : If the drawer is found directly under the current entry.
- 'parent  : If the drawer is found directly under the immediate parent entry 
            (and not under the current entry).
- nil      : If the drawer is not found in either location."
  (interactive (list (point)))

  ;; Ensure the required variable is defined and not empty
  (unless (boundp 'org-srs-log-drawer-name)
    (error "Variable 'org-srs-log-drawer-name' is not defined. Set it to your drawer name"))
  (when (string-empty-p org-srs-log-drawer-name)
    (error "Variable 'org-srs-log-drawer-name' is empty. Set it to your drawer name"))

  ;; Save current buffer state
  (save-excursion
    ;; Ensure we're at a heading
    (goto-char pos)
    (unless (or (org-at-heading-p) (org-back-to-heading t))
      (message "org-srs-entry-p: Not within an Org entry")
      (cl-return-from org-srs-entry-p nil))

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
      
      ;; Output debug message and return result
      (message "org-srs-entry-p: Result (for drawer '%s') = %s" 
               org-srs-log-drawer-name location)
      location)))

(defun my-set-priority-with-heuristics (&optional specific-range retried)
  "Set a random priority within a user-defined heuristic range with retry mechanism.
Optional RETRIED is used internally to prevent infinite recursion."
  (interactive)
  (let* ((priority-ranges my-priority-ranges)
	   (max-retries 3)
	   (retry-delay 0.01)
	   (range
	    (cond
	     (specific-range
	      (cdr (assoc specific-range priority-ranges)))
	     ((called-interactively-p 'any)
	      (let* ((default-range (or (my-get-current-priority-range) (+ 6 (random 4))))
		     (user-choice (read-number
				   "Select a priority range (0-9): "
				   default-range)))
		(cdr (assoc user-choice priority-ranges))))
	     (t
	      (cdr (assoc (or (my-get-current-priority-range) (+ 6 (random 4))) priority-ranges)))))
	   (success nil)
	   (attempt 0)
	   random-priority)
    (if range
	  (let* ((min-priority (car range))
		 (max-priority (cdr range))
		 (desired-priority (+ min-priority
				      (random (1+ (- max-priority min-priority)))))
		 final-priority)
	    (setq random-priority desired-priority)
	    (while (and (not success) (< attempt max-retries))
	      (condition-case err
		  (progn
		    ;; Ensure heading state consistency
		    (when (org-at-heading-p) 
		      (org-back-to-heading t)
		      (org-show-entry)
		      (redisplay))
		    ;; Original priority adjustment logic
		    (let* ((current-priority (string-to-number
					     (or (org-entry-get nil "PRIORITY")
						 (number-to-string org-priority-default))))
			   (delta (- desired-priority current-priority)))
		      (cond
		       ((< delta 0)
			(dotimes (_ (abs delta))
			  (org-priority-up)))
		       ((> delta 0)
			(dotimes (_ delta)
			  (org-priority-down)))
		       (t
			(if (= current-priority org-priority-highest)
			    (progn
			      (org-priority-down)
			      (org-priority-up))
			  (if (= current-priority org-priority-lowest)
			      (progn
				(org-priority-up)
				(org-priority-down))
			    (progn
			      (org-priority-up)
			      (org-priority-down))))))
		      ;; Priority validation
		      (setq final-priority (string-to-number
					   (or (org-entry-get nil "PRIORITY")
					       (number-to-string org-priority-default))))
		      (unless (and final-priority 
				   (integerp final-priority)
				   (= final-priority desired-priority))
			(error "Priority validation failed")))
		    (setq success t))
		;; Error handling with automatic retry
		(error 
		 (setq attempt (1+ attempt))
		 (when (and (< attempt max-retries) 
			    (not (org-entry-get nil "PRIORITY")))
		   (org-entry-put nil "PRIORITY" 
				 (number-to-string org-priority-default)))
		 (if (< attempt max-retries)
		     (progn (message "Retrying (%d/%d)..." attempt max-retries)
			    (sleep-for retry-delay))
		   ;; Trigger auto-retry if not already retried
		   (message "Failed after %d attempts: %s" 
			    max-retries (error-message-string err))
		   (unless retried
		     (message "Auto-retrying...")
		     (my-set-priority-with-heuristics specific-range t))))))
	    (when success 
	      (message "Priority set to: %d" random-priority)))
	(message "Invalid range."))))

(defun my-increase-priority-range ()
  "Increase the priority range by moving to a lower number (0 is the highest priority).
Adjusts the priority within the new range, even if already at the highest."
  (interactive)
  (let ((current-range (or (my-get-current-priority-range) 9)))
    (let ((new-range (max 0 (1- current-range))))
	(my-set-priority-with-heuristics new-range)
	(message "Priority range increased to %d" new-range))))

(defun my-decrease-priority-range ()
  "Decrease the priority range by moving to a higher number (9 is the lowest priority).
Adjusts the priority within the new range, even if already at the lowest."
  (interactive)
  (let ((current-range (or (my-get-current-priority-range) 9)))
    (let ((new-range (min 9 (1+ current-range))))
	(my-set-priority-with-heuristics new-range)
	(message "Priority range decreased to %d" new-range))))

(defun my-ensure-priority-set (&optional max-attempts)
  "Ensure the current heading has a priority set.
If PRIORITY is not set, assign one within the appropriate range.
If PRIORITY is set, reassign a priority within the same range.
Skip priority setting if this entry's SRS drawer is in its parent.
MAX-ATTEMPTS: Maximum number of retry attempts (defaults to 15)."
  (let ((max-attempts (or max-attempts 15))
        (attempt 0)
        (success nil))
    
    ;; First check if this is a parent-level SRS entry
    (save-excursion
      (org-back-to-heading t)
      (when (eq (org-srs-entry-p (point)) 'parent)
        (message "Skipping priority set - SRS drawer is in parent entry")
        (setq success t)))  ;; Mark as successful to skip the loop
    
    (while (and (not success) (< attempt max-attempts))
      (setq attempt (1+ attempt))
      
      (condition-case err
          (save-excursion
            ;; Move to the current heading
            (org-back-to-heading t)
            ;; Ensure the heading is fully visible
            (org-show-entry)
            ;; Retrieve the current PRIORITY property
            (let ((current-priority (org-entry-get nil "PRIORITY")))
              (if (and current-priority (not (string= current-priority " ")))
                  ;; PRIORITY is set; determine its range and reassign within the same range
                  (let* ((priority-value (string-to-number current-priority))
                         (current-range (my-find-priority-range priority-value)))
                    (if current-range
                        (progn
                          (my-set-priority-with-heuristics current-range)
                          (message "Priority reassigned within range %d." current-range)
                          (setq success t))
                      (message "Current priority %d does not fall within any defined range."
                               priority-value)))
                ;; PRIORITY is not set; assign a random priority within appropriate ranges
                (let* ((matching-ranges
                        (cl-remove-if-not
                         (lambda (range)
                           (let ((min (car (cdr range)))
                                 (max (cdr (cdr range))))
                             (and (<= min org-priority-lowest)
                                  (>= max org-priority-default))))
                         my-priority-ranges))
                       (range-ids (mapcar #'car matching-ranges)))
                  (if range-ids
                      (let ((selected-range (nth (random (length range-ids)) range-ids)))
                        (my-set-priority-with-heuristics selected-range)
                        (message "Priority was not set. Assigned random priority within range %d."
                                 selected-range)
                        (setq success t))
                    (error "No valid range found for default priority settings. Check configurations."))))))
        (error
         (message "Attempt %d/%d failed in my-ensure-priority-set: %s" 
                  attempt max-attempts (error-message-string err))
         (when (>= attempt max-attempts)
           (signal (car err) (cdr err))))))
    
    (unless success
      (error "Failed to set priority after %d attempts" max-attempts))))

(defcustom my-random-schedule-default-months 3
  "Default number of months to schedule if none is specified."
  :type 'integer
  :group 'org-queue)

(defun my-round-to-decimals (number decimals)
  "Round NUMBER to DECIMALS decimal places."
  (/ (float (round (* number (expt 10 decimals))))
     (expt 10 decimals)))

(defun my-find-schedule-weight ()
  "Calculate schedule weight based on SCHEDULED date in org header.
			    Returns:
			    - 0 for past dates and today
			    - Number of months ahead (days/30.0) for future dates
			    - `my-random-schedule-default-months` if no SCHEDULED date exists."
  (let* ((scheduled-time (org-get-scheduled-time (point)))  ; Get the SCHEDULED time
	   (current-time (current-time))                      ; Get the current time
	   (days-difference
	    (when scheduled-time
	      ;; Calculate the difference in days, then convert to months
	      (/ (float (- (time-to-days scheduled-time)
			   (time-to-days current-time)))
		 30.0))))
    (cond
     ((null scheduled-time)
	;; If no SCHEDULED time, return the default months
	(or (bound-and-true-p my-random-schedule-default-months) 0))
     ((<= days-difference 0)
	;; If the difference is 0 or negative, return 0
	0)
     (t
	;; Otherwise, return the rounded difference in months to 2 decimal places
	(my-round-to-decimals days-difference 2)))))

(defcustom my-random-schedule-exponent 1
  "Exponent n controlling the bias of the scheduling distribution.
									    - n = 0: Uniform distribution (no bias).
									    - n = 1: Quadratic distribution (default).
									    - n = 2: Cubic distribution (stronger bias towards later dates)."
  :type 'integer
  :group 'org-queue)

(defun my-random-schedule (months &optional n)
  "Schedules an Org heading MONTHS months in the future using a mathematically elegant distribution.
If N is provided, use that as the exponent. If it's not provided, fallback to `my-random-schedule-exponent'.
Skips scheduling if the current heading or its parent has an SRS drawer."
  (when (and (not noninteractive)
             (eq major-mode 'org-mode))
    ;; Check if heading or parent has SRS drawer
    (let ((srs-result (org-srs-entry-p (point))))
      (if (or (eq srs-result 'current) (eq srs-result 'parent))
          ;; Skip scheduling if entry or parent has SRS drawer
          (message "Skipping scheduling for entry with SRS drawer")
        ;; Otherwise, proceed with normal scheduling
        (let* ((today (current-time))
               (total-days (* months 30))
               ;; If `n` is not passed in, use our existing defcustom value
               (n (or n my-random-schedule-exponent))
               (u (/ (float (random 1000000)) 1000000.0))
               (exponent (/ 1.0 (+ n 1)))  ; compute 1/(n+1)
               (x (expt u exponent))
               (days-ahead (floor (* total-days x)))
               (random-date (time-add today (days-to-time days-ahead))))
          (org-schedule nil (format-time-string "%Y-%m-%d" random-date)))))))

(defun my-random-schedule-command (&optional months)
  "Interactive command to schedule MONTHS months in the future.
				If MONTHS is not provided, uses the result of my-find-schedule-weight."
  (interactive
   (list (read-number
	    "Enter the upper month limit: "
	    (my-find-schedule-weight))))
  (save-excursion
    ;; Schedule the current heading
    (my-random-schedule (or months (my-find-schedule-weight)))))

(defun my-advance-schedule ()
  "Advance the current Org heading by a mathematically adjusted number of months.
Uses a function that decreases with increasing current schedule weight,
ensuring that tasks scheduled further in the future are advanced less.
Does not schedule tasks to dates before today.
Skips scheduling if the current heading or its parent is an SRS entry
(contains the org-srs-log-drawer-name drawer)."
  (interactive)
  (when (and (not noninteractive)
             (eq major-mode 'org-mode))
    ;; Check if this is an SRS entry
    (unless (org-srs-entry-p (point)) ; Only continue if NOT an SRS entry
      (let* ((e (exp 1))  ; e ≈ 2.71828
             ;; Get the current scheduled months ahead
             (current-weight (max 0 (my-find-schedule-weight)))  ; Ensure non-negative value
             ;; Calculate the adjustment using f(x) = x - 1 / ln(x + e)
             (adjusted-months (max 0 (- current-weight
                                        (/ 1 (log (+ current-weight e))))))
             ;; Generate a random value between current-weight and adjusted-months
             (min-months (min current-weight adjusted-months))
             (max-months (max current-weight adjusted-months))
             (random-months (random-float min-months max-months))
             ;; Convert random-months to days
             (adjusted-days (* random-months 30)))
        ;; Schedule the task to the adjusted date, ensuring it is not before today
        (org-schedule nil (format-time-string "%Y-%m-%d"
                                             (time-add (current-time)
                                                       (days-to-time adjusted-days))))))))

(defun my-postpone-schedule ()
  "Postpone the current Org heading by a mathematically adjusted number of months.
Calculates the postponement using a function that increases while its derivative decreases,
to ensure that tasks with larger weights are postponed by relatively smaller amounts.
Skip postponing if the current entry or its parent contains an SRS drawer."
  (interactive)
  (when (and (not noninteractive)
             (eq major-mode 'org-mode))
    ;; Check if this is an SRS entry (has SRS drawer in current or parent heading)
    (let ((srs-status (org-srs-entry-p (point))))
      ;; Only proceed if not an SRS entry (i.e., org-srs-entry-p returns nil)
      (unless srs-status
        (let* ((e (exp 1))  ; e ≈ 2.71828
               (current-weight (max 0 (my-find-schedule-weight)))  ; Ensure non-negative value
               ;; Adjusted months using f(x) = x + 1 / ln(x + e)
               (adjusted-months (+ current-weight
                                   (/ 1 (log (+ current-weight e)))))
               ;; Generate a random value between current-weight and adjusted-months
               (min-months (min current-weight adjusted-months))
               (max-months (max current-weight adjusted-months))
               (random-months (random-float min-months max-months))
               ;; Convert random-months to days
               (adjusted-days (* random-months 30))
               (now (current-time))
               (proposed-new-time (time-add now (days-to-time adjusted-days)))
               ;; Calculate tomorrow's midnight
               (now-decoded (decode-time now))
               (year (nth 5 now-decoded))
               (month (nth 4 now-decoded))
               (day (nth 3 now-decoded))
               (tomorrow-midnight (encode-time 0 0 0 (1+ day) month year))
               ;; Ensure new-time is at least tomorrow-midnight
               (new-time (if (time-less-p proposed-new-time tomorrow-midnight)
                             tomorrow-midnight
                           proposed-new-time)))
          ;; Schedule the task to the adjusted date
          (org-schedule nil (format-time-string "%Y-%m-%d" new-time)))))))

(defun my-custom-shuffle (list)
  "Fisher-Yates shuffle implementation for Emacs Lisp."
  (let ((vec (vconcat list)) (i (length list)))
    (while (> i 1)
	(let* ((j (random i))
	       (temp (aref vec (setq i (1- i)))))
	  (aset vec i (aref vec j))
	  (aset vec j temp)))
    (append vec nil)))

(defun my-auto-advance-schedules (&optional power)
  "Advance 2^POWER random tasks (default:64) with proper loop control."
  (interactive "P")
  (let* ((n (or power 6))
	   (limit (expt 2 n))
	   (candidates (org-map-entries #'point-marker nil 'agenda))
	   (shuffled (my-custom-shuffle candidates))
	   (total (length shuffled))
	   (processed 0)
	   (count 0))
    (save-some-buffers t)
    (catch 'break
	(dolist (m shuffled)
	  (when (>= count limit) (throw 'break nil))
	  (setq count (1+ count))
	  (org-with-point-at m
	    (when (my-advance-schedule)
	      (setq processed (1+ processed))))))
    (save-some-buffers t)
    (message "Advanced %d/%d (2^%d=%d)" processed total n limit)))

(defun my-auto-postpone-schedules (&optional power)
  "Postpone 2^POWER random tasks (default:64) with safe iteration."
  (interactive "P")
  (let* ((n (or power 6))
	   (limit (expt 2 n))
	   (candidates (org-map-entries #'point-marker nil 'agenda))
	   (shuffled (my-custom-shuffle candidates))
	   (total (length shuffled))
	   (processed 0)
	   (count 0))
    (save-excursion
	(save-some-buffers t)
	(catch 'break
	  (dolist (m shuffled)
	    (when (>= count limit) (throw 'break nil))
	    (setq count (1+ count))
	    (org-with-point-at m
	      (when (my-postpone-schedule)
		(setq processed (1+ processed))))))
	(save-some-buffers t)
	(message "Postponed %d/%d (2^%d=%d)" processed total n limit))))

(defun my-schedule-command (&optional months)
  "Interactive command that schedules MONTHS months in the future and prompts for priority."
  (interactive
   (list (read-number
	    "Enter the upper month limit: "
	    (my-find-schedule-weight))))
  ;; Schedule the current heading
  (my-random-schedule (or months (my-find-schedule-weight)))
  (my-ensure-priority-set))

(defun my-ensure-priorities-and-schedules-for-all-headings (&optional max-attempts)
  "Ensure priorities and schedules are set for all headings across Org agenda files.
Repeatedly processes headings until all have priorities and schedules, or max-attempts is reached.
MAX-ATTEMPTS: Maximum number of retry attempts (defaults to 15)."
  (interactive)
  (let ((max-attempts (or max-attempts 15))
        (attempt 0)
        (all-complete nil)
        (processed-headings (make-hash-table :test 'equal)))

    (while (and (not all-complete) (< attempt max-attempts))
      (setq attempt (1+ attempt))
      (save-some-buffers t)

      ;; First pass: Count total entries and incomplete entries
      (let ((total-entries 0)
            (incomplete-entries 0))
        (org-map-entries
         (lambda ()
           (setq total-entries (1+ total-entries))
           ;; First check if entry has missing priority or schedule
           (when (or (not (org-entry-get nil "PRIORITY"))
                     (string= (org-entry-get nil "PRIORITY") " ")
                     (not (org-entry-get nil "SCHEDULED")))
             ;; Only now check if it's an SRS entry (more expensive operation)
             (let* ((marker (point-marker))
                    (file (buffer-file-name))
                    (position (point))
                    (heading-id (concat file ":" (number-to-string position)))
                    (srs-status (or (gethash heading-id processed-headings)
                                    (puthash heading-id (org-srs-entry-p (point)) processed-headings))))
               
               ;; Count as incomplete based on SRS status:
               ;; - 'parent: skip both priority and schedule (never incomplete)
               ;; - 'current: only incomplete if missing priority (we skip scheduling)
               ;; - nil: incomplete if missing either priority or schedule
               (cond
                ((eq srs-status 'parent)
                 nil) ; Skip entirely
                ((eq srs-status 'current)
                 ;; Only check priority for 'current entries
                 (when (or (not (org-entry-get nil "PRIORITY"))
                           (string= (org-entry-get nil "PRIORITY") " "))
                   (setq incomplete-entries (1+ incomplete-entries))))
                (t
                 ;; For non-SRS entries, count if missing either
                 (setq incomplete-entries (1+ incomplete-entries)))))))
         nil 'agenda)

        ;; Process entries if there are incomplete ones
        (when (> incomplete-entries 0)
          (org-map-entries
           (lambda ()
             ;; First check if entry has missing priority or schedule
             (when (or (not (org-entry-get nil "PRIORITY"))
                       (string= (org-entry-get nil "PRIORITY") " ")
                       (not (org-entry-get nil "SCHEDULED")))
               ;; Only check SRS status if needed
               (let* ((file (buffer-file-name))
                      (position (point))
                      (heading-id (concat file ":" (number-to-string position)))
                      (srs-status (or (gethash heading-id processed-headings)
                                      (puthash heading-id (org-srs-entry-p (point)) processed-headings))))
                 
                 (cond
                  ;; For 'parent entries, skip entirely
                  ((eq srs-status 'parent)
                   nil)
                  
                  ;; For 'current entries, only set priority if needed
                  ((eq srs-status 'current)
                   (condition-case err
                       (let ((current-priority (org-entry-get nil "PRIORITY")))
                         (when (or (not current-priority) 
                                   (string= current-priority " "))
                           (my-ensure-priority-set)))
                     (error
                      (message "Error processing priority for 'current entry: %s" 
                               (error-message-string err)))))
                  
                  ;; For non-SRS entries, process both priority and schedule
                  (t
                   (condition-case err
                       (progn
                         ;; Ensure priority is set only if missing
                         (let ((current-priority (org-entry-get nil "PRIORITY")))
                           (when (or (not current-priority) 
                                     (string= current-priority " "))
                             (my-ensure-priority-set)))
                         ;; Ensure schedule is set only if missing
                         (unless (org-entry-get nil "SCHEDULED")
                           (my-random-schedule (my-find-schedule-weight) 0)))
                     (error
                      (message "Error processing entry: %s" 
                               (error-message-string err)))))))))
           nil 'agenda))

        ;; Set all-complete if no incomplete entries found
        (setq all-complete (zerop incomplete-entries)))

      (save-some-buffers t)

      (message "Attempt %d/%d completed. %s"
               attempt 
               max-attempts
               (if all-complete
                   "All entries processed successfully!"
                 "Some entries are still incomplete.")))

    (when (and (not all-complete) (>= attempt max-attempts))
      (message "Warning: Reached maximum attempts (%d). Some entries may still be incomplete." 
               max-attempts))))

(defun my-post-org-insert-heading (&rest _args)
    "Run after `org-insert-heading` to assign priority and schedule."
  
    (when (and (not noninteractive)
		 (eq major-mode 'org-mode)
		 (bound-and-true-p org-queue-mode))  ;; 🙌 Only trigger in org-queue-mode
	(my-random-schedule (my-find-schedule-weight) 0)
	(call-interactively #'my-set-priority-with-heuristics)
	(end-of-line)))

(defun my-is-overdue-task ()
  "Return non-nil if the current task is overdue."
  (let ((scheduled-time (org-get-scheduled-time nil)))
    (and scheduled-time
	   (< (time-to-days scheduled-time) (time-to-days (current-time))))))

(defun my-is-outstanding-task ()
  "Return non-nil if the current task is overdue or due today."
  (let ((scheduled-time (org-get-scheduled-time nil)))
    (and scheduled-time
	   (<= (time-to-days scheduled-time) (time-to-days (current-time))))))

(defun my-org-agenda-skip-non-outstanding-tasks ()
  "Skip tasks that are not outstanding."
  (unless (my-is-outstanding-task)
    (org-end-of-subtree t)))

(defun my-org-agenda-skip-past-and-today-tasks ()
  "Skip tasks that are scheduled for today or earlier; show only future tasks."
  (let ((scheduled-time (org-get-scheduled-time nil)))
    (if (or (not scheduled-time)
	      (<= (time-to-days scheduled-time) (time-to-days (current-time))))
	  (org-end-of-subtree t))))

(defun my-org-agenda-skip-scheduled-tasks ()
  "Skip tasks that have a SCHEDULED date."
  (let ((scheduled-time (org-get-scheduled-time nil)))
    (if scheduled-time
	  (org-end-of-subtree t))))

(setq org-agenda-sorting-strategy
	'((agenda priority-down category-down)
	  (todo priority-down)
	  (tags priority-down)
	  (search category-keep)))

(defun my-get-raw-priority-value ()
  "Get the priority value of the current point without using task list."
  (let ((priority-str (org-entry-get nil "PRIORITY")))
    (if priority-str
	  (string-to-number priority-str)
	(+ org-priority-default
	   (random (+ 1 (- org-priority-lowest org-priority-default)))))))

(defun my-extract-marker (task-or-marker)
  "Extract marker from TASK-OR-MARKER which can be a marker or a task plist.
Returns nil if no valid marker could be extracted."
  (cond
   ((markerp task-or-marker) task-or-marker)
   ((and (listp task-or-marker) (plist-get task-or-marker :marker))
    (plist-get task-or-marker :marker))
   (t nil)))

(defun my-safe-marker-buffer (task-or-marker)
  "Safely get the buffer of TASK-OR-MARKER.
TASK-OR-MARKER can be a marker or a plist with a :marker property."
  (let ((marker (my-extract-marker task-or-marker)))
    (when (and marker 
               (markerp marker)
               (marker-buffer marker)
               (buffer-live-p (marker-buffer marker)))
      (marker-buffer marker))))

(defun my-safe-marker-position (task-or-marker)
  "Safely get the position of TASK-OR-MARKER.
TASK-OR-MARKER can be a marker or a plist with a :marker property."
  (let ((marker (my-extract-marker task-or-marker)))
    (when (and marker 
               (markerp marker)
               (marker-buffer marker)
               (buffer-live-p (marker-buffer marker)))
      (marker-position marker))))

(defun my-get-priority-value ()
  "Get the priority value of the current task from the task list."
  (let* ((task (nth my-outstanding-tasks-index my-outstanding-tasks-list))
         (marker (my-extract-marker task))
         (priority-str (when (and marker (marker-buffer marker))
                         (org-with-point-at marker
                           (org-entry-get nil "PRIORITY")))))
    (if priority-str
        (string-to-number priority-str)
      (+ org-priority-default
         (random (+ 1 (- org-priority-lowest org-priority-default)))))))

;; Define a customizable variable for the base directory.
(defcustom org-queue-directory nil
  "Base directory for task files for Org Queue.
                If nil, a safe default directory will be used and created automatically."
  :type 'directory
  :group 'org-queue)

;; Define a customizable variable for the cache file.
(defcustom my-outstanding-tasks-cache-file
  (expand-file-name "org-queue-outstanding-tasks.cache" cache-dir)
  "File path to store the cached outstanding tasks list along with its date stamp.
                  By default, this file will be inside the cache directory (cache-dir)."
  :type 'string
  :group 'org-queue)

(defcustom my-outstanding-tasks-index-file
  (expand-file-name "org-queue-index.cache" cache-dir)
  "File path to store the current task index."
  :type 'string
  :group 'org-queue)

;; Variable to hold the list of outstanding tasks.
(defvar my-outstanding-tasks-list nil
  "List of outstanding tasks, sorted by priority.")

(defun my-save-outstanding-tasks-to-file ()
  "Save task list to cache, correctly handling plists or markers. Index stored separately."
  (with-temp-file my-outstanding-tasks-cache-file
    (let* ((today (format-time-string "%Y-%m-%d"))
           (tasks-saved
            (delq nil
                  (mapcar
                   (lambda (task-or-marker)
                     (let ((marker (cond
                                    ;; If it's already a marker
                                    ((markerp task-or-marker) task-or-marker)
                                    ;; If it's a plist with a marker
                                    ((plist-get task-or-marker :marker)
                                     (plist-get task-or-marker :marker))
                                    ;; Otherwise, can't save it
                                    (t nil))))
                       (when (and marker
                                  (marker-buffer marker)
                                  (buffer-file-name (marker-buffer marker)))
                         (with-current-buffer (marker-buffer marker)
                           (let* ((full (file-truename (buffer-file-name)))
                                  (path (if (and (boundp 'org-queue-directory)
                                                 org-queue-directory
                                                 (file-in-directory-p full org-queue-directory))
                                            (file-relative-name full org-queue-directory)
                                          full)))
                             (cons path (marker-position marker)))))))
                   my-outstanding-tasks-list))))
      ;; Only save date and tasks, no index
      (insert (prin1-to-string (list :date today :tasks tasks-saved)))))
  
  ;; Save index separately
  (my-save-index-to-file))

;; New function to save index separately
(defun my-save-index-to-file ()
  "Save the current task index to a device-specific file."
  (with-temp-file my-outstanding-tasks-index-file
    (insert (prin1-to-string (list :index my-outstanding-tasks-index
                                   :timestamp (current-time))))))

;; New function to load index separately
(defun my-load-index-from-file ()
  "Load the task index from device-specific file."
  (when (file-exists-p my-outstanding-tasks-index-file)
    (condition-case nil
        (let* ((data (with-temp-buffer
                       (insert-file-contents my-outstanding-tasks-index-file)
                       (read (buffer-string))))
               (saved-index (plist-get data :index)))
          (when (and saved-index (numberp saved-index) (>= saved-index 0))
            (setq my-outstanding-tasks-index saved-index)
            t))
      (error nil))))

(defun my-load-outstanding-tasks-from-file ()
  "Load cached tasks, creating proper plist structures. Also loads index."
  (if (file-exists-p my-outstanding-tasks-cache-file)
      (let* ((data (with-temp-buffer
                     (insert-file-contents my-outstanding-tasks-cache-file)
                     (read (buffer-string))))
             (saved-date (plist-get data :date))
             (saved-tasks (plist-get data :tasks))
             (today (format-time-string "%Y-%m-%d")))
        (if (string= saved-date today)
            (progn
              (setq my-outstanding-tasks-list
                    (mapcar
                     (lambda (task-pair)
                       (let* ((stored-path (car task-pair))
                              (position (cdr task-pair))
                              (abs-path (if (or (file-name-absolute-p stored-path)
    						(string-match-p "^[A-Za-z]:[/\\\\]" stored-path))
                                            stored-path
                                          (expand-file-name stored-path 
                                                            (or org-queue-directory default-directory)))))
                         ;; Open the file and create a marker
                         (with-current-buffer (find-file-noselect abs-path)
                           (save-excursion
                             (goto-char position)
                             ;; Create the new plist structure
                             (let* ((marker (point-marker))
                                    (priority (my-get-raw-priority-value))
                                    (flag (my-priority-flag priority))
                                    (file abs-path))
                               (list :marker marker
                                     :priority priority
                                     :flag flag
                                     :file file))))))
                     saved-tasks))
              ;; Load index separately, fallback to 0 if not found or invalid
              (unless (and (my-load-index-from-file)
                           (< my-outstanding-tasks-index (length my-outstanding-tasks-list)))
                (setq my-outstanding-tasks-index 0))
              t)
          nil))
    nil))

(defun my-ensure-synchronized-task-list ()
  "Ensure we have a current, synchronized task list and valid index."
  ;; Try to load from cache first
  (unless (my-load-outstanding-tasks-from-file)
    ;; If cache is stale or missing, regenerate
    (my-get-outstanding-tasks)
    (setq my-outstanding-tasks-index 0)
    (my-save-outstanding-tasks-to-file))
  
  ;; Double-check index validity after loading
  (when (or (>= my-outstanding-tasks-index (length my-outstanding-tasks-list))
            (< my-outstanding-tasks-index 0))
    (setq my-outstanding-tasks-index 0)
    (my-save-index-to-file)))

(defvar my-outstanding-tasks-index 0
  "Current index in the outstanding tasks list.")

(defun my-get-outstanding-tasks ()
  "Populate task list with metadata for stable tracking."
  (setq my-outstanding-tasks-list nil)
  (org-map-entries
   (lambda ()
     (when (and (my-is-outstanding-task)
                (not (org-srs-entry-p (point))))
       (let* ((marker (point-marker))
              (priority (my-get-raw-priority-value))
              (flag (my-priority-flag priority))
              (file (buffer-file-name))
              (task-data (list :marker marker
                              :priority priority
                              :flag flag
                              :file file)))
         (push (cons priority task-data) my-outstanding-tasks-list))))
   nil 'agenda)
  
  ;; Sort by priority and extract only the task data
  (setq my-outstanding-tasks-list
        (mapcar #'cdr
                (sort my-outstanding-tasks-list (lambda (a b) (< (car a) (car b))))))
  (setq my-outstanding-tasks-index 0))

(defun my-remove-current-task ()
  "Remove the task at current index from outstanding tasks list and update cache file."
  (interactive)
  
  ;; Validate the task list
  (unless (and my-outstanding-tasks-list 
               (> (length my-outstanding-tasks-list) 0)
               (< my-outstanding-tasks-index (length my-outstanding-tasks-list)))
    (message "No valid task at current index to remove")
    (cl-return-from my-remove-current-task nil))
  
  ;; Remove the task
  (setq my-outstanding-tasks-list
        (append (seq-take my-outstanding-tasks-list my-outstanding-tasks-index)
                (seq-drop my-outstanding-tasks-list (1+ my-outstanding-tasks-index))))
  
  ;; Set index using your exact logic
  (if (<= my-outstanding-tasks-index 0)
      (setq my-outstanding-tasks-index (1- (length my-outstanding-tasks-list)))
    (setq my-outstanding-tasks-index (1- my-outstanding-tasks-index)))
  
  ;; Update cache file (this will also save the updated index separately)
  (my-save-outstanding-tasks-to-file)
  
  ;; Show feedback
  (message "Task removed. %d tasks remaining." (length my-outstanding-tasks-list))
  
  ;; Regenerate list if it became empty
  (when (zerop (length my-outstanding-tasks-list))
    (my-get-outstanding-tasks)
    (setq my-outstanding-tasks-index 0)
    (my-save-outstanding-tasks-to-file)))

(defun my-auto-postpone-overdue-tasks ()
  "Auto-postpone all overdue tasks using linear interpolation for priorities.
							 If a task's priority is not set, use `org-priority-default` to `org-priority-lowest`
							 as the basis for linear interpolation. The calculated `months` is passed to
							 `my-random-schedule` for randomness. Save all modified files before and after processing."
  (interactive)
  ;; Save all modified buffers before processing
  (save-some-buffers t) ;; Save all modified buffers without prompting
  (let* ((highest-priority org-priority-highest)  ; Highest priority value (e.g., 1)
	   (lowest-priority org-priority-lowest)    ; Lowest priority value (e.g., 64)
	   (default-priority org-priority-default)  ; Default priority value (e.g., 32)
	   (max-months (my-find-schedule-weight))) ; Max months for scheduling
    ;; Iterate over all headings in the agenda files
    (org-map-entries
     (lambda ()
	 (when (my-is-overdue-task)
	   ;; Ensure priority is set
	   (my-ensure-priority-set)
	   ;; Get the priority value or fallback to the default range
	   (let* ((priority-string (org-entry-get nil "PRIORITY"))
		  (priority (if priority-string
				(string-to-number priority-string)
			      ;; Fallback: Use default to lowest priority range
			      (random (+ 1 (- lowest-priority default-priority)))))
		  ;; Linearly interpolate months based on priority
		  (months (* max-months
			     (/ (float (- priority highest-priority))
				(float (- lowest-priority highest-priority))))))
	     ;; Use `my-random-schedule` to schedule the task
	     (my-random-schedule months)
	     (message "Task postponed with priority %d (months: %.2f)." priority months))))
     nil 'agenda))
  ;; Save all modified buffers after processing
  (save-some-buffers t)) ;; Save all modified buffers without prompting

(defun my-postpone-duplicate-priority-tasks ()
  "Postpone duplicate outstanding tasks within the same file that share the same priority.
For each file, only one task per priority level remains as outstanding. 
All other tasks with the same priority in the same file are postponed.
Tasks across different files or with different priorities within the same file are unaffected."
  (interactive)
  ;; Ensure we are operating on agenda files
  (let ((agenda-files (org-agenda-files)))
    (unless agenda-files
	(user-error "No agenda files found. Please set `org-agenda-files` accordingly."))
    (let ((seen-tasks (make-hash-table :test 'equal))) ; Hash table to track seen (file, priority) pairs
	;; Iterate over each agenda file
	(dolist (file agenda-files)
	  (with-current-buffer (find-file-noselect file)
	    (save-excursion
	      (goto-char (point-min))
	      ;; Iterate over all headings in the current file
	      (org-map-entries
	       (lambda ()
		 (when (my-is-outstanding-task)
		   (let* ((priority-string (org-entry-get nil "PRIORITY"))
			  (priority (if priority-string
					(string-to-number priority-string)
				      ;; If priority not set, use default
				      org-priority-default))
			  (key (cons (file-truename file) priority)))
		     (if (gethash key seen-tasks)
			 ;; Duplicate found, postpone it
			 (progn
			   (my-postpone-schedule)
			   (message "Postponed duplicate task with priority %d in file %s." priority file))
		       ;; First occurrence, mark as seen
		       (puthash key t seen-tasks))))))
	       nil 'file))))
    (message "Duplicate outstanding tasks have been processed.")))

(defun my-enforce-priority-constraints ()
  "Enforce hierarchical task constraints where higher priorities dictate lower priority limits.
Processes priorities in descending order (priority 1 → 64) using FIFO task postponement."
  (interactive)
  (let* ((agenda-files (org-agenda-files))
	  (priority-counts (make-hash-table :test 'equal))
	  (tasks-by-priority (make-hash-table :test 'equal)))

    ;; ==== PHASE 1: Data Collection ====
    (dolist (file agenda-files)
    (with-current-buffer (find-file-noselect file)
	(save-excursion
	  (goto-char (point-min))
	  ;; Process tasks from top to bottom (earlier positions first)
	  (org-map-entries
	    (lambda ()
	      (when (my-is-outstanding-task)
		(let* ((priority-str (org-entry-get nil "PRIORITY"))
		      (priority (if priority-str 
				  (string-to-number priority-str)
				  org-priority-default))
		      (task-entry (cons (buffer-file-name) (point))))
		  ;; Store tasks in reverse order to facilitate O(1) pop-front
		  (puthash priority 
			  (nconc (gethash priority tasks-by-priority) (list task-entry))
			  tasks-by-priority)
		  (puthash priority 
			  (1+ (gethash priority priority-counts 0))
			  priority-counts))))
	    nil 'file))))

    ;; ==== PHASE 2: Priority Order Setup ====
    ;; 1. Get sorted priorities 1 (highest) to 64 (lowest)
    (let* ((sorted-priorities (sort (hash-table-keys priority-counts) #'<))
	    (current-max nil)
	    highest-processed)

	;; ==== PHASE 3: Constraint Processing ====
	;; Process highest (1 → 64) priorities first
	(dolist (prio sorted-priorities)
	  ;; No reverse needed since (sort '<) returns 1,2,...,64
	  (let ((count (gethash prio priority-counts 0))
		(tasks (gethash prio tasks-by-priority '())))
	    (cond
	      ((zerop count) ; Skip empty priorities
	      nil)

	      ;; Set initial constraint from highest priority with tasks
	      ((null current-max)
	      (setq current-max count
		    highest-processed prio)
	      (message "[CONSTRAINT] Priority %d = new global max: %d" 
		      prio current-max))

	      ;; Enforce constraints for lower priorities
	      ((> count current-max)
	      (let ((excess (- count current-max)))
		(message "[ENFORCE] Priority %d overflow (%d > max %d). Postponing %d tasks..."
			prio count current-max excess)
		;; Process oldest tasks first (FIFO through list order)
		(dotimes (_ excess)
		  (when-let ((task (pop tasks))) 
		    (let ((file (car task))
			  (pos (cdr task)))
		      ;; Postpone logic
		      (with-current-buffer (find-file-noselect file)
			(goto-char pos)
			(my-postpone-schedule)
			(message "Postponed priority %d task: %s" prio 
				(file-name-nondirectory file))))))
		(puthash prio tasks tasks-by-priority)))
          
	      ;; Update current-max for subsequent priorities
	      (t
	      (setq current-max (min current-max count))))))

	;; ==== PHASE 4: Cleanup ====
	(save-some-buffers t)
	(message "[COMPLETE] Constrained %d priorities. Final max: %d"
		(hash-table-count priority-counts)
		current-max))))

(defun my-postpone-consecutive-same-file-tasks ()
  "Postpone consecutive tasks from the same file, keeping only the first.
Saves buffers and regenerates the task list for consistency."
  (interactive)
  (my-reset-outstanding-tasks-index)
  (when my-outstanding-tasks-list
    (let ((prev-file nil)
          (modified-buffers nil)
          (original-count (length my-outstanding-tasks-list)))
      ;; Iterate over the task PLISTS, not raw markers
      (dolist (entry my-outstanding-tasks-list)
        ;; Extract the real marker from the plist
        (let* ((marker (plist-get entry :marker))
               (buffer (and (markerp marker)
                            (marker-buffer marker)))
               (file   (and buffer
                            (buffer-file-name buffer))))
          (when buffer
            (if (and file (equal file prev-file))
                (progn
                  ;; postpone it
                  (with-current-buffer buffer
                    (goto-char (marker-position marker))
                    (my-postpone-schedule))
                  (push buffer modified-buffers)
                  (message "Postponed duplicate in: %s"
                           (file-name-nondirectory file)))
              (setq prev-file file)))))
      ;; Save all changed buffers
      (dolist (buf (delete-dups modified-buffers))
        (when (buffer-live-p buf)
          (with-current-buffer buf
            (save-buffer))))
      ;; If we actually postponed anything, rebuild the list
      (when modified-buffers
        (my-get-outstanding-tasks)
        (message "%d consecutive task(s) postponed, list regenerated."
                 (- original-count (length my-outstanding-tasks-list))))))
  (my-reset-outstanding-tasks-index))

(defun my-launch-anki ()
  "Launch Anki application if it exists. Works on Windows and macOS. On error or Android, show message and call `my-show-current-flag-status`."
  (interactive)
  (cond
   ;; Android: Just show flag status and message
   (my-android-p
    (message "📱 Please open Anki app manually for reviews")
    (my-show-current-flag-status))
   
   ;; Desktop systems
   (t
    (let ((anki-path
           (cond
            ;; Windows
            ((eq system-type 'windows-nt)
             (expand-file-name "AppData/Local/Programs/Anki/anki.exe" (getenv "USERPROFILE")))
            ;; macOS (default install location)
            ((eq system-type 'darwin)
             "/Applications/Anki.app/Contents/MacOS/Anki")
            ;; Else nil (e.g. GNU/Linux)
            (t nil))))
      (if (and anki-path (file-exists-p anki-path))
          (condition-case err
              (start-process "Anki" nil anki-path)
            (error
             (message "Failed to launch Anki: %s" (error-message-string err))
             (my-show-current-flag-status)))
        (message "Anki executable not found%s"
                 (if anki-path (format " at: %s" anki-path) " on this OS."))
        (my-show-current-flag-status))))))

(defconst my-priority-flag-table
  '((1 . "Flag:1")          ; 1
    (2 . "Flag:2")          ; 2
    (3 . "Flag:3")          ; 3–4
    (4 . "Flag:4")          ; 5–8
    (5 . "Flag:5")          ; 9–16
    (6 . "Flag:6")          ; 17–32
    (7 . "Flag:7"))         ; 33–64
  "Mapping from flag group to description for Anki-style flagging.")

(defun my-priority-flag (prio)
  "Return the flag number for a given PRIORITY."
  (cond
   ((= prio 1) 1)
   ((= prio 2) 2)
   ((<= prio 4) 3)
   ((<= prio 8) 4)
   ((<= prio 16) 5)
   ((<= prio 32) 6)
   ((<= prio 64) 7)))

(defun my-current-flag-counts ()
  "Calculate flag metrics for the current outstanding task."
  ;; Validate task list and index
  (unless (and my-outstanding-tasks-list
               (>= my-outstanding-tasks-index 0)
               (< my-outstanding-tasks-index (length my-outstanding-tasks-list)))
    (message "No valid task at current position")
    (cl-return-from my-current-flag-counts nil))
  
  ;; Get current task information
  (let* ((task-or-marker (nth my-outstanding-tasks-index my-outstanding-tasks-list))
         (current-flag (my-get-marker-flag task-or-marker))
         (flag-name (alist-get current-flag my-priority-flag-table)))
    
    ;; Check validity
    (unless current-flag
      (message "Cannot determine flag for current task")
      (cl-return-from my-current-flag-counts nil))
    
    ;; Count same-flag tasks and current position
    (let ((same-flag-count 0)
          (position 0))
      
      ;; Scan all tasks
      (dotimes (i (length my-outstanding-tasks-list))
        (let* ((m (nth i my-outstanding-tasks-list))
               (flag (my-get-marker-flag m)))
          (when (and flag (= flag current-flag))
            (setq same-flag-count (1+ same-flag-count))
            (when (= i my-outstanding-tasks-index)
              (setq position same-flag-count)))))
      
      ;; Calculate remaining (including current)
      (let ((remaining (- same-flag-count (1- position))))
        (list :flag-name flag-name
              :flag-num current-flag
              :flag-count same-flag-count
              :flag-pos position
              :flag-left remaining)))))

(defun my-get-marker-flag (marker-or-task)
  "Helper to safely get flag from marker or task."
  (cond
   ;; Case 1: It's a marker
   ((markerp marker-or-task)
    (when (and (marker-buffer marker-or-task)
               (buffer-live-p (marker-buffer marker-or-task)))
      (with-current-buffer (marker-buffer marker-or-task)
        (save-excursion
          (goto-char (marker-position marker-or-task))
          (my-priority-flag
           (string-to-number (or (org-entry-get nil "PRIORITY") "0")))))))
   
   ;; Case 2: It has a :flag property (plist)
   ((plist-get marker-or-task :flag)
    (plist-get marker-or-task :flag))
   
   ;; Case 3: It has a :marker property (plist)
   ((plist-get marker-or-task :marker)
    (let ((marker (plist-get marker-or-task :marker)))
      (when (and (markerp marker)
                 (marker-buffer marker)
                 (buffer-live-p (marker-buffer marker)))
        (with-current-buffer (marker-buffer marker)
          (save-excursion
            (goto-char (marker-position marker))
            (my-priority-flag
             (string-to-number (or (org-entry-get nil "PRIORITY") "0"))))))))
   
   ;; Default: Can't determine flag
   (t nil)))

(defun my-show-current-flag-status ()
  "Show flag info and current outstanding task index (1-based) and total."
  (interactive)
  (let ((data (my-current-flag-counts))
        (tasks (if (boundp 'my-outstanding-tasks-list)
                   (length my-outstanding-tasks-list)
                 nil))
        (idx (if (and (boundp 'my-outstanding-tasks-index)
                      (boundp 'my-outstanding-tasks-list)
                      my-outstanding-tasks-list)
                 (1+ my-outstanding-tasks-index)
               nil)))
    (if data
        (let* ((flag-name (plist-get data :flag-name))
               (flag-num (plist-get data :flag-num))
               (fidx (plist-get data :flag-pos))
               (ftotal (plist-get data :flag-count))
               (fleft (plist-get data :flag-left)))
          (message "%s (%d of %d, %d left) | Task %s/%s"
                   (or flag-name (format "Flag %s" (or flag-num "?")))
                   fidx ftotal fleft
                   (or idx "?")
                   (or tasks 0)))
      (message "No current flag info. Task %s/%s"
               (or idx "?")
               (or tasks 0)))))

(defun my-pulse-highlight-current-line (&optional time)
  "Temporarily pulse-highlight the current line.

Optional argument TIME specifies the delay between pulse iterations in seconds.
Defaults to 0.2 seconds."
  (let ((pulse-iterations 3)
	  (pulse-delay (or time 0.2)))
    (pulse-momentary-highlight-one-line (point))))

(defun widen-and-recenter ()
  "Widen buffer, reset folding, show top-level children, and recenter point."
  (interactive)
  (let ((marker (point-marker))) ;; Store original position
    (widen)                      ;; Remove narrowing
    (ignore-errors (outline-up-heading 1))
    (org-overview)
    (ignore-errors (org-fold-show-children))
    (goto-char marker)           ;; Return to original position
    (ignore-errors (org-reveal t))
    (ignore-errors (org-show-children))
    ;; Only recenter when buffer is displayed in current window
    (when (eq (current-buffer) (window-buffer (selected-window)))
      (recenter))
    (ignore-errors (org-highlight-custom-syntax))))

(defun org-show-parent-heading-cleanly ()
  "Move up to the parent heading, widen the buffer, and then reveal the parent heading along with its children."
  (interactive)
  (widen-and-recenter)
  (ignore-errors (outline-up-heading 1)) ;; Move up heading (safely handle errors)
  (org-narrow-to-subtree)
  (org-highlight-custom-syntax))

;; Add this to your Emacs config - never ask about reverting
(setq revert-without-query '(".*"))

(defun my-display-task-at-marker (task-or-marker)
  "Display the task at TASK-OR-MARKER with appropriate visibility settings."
  (widen-and-recenter)
  (when (buffer-file-name)
    ;; Temporarily suppress revert prompts
    (let ((revert-without-query '(".*")))
      (revert-buffer t t t)))
  (let* ((marker (my-extract-marker task-or-marker))
         (buf (and marker (marker-buffer marker)))
         (pos (and marker (marker-position marker))))
    (when (and buf pos)
      (condition-case err
          (progn
            (switch-to-buffer buf)
            (when (buffer-file-name buf)
              (let ((revert-without-query '(".*")))
                (revert-buffer t t t)))
            (widen-and-recenter)
            (goto-char pos)
            (widen-and-recenter)
            
            (org-narrow-to-subtree)
            (org-overview)
            (org-reveal t)
            (org-show-entry)
            (show-children)
            
            (when (eq buf (window-buffer (selected-window)))
              (recenter)))
        (error
         (message "Error displaying task: %s" (error-message-string err)))))))

(defun my-show-next-outstanding-task ()
  "Show the next outstanding task in priority order with proper SRS handling."
  (interactive)

  (widen-and-recenter)

  ;; Ensure we have synchronized data
  (my-ensure-synchronized-task-list)

  ;; First check if SRS session just ended (detect message)
  (when (and (current-message)
             (string-match-p "No more cards to review" (current-message)))
    (message "Review session complete - continuing with tasks")
    (sit-for 1)  ;; Brief pause for user to see the message
    (setq my-srs-reviews-exhausted t))
  
  ;; If no list exists or we're at the end, get/refresh the list
  (when (or (not my-outstanding-tasks-list)
            (>= my-outstanding-tasks-index (length my-outstanding-tasks-list)))
    (if my-outstanding-tasks-list
        ;; If we have a list but reached the end, reset index to 0
        (setq my-outstanding-tasks-index 0)
      ;; If no list, get it
      (my-get-outstanding-tasks)))
  
  ;; Increment index for next task
  (when (and my-outstanding-tasks-list 
             (< my-outstanding-tasks-index (length my-outstanding-tasks-list)))
    (setq my-outstanding-tasks-index (1+ my-outstanding-tasks-index))
    ;; Handle wraparound if we go past the end
    (when (>= my-outstanding-tasks-index (length my-outstanding-tasks-list))
      (setq my-outstanding-tasks-index 0)))
  
  ;; Save the updated index
  (my-save-index-to-file)
  
  ;; Now show the task at the current index
  (if (and my-outstanding-tasks-list
           (< my-outstanding-tasks-index (length my-outstanding-tasks-list)))
      (let ((task-or-marker (nth my-outstanding-tasks-index my-outstanding-tasks-list)))
        ;; Display operations
        (my-display-task-at-marker task-or-marker)
        (my-pulse-highlight-current-line)
        
        ;; Handle SRS reviews
        (if my-android-p
            ;; On Android: skip SRS, just launch Anki
            (my-launch-anki)
          ;; On desktop: use SRS integration
          (if (not my-srs-reviews-exhausted)
              (progn
                (my-srs-quit-reviews)
                (condition-case nil
                    (my-srs-start-reviews)
                  (error (setq my-srs-reviews-exhausted t))))
            (my-launch-anki)))

        (my-show-current-flag-status))
    (message "No outstanding tasks found.")))

(defun my-show-previous-outstanding-task ()
  "Show the previous outstanding task in priority order, cycling if needed."
  (interactive)

  (widen-and-recenter)
  
  ;; Ensure we have synchronized data
  (my-ensure-synchronized-task-list)
  
  (if my-outstanding-tasks-list
      (progn
        ;; Update index BEFORE showing the task for consistency
        (if (<= my-outstanding-tasks-index 0)
            (setq my-outstanding-tasks-index (1- (length my-outstanding-tasks-list)))
          (setq my-outstanding-tasks-index (1- my-outstanding-tasks-index)))
        
        ;; Save the updated index
        (my-save-index-to-file)
        
        (let ((task-or-marker (nth my-outstanding-tasks-index my-outstanding-tasks-list)))
          (my-display-task-at-marker task-or-marker)
          (my-pulse-highlight-current-line)
          (my-show-current-flag-status)))
    (message "No outstanding tasks to navigate.")))

(defun my-show-current-outstanding-task ()
  "Show the current outstanding task, or get a new list and show the first task if not valid."
  (interactive)

  (widen-and-recenter)
  
  ;; Ensure we have synchronized data
  (my-ensure-synchronized-task-list)
    
  (if (and my-outstanding-tasks-list 
           (< my-outstanding-tasks-index (length my-outstanding-tasks-list)))
      (let ((task-or-marker (nth my-outstanding-tasks-index my-outstanding-tasks-list)))
        ;; Display operations
        (my-display-task-at-marker task-or-marker)
        (my-pulse-highlight-current-line)
        (my-show-current-flag-status))
    ;; Truly no tasks - unlikely after synchronization above
    (message "No outstanding tasks found.")))

(defun my-reset-outstanding-tasks-index ()
  "Reset the outstanding tasks index to start from the first task."
  (interactive)
  (my-get-outstanding-tasks)
  (setq my-outstanding-tasks-index 0)
  (my-save-outstanding-tasks-to-file)  ;; This will save both tasks and index
  (message "Outstanding tasks index reset."))

(defun my-reset-and-show-current-outstanding-task ()
  "Reset the outstanding tasks index and then show the current outstanding task."
  (interactive)  ;; Allows the function to be executed via M-x in Emacs
  (my-reset-outstanding-tasks-index)  ;; Call function to reset tasks index
  (my-show-current-outstanding-task))  ;; Call function to show the first/current task

(defun my-auto-task-setup ()
  "Initialize and set up automatic task management processes upon Emacs startup."
  (message "Starting automatic task setup...")
  
  ;; Enable debug on error to get better backtraces
  (setq debug-on-error t)
  
  ;; Add tracing for each step
  (condition-case err
      (progn
        (message "Step 1: Attempting to load tasks from file...")
        (let ((cache-loaded (my-load-outstanding-tasks-from-file)))
          (message "Step 1 complete: Cache loaded? %s" cache-loaded)
          
          (message "Checking task format - First task: %S" 
                   (and my-outstanding-tasks-list 
                        (car my-outstanding-tasks-list)))
          
          (if cache-loaded
              (progn
                (message "Step 2A: Cache exists, processing...")
                (when (boundp 'my-srs-reviews-exhausted)
                  (setq my-srs-reviews-exhausted nil))
                (message "Step 2A complete: About to schedule task display"))
            
            (progn
              (message "Step 2B: No cache, running maintenance...")
              (when (require 'org-roam nil t)
                (message "Step 2B.1: Setting up org-roam...")
                (org-roam-db-autosync-mode)
                (org-id-update-id-locations (org-agenda-files))
                (message "Step 2B.1 complete"))
              
              (message "Step 2B.2: Running maintenance operations...")
              (message "Step 2B.2.1: Ensuring priorities and schedules...")
              (my-ensure-priorities-and-schedules-for-all-headings)
              
              (message "Step 2B.2.2: Advancing schedules...")
              (my-auto-advance-schedules 8)
              
              (message "Step 2B.2.3: Postponing overdue tasks...")
              (my-auto-postpone-overdue-tasks)
              
              (message "Step 2B.2.4: Postponing duplicate priority tasks...")
              (my-postpone-duplicate-priority-tasks)
              
              (message "Step 2B.2.5: Enforcing priority constraints...")
              (my-enforce-priority-constraints)
              
              (message "Step 2B.2.6: Re-ensuring priorities and schedules...")
              (my-ensure-priorities-and-schedules-for-all-headings)
              
              (message "Step 2B.2.7: Postponing consecutive same-file tasks...")
              (my-postpone-consecutive-same-file-tasks)
              
              (message "Step 2B.3: Getting outstanding tasks...")
              (my-get-outstanding-tasks)
              (setq my-outstanding-tasks-index 0)
              
              (message "Step 2B.4: Saving tasks to file...")
              (my-save-outstanding-tasks-to-file)
              (message "Maintenance complete"))))
        
        (message "Step 3: Enabling org-queue-mode...")
        (org-queue-mode 1)
      	
        ;; Step 4: Initialize SRS system for faster future access
        (message "Step 4: Pre-initializing SRS system...")
	(unless my-android-p
	  (if (not my-srs-reviews-exhausted)
	      (progn
		(my-srs-quit-reviews)
		(let ((temp-frame (make-frame '((visibility . nil) (width . 80) (height . 24)))))
		  (unwind-protect
		      (with-selected-frame temp-frame
			(cl-letf (((symbol-function 'read-key) (lambda (&rest _) 32)))
			  (condition-case nil
			      (my-srs-start-reviews)
			    (error (setq my-srs-reviews-exhausted t))))
			(my-srs-quit-reviews))
		    (delete-frame temp-frame))))
	    (my-launch-anki)))
        
        (message "✓ Automatic task setup completed successfully.")
        
        ;; Schedule task display - this is done whether cache exists or not
        (run-with-idle-timer 1.5 nil
                             (lambda ()
                               (condition-case err
                                   (if (and my-outstanding-tasks-list
                                            (> (length my-outstanding-tasks-list) 0)
                                            (< my-outstanding-tasks-index (length my-outstanding-tasks-list)))
                                       (let ((task-or-marker (nth my-outstanding-tasks-index my-outstanding-tasks-list)))
                              		 ;; Use safe display function
                              		 (my-display-task-at-marker task-or-marker)
                              		 (my-show-current-flag-status))
                                     (message "Task list empty or invalid index"))
                              	 (error
                                  (message "Error preparing task display: %s" (error-message-string err))))))
        (message "Task display scheduled.")

        ;; Disable debug mode after successful setup
        (setq debug-on-error nil))
    
    (error
     (message "❌ Error during task setup: %s" (error-message-string err))
     (message "Task that caused error: %S" 
              (and my-outstanding-tasks-list 
                   (nth (or my-outstanding-tasks-index 0) 
                        my-outstanding-tasks-list)))
     
     (message "Backtrace: %S" (with-output-to-string (backtrace)))
     
     ;; Try emergency generation with safety
     (unless my-outstanding-tasks-list
       (message "Attempting emergency task list generation...")
       (condition-case err-emergency
           (progn
             (my-get-outstanding-tasks)
             (setq my-outstanding-tasks-index 0)
             (message "Generated emergency task list with %d tasks." 
                      (length my-outstanding-tasks-list)))
         (error
          (message "Emergency task generation also failed: %s" 
                   (error-message-string err-emergency)))))
     
     ;; Disable debug mode after handling error
     (setq debug-on-error nil))))

(defun my-emergency-reset ()
  "Emergency reset function that clears all task cache data and rebuilds."
  (interactive)
  (message "Performing emergency reset...")
  
  ;; Clear all variables
  (setq my-outstanding-tasks-list nil)
  (setq my-outstanding-tasks-index 0)
  (when (file-exists-p my-outstanding-tasks-cache-file)
    (delete-file my-outstanding-tasks-cache-file))
  
  ;; Rebuild the task list
  (my-get-outstanding-tasks)
  (my-save-outstanding-tasks-to-file)
  (message "Reset complete. Generated %d tasks." 
           (length my-outstanding-tasks-list)))

(add-hook 'emacs-startup-hook #'my-auto-task-setup 100)

(provide 'org-queue)
