;;; org-queue-display.el --- Display and UI functions for org-queue -*- lexical-binding: t -*-

;;; Code:

(require 'pulse)
(require 'seq)
(require 'subr-x)
(require 'cl-lib)
(require 'org-queue-config)
(require 'org-queue-utils)
(require 'org-queue-srs-bridge)
(require 'org-queue-tasks)
(require 'org-queue-priority)

(defun my--queue-task-candidates ()
  "Return an ordered list of (display . index) for the current queue.
Display shows \"NN. Title — file\". Titles may be \"(untitled)\"."
  (let ((cands nil)
        (len (length my-outstanding-tasks-list)))
    (dotimes (i len)
      (let* ((task (nth i my-outstanding-tasks-list))
             (buf (my-safe-marker-buffer task))
             (head (with-current-buffer (or buf (current-buffer))
                     (save-excursion
                       (let ((m (my-extract-marker task)))
                         (when (markerp m)
                           (goto-char (marker-position m))
                           (or (org-get-heading t t t t) ""))))))
             (title (if (and head (> (length head) 0)) head "(untitled)"))
             (file (plist-get task :file))
             (file-short (and file (file-name-nondirectory file)))
             (disp (format "%4d. %s — %s"
                           (1+ i)
                           title
                           (or file-short ""))))
        (push (cons disp i) cands)))
    (nreverse cands)))

(defun my--completion-table-preserve-order (candidates)
  "Completion table over CANDIDATES that preserves their order."
  (lambda (string pred action)
    (if (eq action 'metadata)
        '(metadata (display-sort-function . identity)
                   (cycle-sort-function . identity))
      (complete-with-action action candidates string pred))))

(defun my-queue-switch-to-task ()
  "Choose a task from the queue in queue order and jump to it.
Updates `my-outstanding-tasks-index` and shows the task."
  (interactive)
  (my-ensure-synchronized-task-list)
  (if (null my-outstanding-tasks-list)
      (message "Queue is empty.")
    (let* ((pairs (my--queue-task-candidates))
           (table (my--completion-table-preserve-order (mapcar #'car pairs)))
           (default (caar pairs))
           (choice (completing-read "Switch to task: " table nil t nil nil default))
           (idx (cdr (assoc choice pairs))))
      (when (numberp idx)
        (setq my-outstanding-tasks-index idx)
        (my-save-index-to-file)
        (my-show-current-outstanding-task)))))

;; Display and UI faces
(defface org-clozed-face
  '((t (:background "#E67300" :foreground "black")))
  "Face for clozed text in org-mode.")

(defface org-extract-face
  '((t (:background "#44C2FF" :foreground "black")))
  "Face for extracted text in org-mode.")

(defvar-local org-custom-overlays nil
  "List of overlays for custom syntax highlighting.")

;; Flag display constants and functions
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

(defun my-show-current-flag-status ()
  "Show flag info, TODO status, and current outstanding task index."
  (interactive)
  (let ((data (my-current-flag-counts))
        (tasks (if (boundp 'my-outstanding-tasks-list)
                   (length my-outstanding-tasks-list)
                 nil))
        (idx (if (and (boundp 'my-outstanding-tasks-index)
                      (boundp 'my-outstanding-tasks-list)
                      my-outstanding-tasks-list)
                 (1+ my-outstanding-tasks-index)
               nil))
        (todo-status (when (and (boundp 'my-outstanding-tasks-list)
                               (boundp 'my-outstanding-tasks-index)
                               my-outstanding-tasks-list
                               (< my-outstanding-tasks-index (length my-outstanding-tasks-list)))
                       (let ((current-task (nth my-outstanding-tasks-index my-outstanding-tasks-list)))
                         (plist-get current-task :is-todo)))))
    (if data
        (let* ((flag-name (plist-get data :flag-name))
               (flag-num (plist-get data :flag-num))
               (fidx (plist-get data :flag-pos))
               (ftotal (plist-get data :flag-count))
               (fleft (plist-get data :flag-left))
               (tleft (if (and idx tasks) (- tasks idx) nil))
               (todo-indicator (cond ((eq todo-status t) "TODO")
                                   ((eq todo-status nil) "OTHER")
                                   (t "UNKNOWN"))))  ; Changed from "?" to "UNKNOWN"
          (message "%s (%d of %d, %d left) | %s Task %d/%d, %d left"
                   (or flag-name (format "Flag %s" (or flag-num "UNKNOWN")))  ; Safe string
                   (or fidx 0) (or ftotal 0) (or fleft 0)  ; Ensure numbers
                   todo-indicator
                   (or idx 0)
                   (or tasks 0)
                   (or tleft 0)))
      (let ((tleft (if (and idx tasks) (- tasks idx) nil))
            (todo-indicator (cond ((eq todo-status t) "TODO")
                                ((eq todo-status nil) "OTHER")
                                (t "UNKNOWN"))))  ; Changed from "?" to "UNKNOWN"
        (message "No current flag info. %s Task %d/%d, %d left"
                 todo-indicator
                 (or idx 0)
                 (or tasks 0)
                 (or tleft 0))))))

(defun my-anki-running-p ()
  (cond
   ((eq system-type 'darwin)
    (zerop (call-process "pgrep" nil nil nil "-x" "Anki")))
   ((eq system-type 'gnu/linux)
    (or (zerop (call-process "pgrep" nil nil nil "-x" "anki"))
	(zerop (call-process "pgrep" nil nil nil "-x" "anki.bin"))))
   ((eq system-type 'windows-nt)
    (with-temp-buffer
      (call-process "tasklist" nil t nil "/FI" "IMAGENAME eq anki.exe")
      (goto-char (point-min))
      (search-forward "anki.exe" nil t)))))

;; Anki launch function
(defun my-launch-anki ()
  "Run or focus Anki; never spawn duplicate instances.
Prereqs:
- my-anki-running-p: t when Anki is running.
- (optional) my-android-p: t on Android."
  (interactive)
  (cond
   ;; Android
   ((and (fboundp 'my-android-p) (my-android-p))
    (message "Please open the Anki app manually on Android."))

   ;; macOS: focus if running (AppleScript), else launch (open -a).
   ((eq system-type 'darwin)
    (if (and (fboundp 'my-anki-running-p) (my-anki-running-p))
        (start-process "anki-activate" nil "osascript" "-e"
                       "tell application id \"net.ankiweb.dtop\" to activate")
      (start-process "anki-open" nil "/usr/bin/open" "-a" "Anki")))

   ;; Windows
   ((eq system-type 'windows-nt)
    (if (and (fboundp 'my-anki-running-p) (my-anki-running-p))
        (ignore-errors
          (start-process "anki-activate" nil "powershell" "-NoProfile" "-Command"
                         "$p=Get-Process -Name anki -ErrorAction SilentlyContinue; if($p){$ws=New-Object -ComObject WScript.Shell; $ws.AppActivate($p.Id)}"))
      (w32-shell-execute "open" "anki.exe")))

   ;; GNU/Linux
   (t
    (if (and (fboundp 'my-anki-running-p) (my-anki-running-p))
        (cond
         ((executable-find "wmctrl")
          (or (zerop (call-process "wmctrl" nil nil nil "-x" "-a" "anki.Anki"))
              (zerop (call-process "wmctrl" nil nil nil "-a" "Anki"))
              (message "Anki is running; could not raise via wmctrl.")))
         ((executable-find "xdotool")
          (start-process "anki-activate" nil "xdotool"
                         "search" "--onlyvisible" "--class" "anki"
                         "windowactivate" "--sync"))
         (t
          (message "Anki is running; install wmctrl/xdotool to raise it.")))
      (let ((exe (or (executable-find "anki")
                     (executable-find "anki.bin"))))
        (if exe
            (start-process "Anki" nil exe)
          (message "Anki executable not found on PATH.")))))))

;; Display and highlighting functions
(defun my-pulse-highlight-current-line (&optional time)
  "Temporarily pulse-highlight the current line.

Optional argument TIME specifies the delay between pulse iterations in seconds.
Defaults to 0.2 seconds."
  (let ((original-pulse-iterations (and (boundp 'pulse-iterations) pulse-iterations))
        (original-pulse-delay (and (boundp 'pulse-delay) pulse-delay)))
    (unwind-protect
        (progn
          (setq pulse-iterations 3)
          (setq pulse-delay (or time 0.2))
          (pulse-momentary-highlight-one-line (point)))
      (when (boundp 'pulse-iterations)
        (setq pulse-iterations original-pulse-iterations))
      (when (boundp 'pulse-delay)
        (setq pulse-delay original-pulse-delay)))))

(defun widen-and-recenter ()
  "Widen buffer, reset folding, show top-level children, and recenter point."
  (interactive)
  (when (eq major-mode 'org-mode)
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
      (ignore-errors (org-highlight-custom-syntax)))))

(defun org-show-parent-heading-cleanly ()
  "Move up to the parent heading, widen the buffer, and then reveal the parent heading along with its children."
  (interactive)
  (widen-and-recenter)
  (ignore-errors (outline-up-heading 1)) ;; Move up heading (safely handle errors)
  (org-narrow-to-subtree)
  (org-highlight-custom-syntax))

;; Add this to your Emacs config - never ask about reverting
(setq revert-without-query '(".*"))

(defun my-queue-register-visit (_buf)
  "No-op. Keep native C-x b behavior."
  nil)

(defun my-display-task-at-marker (task-or-marker)
  "Display the task at TASK-OR-MARKER with appropriate visibility settings.
Always resolves the live position by :id if available (lazy and per-ID).
Falls back to the existing marker only when ID cannot be resolved.
Also registers the displayed buffer as a recent queue buffer."
  (let* ((id   (and (listp task-or-marker) (plist-get task-or-marker :id)))
         (file (and (listp task-or-marker) (plist-get task-or-marker :file)))
         ;; Start with whatever we have, but we will prefer resolving by ID below.
         (marker (my-extract-marker task-or-marker))
         (buf    (and marker (marker-buffer marker)))
         (pos    (and marker (marker-position marker))))
    (when (and buf (buffer-live-p buf))
      (condition-case err
          (progn
            (switch-to-buffer buf)
            (when (buffer-file-name buf)
              (let ((revert-without-query '(".*")))
                (revert-buffer t t t)))
            (widen-and-recenter)

            ;; Always resolve by ID if we have one (lazy, targeted, per call)
            (when id
              (let ((resolved
                     (or (org-id-find id 'marker)
                         (progn
                           (when (or file (buffer-file-name))
                             (ignore-errors
                               (org-id-update-id-locations
                                (list (file-truename (or file (buffer-file-name)))))))
                           (org-id-find id 'marker)))))
                (when (markerp resolved)
                  (setq marker resolved
                        buf    (marker-buffer resolved)
                        pos    (marker-position resolved))
                  (unless (eq (current-buffer) buf)
                    (switch-to-buffer buf)))))

            ;; Final position fallback if ID was missing or not resolved
            (setq pos (or pos (and (markerp marker) (marker-position marker)) (point-min)))
            (goto-char pos)
            ;; Make sure we are on the heading before narrowing/revealing
            (when (derived-mode-p 'org-mode)
              (org-back-to-heading t))

            (org-narrow-to-subtree)
            (org-overview)
            (org-reveal t)
            (org-show-entry)
            (org-show-children)

            (when (eq buf (window-buffer (selected-window)))
              (recenter)))
        (error
         (message "Error displaying task: %s" (error-message-string err)))))))

;; Custom syntax highlighting functions
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
      ;; Simple approach for now - just handle basic cases
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
               
               ;; Handle closing braces - only count when not in special context
               ((and (= char-at-point ?}) (looking-at "}}")
                     (not context-stack))
                (setq brace-level (1- brace-level))
                (if (zerop brace-level)
                    (progn
                      (setq found-end (+ (point) 2))
                      (forward-char 2))
                  (forward-char 2)))
               
               ;; Default: just move forward one character
               (t
                (forward-char 1)))))
          
          ;; Apply highlighting if we found the end
          (when found-end
            ;; Create overlay for the entire construct
            (let ((main-overlay (make-overlay start found-end))
                  ;; Extract the content between the markers
                  (content-text (buffer-substring-no-properties content-start (- found-end 2))))
              
              ;; Set the face for the overlay
              (overlay-put main-overlay 'face
                           (cond
                            ((string= type "extract") 'org-extract-face)
                            ((string= type "clozed") 'org-clozed-face)
                            (t 'org-extract-face)))  ; Default to extract face
              
              ;; Hide the opening and closing markers, show only content
              (overlay-put main-overlay 'display content-text)
              
              (push main-overlay org-custom-overlays))))))))

(defun org-refresh-all-custom-highlighting ()
  "Refresh custom syntax highlighting in all org-mode buffers."
  (interactive)
  (dolist (buffer (buffer-list))
    (with-current-buffer buffer
      (when (eq major-mode 'org-mode)
        (org-highlight-custom-syntax)))))

(defun org-refresh-custom-highlighting ()
  "Refresh custom syntax highlighting in the current buffer if it's an org-mode buffer."
  (interactive)
  (when (eq major-mode 'org-mode)
    (org-highlight-custom-syntax)))

;; Interactive content creation functions
(defun my--strip-leading-stars (s)
  "Remove leading stars + spaces from S to avoid creating a new heading."
  (if (string-match "^\\*+\\s-*" s)
      (substring s (match-end 0))
    s))

(defun org-interactive-cloze ()
  "Create a cloze deletion from selected text; header-aware.
- If region is on a heading line, cloze context is built from the heading
  display. In the child body,
  leading stars are stripped to avoid creating a new heading.
- Also mirrors extract: inherits parent priority range and sets child priority."
  (interactive)
  (if (not (use-region-p))
      (message "Please select text to create a cloze")
    (let* ((start (region-beginning))
           (end (region-end))
           (selected-text (buffer-substring-no-properties start end))
           ;; Current heading pos/level
           (heading-pos (save-excursion (goto-char start) (org-back-to-heading t) (point)))
           (heading-level (save-excursion (goto-char heading-pos) (org-current-level)))
           (parent-priority-range (save-excursion
                                   (goto-char heading-pos)
                                   (my-get-current-priority-range)))
           ;; Will be recomputed after insertion
           (content-line nil)
           (on-heading (save-excursion
                         (goto-char start)
                         (beginning-of-line)
                         (org-at-heading-p))))
      ;; Replace selected text with cloze
      (delete-region start end)
      (insert (format "{{clozed:%s}}" selected-text))

      ;; Get updated content line
      (setq content-line (buffer-substring-no-properties
                          (line-beginning-position)
                          (line-end-position)))

      ;; If this is a heading line, build a clean heading display string
      (when on-heading
        ;; Prevent creating a heading inside the child body: drop leading stars.
        (setq content-line (my--strip-leading-stars content-line)))

      ;; Create child heading with cloze placeholder
      (save-excursion
        (goto-char heading-pos)
        (org-end-of-subtree)
        (let ((new-heading-pos (point)))
          (insert "\n" (make-string (1+ heading-level) ?*) " ")
          ;; Replace the current cloze with [...]
          (let* ((child-content
                  (replace-regexp-in-string
                   (regexp-quote (format "{{clozed:%s}}" selected-text))
                   "[...]"
                   content-line))
                 (processed-content child-content))
            ;; Replace any other clozes with their content
            (while (string-match "{{clozed:\\([^}]+\\)}}" processed-content)
              (setq processed-content
                    (replace-match "\\1" t nil processed-content)))
            (insert processed-content))
          (insert "\n" selected-text)
          ;; Priority (mirror extract), ID, and org-srs
          (when parent-priority-range
            (goto-char (1+ new-heading-pos))
            (my-set-priority-with-heuristics parent-priority-range))
          (goto-char (1+ new-heading-pos))
          (org-id-get-create)
          (when (fboundp 'org-srs-item-new)
            (org-srs-item-new 'card))))
      (message "Created cloze deletion%s"
               (if (fboundp 'org-srs-item-new) " with org-srs card" "")))))

(defun org-interactive-extract ()
  "Create a SuperMemo-style extract from selected text and generate a child heading."
  (interactive)
  (if (not (use-region-p))
      (message "Please select text to extract")
    (let* ((start (region-beginning))
           (end (region-end))
           (selected-text (buffer-substring-no-properties start end))
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
           (ends-with-newline (and (> end start)
                                  (= (char-before end) ?\n))))
      
      ;; Clean the text
      (setq cleaned-text (replace-regexp-in-string "\\^{[^}]*}" "" cleaned-text))
      (setq cleaned-text (replace-regexp-in-string "  +" " " cleaned-text))
      
      ;; Replace original text with extract marker
      (delete-region start end)
      (if ends-with-newline
          (let ((text-without-newline (substring selected-text 0 -1)))
            (insert (format "{{extract:%s}}\n" text-without-newline)))
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

(defun org-remove-all-extract-blocks ()
  "Remove all extract blocks entirely from the current buffer (including their content).
This is useful after extracting content to child headings - removes the extracted text 
from the original location to help focus on remaining unprocessed text."
  (interactive)
  (when (eq major-mode 'org-mode)
    ;; Clear overlays first so we can see the actual text
    (org-clear-custom-overlays)
    (save-excursion
      (goto-char (point-min))
      (let ((count 0))
        ;; Simple approach: find {{extract:...}} blocks and remove them entirely
        (while (re-search-forward "{{extract:[^}]*}}" nil t)
          (delete-region (match-beginning 0) (match-end 0))
          (setq count (1+ count)))
        
        ;; Clean up consecutive empty lines after removal  
        (goto-char (point-min))
        (while (re-search-forward "\n\n\n+" nil t)
          (replace-match "\n\n"))
        
        (message "Removed %d extract blocks and cleaned up empty lines" count))
      ;; Refresh highlighting after removal
      (org-highlight-custom-syntax))))

;; Help function
(defun org-queue-show-help ()
  "Display comprehensive help documentation for org-queue-mode.
Includes all commands, access methods, and usage examples."
  (interactive)
  (with-help-window "*Org Queue Help*"
    (princ "Org Queue Mode - Comprehensive Help\n")
    (princ "====================================\n\n")
    (princ "Org Queue is a comprehensive task management system for Org mode.\n")
    (princ "It provides intelligent task navigation, priority management, and learning tools.\n\n")
    
    (princ "Navigation Commands:\n")
    (princ "  f - Next outstanding task\n")
    (princ "  b - Previous outstanding task\n")
    (princ "  c - Show current outstanding task\n")
    (princ "  u - Show parent heading context\n\n")
    
    (princ "View Control:\n")
    (princ "  w - Widen and recenter view\n")
    (princ "  n - Narrow to current subtree\n\n")
    
    (princ "Task Operations:\n")
    (princ "  r - Remove current task from queue\n")
    (princ "  R - Reset and show current task\n\n")
    
    (princ "Structure Editing:\n")
    (princ "  W - Cut subtree\n")
    (princ "  Y - Paste subtree\n")
    (princ "  D - Demote subtree level\n")
    (princ "  P - Promote subtree level\n\n")
    
    (princ "Content Creation:\n")
    (princ "  x - Create extract block (SuperMemo-style)\n")
    (princ "  X - Remove all extract blocks\n\n")
    
    (when (featurep 'org-srs)
      (princ "SRS & Review:\n")
      (princ "  z - Create cloze deletion\n")
      (princ "  Z - Create SRS card in current entry\n")
      (princ "  1 - Rate again (difficult)\n")
      (princ "  2 - Rate hard (if available)\n")
      (princ "  3 - Rate good (easy)\n\n"))
    
    (princ "Priority Control:\n")
    (princ "  , - Set priority with heuristics\n")
    (princ "  i - Increase priority range (lower numbers)\n")
    (princ "  d - Decrease priority range (higher numbers)\n\n")
    
    (princ "Schedule Control:\n")
    (princ "  s - Schedule task with priority\n")
    (princ "  a - Advance current schedule\n")
    (princ "  p - Postpone current schedule\n\n")
    
    (when (featurep 'org-web-tools)
      (princ "Web Integration:\n")
      (princ "  I - Insert web page as org entry\n")
      (princ "  l - Insert link for URL\n\n"))
    
    (when (featurep 'gptel)
      (princ "AI Integration:\n")
      (princ "  g - Start GPT chat session\n\n"))
    
    (princ "System Commands:\n")
    (princ "  t - Toggle auto-enable mode\n")
    (princ "  e - Exit org-queue-mode\n")
    (princ "  ? - Show this help\n\n")
    
    (princ "Access Methods:\n")
    (princ "  • Direct keys: Use commands directly in org buffers\n")
    (princ "  • Prefix key: C-; followed by command key\n")
    (princ "  • Menu Bar: 'Org Queue' menu when mode is active\n")
    (princ "  • Context menu: Right-click in org-mode buffers\n")
    (princ "  • Toolbar: GUI navigation buttons (if available)\n\n")
    
    (princ "Usage Tips:\n")
    (princ "  • Use heuristic priority setting for smart task ordering\n")
    (princ "  • Extract blocks help with incremental reading workflows\n")
    (princ "  • Schedule management supports spaced repetition patterns\n")
    (princ "  • All features integrate seamlessly with standard org-mode\n")
    (princ "  • Detailed categorization in menu helps discover features\n\n")
    
    (princ "For detailed information about specific commands:\n")
    (princ "  M-x describe-function RET <command-name> RET\n")))

(defvar org-custom-syntax-timer nil
  "Timer for delayed custom syntax highlighting.")

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

(add-hook 'org-mode-hook 'org-highlight-custom-syntax)
(add-hook 'org-mode-hook
          (lambda ()
            (add-hook 'after-change-functions 
                      'org-update-custom-syntax-after-change nil t)))

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

;; Queue chooser (tabulated-list-mode) — fixed-width columns, marking, subset mode, schedule/prio tools
(require 'tabulated-list)
(require 'org)
(require 'cl-lib)
(require 'org-queue-priority)  ;; for my-queue-spread-priorities
(require 'org-queue-schedule)  ;; for my-advance-schedule / my-postpone-schedule

(defgroup org-queue-chooser nil
  "Chooser UI for org-queue."
  :group 'org-queue)

(defcustom org-queue-chooser-buffer-name "*Org Queue*"
  "Name of the queue chooser buffer."
  :type 'string
  :group 'org-queue-chooser)

(defcustom org-queue-chooser-open-in-tab nil
  "If non-nil, open the chooser in a dedicated tab-bar tab."
  :type 'boolean
  :group 'org-queue-chooser)

(defcustom org-queue-chooser-mark-width 1
  "Width of the mark column."
  :type 'integer
  :group 'org-queue-chooser)

(defcustom org-queue-chooser-index-width 5
  "Width of the index column."
  :type 'integer
  :group 'org-queue-chooser)

(defcustom org-queue-chooser-priority-width 4
  "Width of the numeric priority column."
  :type 'integer
  :group 'org-queue-chooser)

(defcustom org-queue-chooser-title-width 40
  "Width of the Title column."
  :type 'integer
  :group 'org-queue-chooser)

(defcustom org-queue-chooser-preview-width 40
  "Width of the Preview column."
  :type 'integer
  :group 'org-queue-chooser)

(defcustom org-queue-chooser-sched-width 10
  "Width of the Scheduled column (YYYY-MM-DD)."
  :type 'integer
  :group 'org-queue-chooser)

(defcustom org-queue-chooser-file-width 28
  "Width of the File column."
  :type 'integer
  :group 'org-queue-chooser)

(defface org-queue-chooser-index-face
  '((t (:inherit (fixed-pitch shadow))))
  "Face for the index column."
  :group 'org-queue-chooser)

(defface org-queue-chooser-priority-face
  '((t (:inherit (fixed-pitch bold))))
  "Face for the priority column."
  :group 'org-queue-chooser)

(defface org-queue-chooser-title-face
  '((t (:inherit (fixed-pitch bold))))
  "Face for the title column."
  :group 'org-queue-chooser)

(defface org-queue-chooser-file-face
  '((t (:inherit (fixed-pitch shadow))))
  "Face for the file column."
  :group 'org-queue-chooser)

(defface org-queue-chooser-preview-face
  '((t (:inherit (fixed-pitch shadow))))  ; removed 'italic' 
  "Face for the preview column."
  :group 'org-queue-chooser)

(defface org-queue-chooser-mark-face
  '((t (:inherit (fixed-pitch bold))))
  "Face for the mark column."
  :group 'org-queue-chooser)

(defface org-queue-chooser-sched-face
  '((t (:inherit (fixed-pitch shadow))))
  "Face for the scheduled column."
  :group 'org-queue-chooser)

(defvar org-queue-chooser-mode-map
  (let ((map (make-sparse-keymap)))
    ;; Row-wise movement (skip wrapped lines)
    (define-key map (kbd "j") #'org-queue-chooser-next-row)
    (define-key map (kbd "k") #'org-queue-chooser-previous-row)
    ;; Visit current selection
    (define-key map (kbd "RET") #'org-queue-chooser-visit)
    (define-key map (kbd "f")   #'org-queue-chooser-visit)
    (define-key map (kbd "o")   #'org-queue-chooser-visit-other-window)
    ;; Marking (Dired-like)
    (define-key map (kbd "m") #'org-queue-chooser-mark)
    (define-key map (kbd "u") #'org-queue-chooser-unmark)
    (define-key map (kbd "t") #'org-queue-chooser-toggle-mark)
    (define-key map (kbd "U") #'org-queue-chooser-unmark-all)
    (define-key map (kbd "M") #'org-queue-chooser-mark-region)
    ;; Bulk operations on selection
    (define-key map (kbd "S") #'org-queue-chooser-spread-schedule)  ;; schedule spread
    (define-key map (kbd "H") #'org-queue-chooser-spread-priorities) ;; priority spread
    (define-key map (kbd "a") #'org-queue-chooser-advance-marked)
    (define-key map (kbd "p") #'org-queue-chooser-postpone-marked)
    (define-key map (kbd "O") #'org-queue-chooser-add-to-outstanding)
    ;; Subset from current buffer/visible region
    (define-key map (kbd "C") #'org-queue-open-chooser-from-buffer)
    ;; Tabs
    (define-key map (kbd "T")     #'org-queue-chooser-visit-in-new-tab)
    (define-key map (kbd "C-c t") #'org-queue-chooser-visit-in-new-tab-foreground)
    ;; Refresh
    (define-key map (kbd "g") #'org-queue-chooser-refresh)
    ;; Open chooser in its own tab (foreground)
    (define-key map (kbd "C-c T") #'org-queue-chooser-open-in-tab)
    ;; Reorder tasks (only valid for global queue, not for subset)
    (define-key map (kbd "M-n")      #'org-queue-chooser-move-down)
    (define-key map (kbd "M-p")      #'org-queue-chooser-move-up)
    (define-key map (kbd "M-<down>") #'org-queue-chooser-move-down)
    (define-key map (kbd "M-<up>")   #'org-queue-chooser-move-up)
    (define-key map (kbd "M-g M-g")  #'org-queue-chooser-move-to-position)
    ;; Quit
    (define-key map (kbd "q") #'quit-window)
    ;; Disable 'n' to prevent conflict with next-line from tabulated-list-mode
    (define-key map (kbd "n") #'ignore)
    ;; Optionally disable 'N' and 'P' if desired
    ;; (define-key map (kbd "N") #'ignore)
    ;; (define-key map (kbd "P") #'ignore)
    map)
  "Keymap for org-queue-chooser-mode.")

(define-derived-mode org-queue-chooser-mode tabulated-list-mode "Org-Queue-Chooser"
  "Browse and operate on queue tasks (global or subset)."
  ;; Define the table format for the queue chooser
  (setq tabulated-list-format
        (vector
         (list (org-queue-chooser--truncate-pad "M" org-queue-chooser-mark-width)
               org-queue-chooser-mark-width nil)
         (list (org-queue-chooser--truncate-pad "#" org-queue-chooser-index-width)
               org-queue-chooser-index-width t :right-align t)
         (list (org-queue-chooser--truncate-pad "Pri" org-queue-chooser-priority-width)
               org-queue-chooser-priority-width t :right-align t)
         (list (org-queue-chooser--truncate-pad "Title" org-queue-chooser-title-width)
               org-queue-chooser-title-width nil)
         (list (org-queue-chooser--truncate-pad "Preview" org-queue-chooser-preview-width)
               org-queue-chooser-preview-width nil)
         (list (org-queue-chooser--truncate-pad "Sched" org-queue-chooser-sched-width)
               org-queue-chooser-sched-width t)
         (list (org-queue-chooser--truncate-pad "File" org-queue-chooser-file-width)
               org-queue-chooser-file-width t)))
  (setq tabulated-list-padding 2)
  (setq tabulated-list-sort-key nil)
  ;; Use nil to print header as first row (same font/face as body)
  (setq tabulated-list-use-header-line nil)
  
  ;; Fixed pitch for exact alignment
  (setq-local face-remapping-alist
              (append '((default fixed-pitch)
                        (header-line fixed-pitch)
                        (tabulated-list-header fixed-pitch)
                        (tabulated-list-header-face fixed-pitch))
                      face-remapping-alist))
  
  ;; Force all faces to use the same mono family with normal weight/slant
  (setq-local indent-tabs-mode nil)
  (setq-local use-default-font-for-symbols t)
  
  ;; Force everything to normal mono, no bold/italic
  (let* ((fam (face-attribute 'fixed-pitch :family nil t)))
    (when fam
      ;; Force all column faces to use the same mono family with normal weight/slant
      (dolist (f '(org-queue-chooser-index-face
                   org-queue-chooser-file-face
                   org-queue-chooser-sched-face
                   org-queue-chooser-mark-face
                   org-queue-chooser-priority-face
                   org-queue-chooser-title-face
                   org-queue-chooser-preview-face))
        (face-remap-add-relative f `(:family ,fam :weight normal :slant normal)))))
  
  ;; Extra insurance: explicitly force preview face to normal weight/slant
  (face-remap-add-relative 'org-queue-chooser-preview-face 
                           '(:weight normal :slant normal))
  
  ;; Also ensure the header uses the same fixed pitch
  (when header-line-format
    (face-remap-add-relative 'header-line 
                             '(:family "monospace" :weight normal :slant normal)))
  
  ;; Ensure symbol blocks use mono font (prevents stray proportional glyphs)
  (let* ((fam (face-attribute 'fixed-pitch :family nil t))
         (font (and fam (font-spec :family fam))))
    (when font
      (dolist (range '((#x2000 . #x206F)  ; General Punctuation
                       (#x2190 . #x21FF)  ; Arrows
                       (#x2200 . #x22FF)  ; Math Operators
                       (#x2300 . #x23FF)  ; Misc Technical
                       (#x2500 . #x257F)  ; Box Drawing
                       (#x2580 . #x259F)  ; Block Elements
                       (#x25A0 . #x25FF)  ; Geometric Shapes
                       (#x2B00 . #x2BFF))) ; Misc Symbols and Arrows
        (set-fontset-font (frame-parameter nil 'font) range font nil 'prepend))))
  
  ;; Disable ligatures/prettify that might affect character widths
  (when (bound-and-true-p prettify-symbols-mode)
    (prettify-symbols-mode -1))
  (when (fboundp 'ligature-mode)
    (ligature-mode -1))
  
  ;; Standard settings for proper column display
  (setq-local truncate-lines t)
  (setq-local line-move-visual nil)
  (setq-local word-wrap nil)
  (setq-local auto-hscroll-mode 1)
  (setq-local hscroll-margin 5)
  (setq-local hscroll-step 2)
  (tabulated-list-init-header))

;; Subset support
(defvar-local org-queue-chooser--tasks nil
  "If non-nil, this chooser shows these tasks instead of the global queue.")

(defvar-local org-queue-chooser--subset-p nil
  "Non-nil when displaying a subset (does not allow reordering of the global queue).")

(defvar-local org-queue-chooser--marks (make-hash-table :test 'eql)
  "Row marks for bulk operations.")

(defun org-queue-chooser--task-list ()
  "Return the active list of tasks.
If we are in subset mode, return the subset list (even if empty).
Otherwise, return the global outstanding queue."
  (if org-queue-chooser--subset-p
      org-queue-chooser--tasks
    my-outstanding-tasks-list))

(defun org-queue-chooser--asciiize (s)
  "Normalize S so every char is monospace-friendly and has stable width.
  Converts problematic Unicode characters to ASCII equivalents to prevent
  column drift in tabulated-list-mode caused by proportional font fallbacks."
  (let ((res (or s "")))
    ;; Tabs -> space (tabs can have variable width)
    (setq res (replace-regexp-in-string "\t" " " res))
    
    ;; Ellipsis (U+2026) -> three ASCII dots
    (setq res (replace-regexp-in-string "\u2026" "..." res))
    
    ;; Various dashes/minus/hyphen variants -> ASCII hyphen
    ;; U+2010 HYPHEN
    ;; U+2012 FIGURE DASH  
    ;; U+2013 EN DASH
    ;; U+2014 EM DASH
    ;; U+2212 MINUS SIGN (often proportional in fallback)
    (setq res (replace-regexp-in-string "[\u2010\u2012\u2013\u2014\u2212]" "-" res))
    
    ;; Curly/smart quotes -> ASCII quotes
    ;; U+201C LEFT DOUBLE QUOTATION MARK
    ;; U+201D RIGHT DOUBLE QUOTATION MARK
    (setq res (replace-regexp-in-string "[\u201C\u201D]" "\"" res))
    ;; U+2018 LEFT SINGLE QUOTATION MARK
    ;; U+2019 RIGHT SINGLE QUOTATION MARK (also used as apostrophe)
    (setq res (replace-regexp-in-string "[\u2018\u2019]" "'" res))
    
    ;; Various spaces with odd widths -> regular ASCII space
    ;; U+00A0 NO-BREAK SPACE
    ;; U+202F NARROW NO-BREAK SPACE (common in pasted text)
    ;; U+205F MEDIUM MATHEMATICAL SPACE
    ;; U+2000-200A Various width spaces (en quad, em quad, thin space, etc.)
    (setq res (replace-regexp-in-string "[\u00A0\u202F\u205F\u2000-\u200A]" " " res))
    
    ;; Zero-width and joining characters -> remove completely
    ;; U+200B ZERO WIDTH SPACE
    ;; U+200C ZERO WIDTH NON-JOINER
    ;; U+200D ZERO WIDTH JOINER
    ;; U+2060 WORD JOINER (invisible but affects shaping)
    ;; U+FEFF ZERO WIDTH NO-BREAK SPACE (BOM when not at start)
    ;; U+00AD SOFT HYPHEN (invisible unless at line break)
    (setq res (replace-regexp-in-string "[\u200B\u200C\u200D\u2060\uFEFF\u00AD]" "" res))
    
    ;; Additional characters you might want to handle:
    ;; Uncomment if you encounter these in your text:

    ;; Common mathematical symbols that might slip through
    (setq res (replace-regexp-in-string "\u00D7" "x" res))   ; multiplication sign
    (setq res (replace-regexp-in-string "\u00F7" "/" res))   ; division sign
    (setq res (replace-regexp-in-string "\u2260" "!=" res))  ; not equal
    (setq res (replace-regexp-in-string "\u2264" "<=" res))  ; less than or equal
    (setq res (replace-regexp-in-string "\u2265" ">=" res))  ; greater than or equal

    ;; Arrows (often proportional in fallback fonts)
    (setq res (replace-regexp-in-string "\u2190" "<-" res))  ; left arrow
    (setq res (replace-regexp-in-string "\u2192" "->" res))  ; right arrow
    (setq res (replace-regexp-in-string "\u2194" "<->" res)) ; left-right arrow

    ;; Bullet points and dots
    (setq res (replace-regexp-in-string "[\u2022\u2023\u2043]" "*" res))  ; bullets
    (setq res (replace-regexp-in-string "[\u00B7\u2027]" "." res))        ; middle dots
 
    res))

(defun org-queue-chooser-diagnose-row ()
  "Diagnose non-ASCII characters in the current row's Title and Preview columns."
  (interactive)
  (let* ((vec (tabulated-list-get-entry)))
    (when vec
      (let* ((title (aref vec 3))
             (preview (aref vec 4))
             (title-chars (seq-filter (lambda (c) (> c 127)) (string-to-list title)))
             (preview-chars (seq-filter (lambda (c) (> c 127)) (string-to-list preview))))
        (if (or title-chars preview-chars)
            (progn
              (when title-chars
                (message "Title non-ASCII: %S (codepoints: %S)" 
                         (mapconcat (lambda (c) (char-to-string c)) title-chars "")
                         (mapcar (lambda (c) (format "U+%04X" c)) title-chars)))
              (when preview-chars
                (message "Preview non-ASCII: %S (codepoints: %S)"
                         (mapconcat (lambda (c) (char-to-string c)) preview-chars "")
                         (mapcar (lambda (c) (format "U+%04X" c)) preview-chars))))
          (message "No non-ASCII characters found in Title or Preview"))))))

(defun org-queue-chooser--truncate-pad (s width)
  "Truncate S to WIDTH columns with ASCII ellipsis and pad to exactly WIDTH.
This version is deterministic and guarantees exactly WIDTH columns output."
  (let* ((s (or s ""))
         (w (string-width s)))
    (cond
     ;; String fits - pad to exact width
     ((<= w width)
      (concat s (make-string (- width w) ?\s)))
     ;; Width too small for ellipsis - just hard truncate
     ((<= width 3)
      (truncate-string-to-width s width))
     ;; Normal case - truncate and add "..." to exactly fill width
     (t
      (let* ((content-width (- width 3))
             (truncated (truncate-string-to-width s content-width)))
        ;; Ensure we get exactly content-width, then add "..."
        (concat (if (< (string-width truncated) content-width)
                    (concat truncated (make-string (- content-width (string-width truncated)) ?\s))
                  truncated)
                "..."))))))

(defun org-queue-chooser--rjust (s width)
  "Right-justify S into WIDTH using spaces."
  (let* ((s (or s ""))
         (w (string-width s)))
    (if (>= w width) s
      (concat (make-string (- width w) ?\s) s))))

(defun org-queue-chooser--org-to-plain (s)
  "Convert a small Org snippet S to plain text."
  (setq s (or s ""))
  (if (and (fboundp 'org-export-string-as)
           (boundp 'org-export-backends))
      (condition-case _
          (let ((out (org-export-string-as
                      s 'ascii t
                      '(:with-planning nil
                        :with-priority nil
                        :with-tags nil
                        :with-todo-keywords nil
                        :with-statistics-cookies nil
                        :with-footnotes nil
                        :with-properties nil))))
            (string-trim (replace-regexp-in-string "[ \t\n\r]+" " " out)))
        (error
         (let ((txt s))
           (setq txt (replace-regexp-in-string "\\[\\[\\([^]]+\\)\\]\\[\\([^]]+\\)\\]\\]" "\\2" txt))
           (setq txt (replace-regexp-in-string "\\[\\[\\([^]]+\\)\\]\\]" "\\1" txt))
           (setq txt (replace-regexp-in-string "[*/_=~+]\\([^*/_=~+]+\\)[*/_=~+]" "\\1" txt))
           (string-trim (replace-regexp-in-string "[ \t\n\r]+" " " txt)))))
    (let ((txt s))
      (setq txt (replace-regexp-in-string "\\[\\[\\([^]]+\\)\\]\\[\\([^]]+\\)\\]\\]" "\\2" txt))
      (setq txt (replace-regexp-in-string "\\[\\[\\([^]]+\\)\\]\\]" "\\1" txt))
      (setq txt (replace-regexp-in-string "[*/_=~+]\\([^*/_=~+]+\\)[*/_=~+]" "\\1" txt))
      (string-trim (replace-regexp-in-string "[ \t\n\r]+" " " txt)))))

(defun org-queue-chooser--heading-as-plain (s)
  "Plain heading text; strip literal numeric [#N] cookies if present."
  (let ((txt (org-queue-chooser--org-to-plain (or s ""))))
    (setq txt (replace-regexp-in-string "\\[#[0-9]+\\]" "" txt))
    (string-trim txt)))

(defun org-queue-chooser--gather-preview (task)
  "One-line preview for TASK body only (no children/meta)."
  (let* ((m (my-extract-marker task))
         (buf (and (markerp m) (marker-buffer m))))
    (when (and (markerp m) buf (buffer-live-p buf))
      (with-current-buffer buf
        (save-excursion
          (goto-char (marker-position m))
          (org-back-to-heading t)
          (let* ((start (progn (forward-line 1) (point)))
                 (start (progn
                          (when (fboundp 'org-end-of-meta-data)
                            (goto-char start)
                            (org-end-of-meta-data t))
                          (point)))
                 (end (save-excursion
                        (or (and (outline-next-heading) (point)) (point-max)))))
            (save-restriction
              (narrow-to-region start end)
              (goto-char (point-min))
              (while (re-search-forward "^[ \t]*#\\+.*$" nil t)
                (replace-match "" t t))
              (goto-char (point-min))
              (let* ((raw (buffer-substring-no-properties
                           (point-min)
                           (min (point-max) (+ (point-min) 400))))
                     (plain (org-queue-chooser--org-to-plain raw)))
                (string-trim (replace-regexp-in-string "[ \t\n\r]+" " " plain))))))))))

(defun org-queue-chooser--scheduled-string (task)
  "Return YYYY-MM-DD or empty."
  (let* ((m (my-extract-marker task))
         (buf (and (markerp m) (marker-buffer m))))
    (when (and (markerp m) buf (buffer-live-p buf))
      (with-current-buffer buf
        (save-excursion
          (goto-char (marker-position m))
          (let ((tm (org-get-scheduled-time nil)))
            (if tm (format-time-string "%Y-%m-%d" tm) "")))))))

(defun org-queue-chooser--current-index ()
  (tabulated-list-get-id))

(defun org-queue-chooser--goto-next-row ()
  (let ((cur (org-queue-chooser--current-index)))
    (forward-line 1)
    (while (and (not (eobp))
                (equal (org-queue-chooser--current-index) cur))
      (forward-line 1))
    (when (and (eobp) (not (org-queue-chooser--current-index)))
      (forward-line -1))))

(defun org-queue-chooser--goto-previous-row ()
  (let ((cur (org-queue-chooser--current-index)))
    (forward-line -1)
    (while (and (not (bobp))
                (or (not (org-queue-chooser--current-index))
                    (equal (org-queue-chooser--current-index) cur)))
      (forward-line -1))
    (unless (org-queue-chooser--current-index)
      (goto-char (point-min))
      (forward-line 1))))

(defun org-queue-chooser-next-row () (interactive) (org-queue-chooser--goto-next-row))
(defun org-queue-chooser-previous-row () (interactive) (org-queue-chooser--goto-previous-row))

(defun org-queue-chooser--entries ()
  "Build `tabulated-list-entries' from the active task list (global or subset)."
  (let ((entries nil)
        (lst (org-queue-chooser--task-list))
        (len (length (org-queue-chooser--task-list))))
    (dotimes (i len)
      (let* ((task (nth i lst))
             (m (my-extract-marker task))
             (buf (and (markerp m) (marker-buffer m)))
             (title-raw (with-current-buffer (or buf (current-buffer))
                          (save-excursion
                            (when (markerp m)
                              (goto-char (marker-position m))
                              (org-get-heading t t t t)))))
             ;; ASCII-sanitize all text fields
             (title (org-queue-chooser--asciiize
                     (org-queue-chooser--heading-as-plain title-raw)))
             (file (plist-get task :file))
             (file-short (org-queue-chooser--asciiize
                          (and file (file-name-nondirectory file))))
             (preview (org-queue-chooser--asciiize
                       (or (org-queue-chooser--gather-preview task) "")))
             (prio (or (plist-get task :priority)
                       (and (markerp m)
                            (org-with-point-at m
                              (my-get-raw-priority-value)))))
             (sched (org-queue-chooser--scheduled-string task))
             (marked (gethash i org-queue-chooser--marks))
             ;; Fixed-width columns
             (mark-col (org-queue-chooser--truncate-pad (if marked "*" " ") org-queue-chooser-mark-width))
             (idx-str   (org-queue-chooser--rjust (format "%d" (1+ i)) org-queue-chooser-index-width))
             (prio-str  (org-queue-chooser--rjust (number-to-string (or prio 0)) org-queue-chooser-priority-width))
             (title-col (org-queue-chooser--truncate-pad (if (and title (> (length title) 0)) title "(untitled)") org-queue-chooser-title-width))
             (preview-col (org-queue-chooser--truncate-pad preview org-queue-chooser-preview-width))
             (sched-col   (org-queue-chooser--truncate-pad (or sched "") org-queue-chooser-sched-width))
             (file-col    (org-queue-chooser--truncate-pad file-short org-queue-chooser-file-width))
             (vec (vector
                   (propertize mark-col  'face 'org-queue-chooser-mark-face)
                   (propertize idx-str   'face 'org-queue-chooser-index-face)
                   (propertize prio-str  'face 'org-queue-chooser-priority-face)
                   (propertize title-col 'face 'org-queue-chooser-title-face)
                   (propertize preview-col 'face 'org-queue-chooser-preview-face)
                   (propertize sched-col 'face 'org-queue-chooser-sched-face)
                   (propertize file-col  'face 'org-queue-chooser-file-face))))
        (push (list i vec) entries)))
    (nreverse entries)))

(defun org-queue-chooser-refresh ()
  "Refresh the chooser buffer."
  (interactive)
  (let ((inhibit-read-only t))
    (setq tabulated-list-entries (org-queue-chooser--entries))
    (tabulated-list-print t)))

(defun org-queue-chooser--goto-index (idx)
  "Place point on the row with index IDX (relative to current task list)."
  (let* ((lst (org-queue-chooser--task-list))
         (len (length lst)))
    (when (and (numberp idx) (>= idx 0) (< idx len))
      (goto-char (point-min))
      (forward-line) ;; header
      (let ((done nil))
        (while (and (not done) (not (eobp)))
          (let ((row-id (tabulated-list-get-id)))
            (if (and (numberp row-id) (= row-id idx))
                (setq done t)
              (forward-line 1))))))))

(defun org-queue-open-chooser ()
  "Open the queue chooser buffer showing the global queue."
  (interactive)
  (my-ensure-synchronized-task-list)
  (let ((buf (get-buffer-create org-queue-chooser-buffer-name)))
    (pop-to-buffer buf)
    (org-queue-chooser-mode)
    (setq org-queue-chooser--tasks nil
          org-queue-chooser--subset-p nil
          org-queue-chooser--marks (make-hash-table :test 'eql))
    (org-queue-chooser-refresh)
    (org-queue-chooser--goto-index my-outstanding-tasks-index)))

(defun org-queue-chooser-open-in-tab ()
  "Open the queue chooser in a dedicated tab-bar tab named \"Queue\"."
  (interactive)
  (if (not (fboundp 'tab-bar-new-tab))
      (progn (message "tab-bar-mode not available in this Emacs") (org-queue-open-chooser))
    (tab-bar-mode 1)
    (let ((tab-name "Queue"))
      (condition-case _
          (tab-bar-select-tab-by-name tab-name)
        (error
         (tab-bar-new-tab)
         (tab-bar-rename-tab tab-name)))
      (let ((buf (get-buffer-create org-queue-chooser-buffer-name)))
        (switch-to-buffer buf)
        (org-queue-chooser-mode)
        (setq org-queue-chooser--tasks nil
              org-queue-chooser--subset-p nil
              org-queue-chooser--marks (make-hash-table :test 'eql))
        (org-queue-chooser-refresh)
        (org-queue-chooser--goto-index my-outstanding-tasks-index)))))

(defun org-queue-chooser-visit ()
  "Visit the task on the current row. In global mode, update my-outstanding-tasks-index."
  (interactive)
  (let* ((id (tabulated-list-get-id))
         (lst (org-queue-chooser--task-list)))
    (if (not (and (numberp id) (< id (length lst))))
        (message "No task on this row")
      (let* ((task (nth id lst)))
        (when (and (not org-queue-chooser--subset-p)
                   (eq lst my-outstanding-tasks-list))
          (setq my-outstanding-tasks-index id)
          (my-save-index-to-file))
        (my-display-task-at-marker task)
        (my-pulse-highlight-current-line)))))

(defun org-queue-chooser-visit-other-window ()
  "Visit the task on the current row in other window (keep chooser selected)."
  (interactive)
  (let* ((id (tabulated-list-get-id))
         (lst (org-queue-chooser--task-list)))
    (if (not (and (numberp id) (< id (length lst))))
        (message "No task on this row")
      (let* ((task (nth id lst))
             (m (my-extract-marker task))
             (buf (and (markerp m) (marker-buffer m)))
             (pos (and (markerp m) (marker-position m))))
        (unless (and buf pos)
          (user-error "Cannot display this task"))
        (let ((win (display-buffer buf '((display-buffer-pop-up-window)
                                         (inhibit-same-window . t)))))
          (when (window-live-p win)
            (with-selected-window win
              (goto-char pos)
              (when (derived-mode-p 'org-mode)
                (org-back-to-heading t)
                (org-narrow-to-subtree)
                (org-overview)
                (org-reveal t)
                (org-show-entry)
                (org-show-children))
              (recenter))))))))

(defun org-queue-chooser--tab-name-for-index (idx)
  "Return a meaningful tab name for queue index IDX."
  (let* ((lst (org-queue-chooser--task-list))
         (task (and (numberp idx) (nth idx lst)))
         (m (and task (my-extract-marker task)))
         (buf (and (markerp m) (marker-buffer m)))
         (title-raw (when (and (markerp m) buf (buffer-live-p buf))
                      (with-current-buffer buf
                        (save-excursion
                          (goto-char (marker-position m))
                          (org-get-heading t t t t)))))
         (title (org-queue-chooser--asciiize
                 (org-queue-chooser--heading-as-plain title-raw)))
         (file (and task (plist-get task :file)))
         (base (cond ((and title (> (length title) 0)) title)
                     (file (file-name-nondirectory file))
                     (t "Untitled"))))
    (format "#%d %s" (1+ idx)
            (if (> (length base) 40)
                (concat (substring base 0 39) "...")  ; ASCII dots
              base))))

(defun org-queue-chooser-visit-in-new-tab ()
  "Open current row task in a new tab, then return to chooser (background)."
  (interactive)
  (let* ((idx (tabulated-list-get-id))
         (lst (org-queue-chooser--task-list)))
    (if (not (and (numberp idx) (< idx (length lst))))
        (message "No task on this row")
      (if (fboundp 'tab-bar-new-tab)
          (progn
            (tab-bar-mode 1)
            (tab-bar-new-tab)
            (tab-bar-rename-tab (org-queue-chooser--tab-name-for-index idx))
            (let ((task (nth idx lst)))
              (my-display-task-at-marker task))
            (ignore-errors (tab-bar-switch-to-prev-tab)))
        (org-queue-chooser-visit)))))

(defun org-queue-chooser-visit-in-new-tab-foreground ()
  "Open current row task in a new tab and select it (foreground)."
  (interactive)
  (let* ((idx (tabulated-list-get-id))
         (lst (org-queue-chooser--task-list)))
    (if (not (and (numberp idx) (< idx (length lst))))
        (message "No task on this row")
      (if (fboundp 'tab-bar-new-tab)
          (progn
            (tab-bar-mode 1)
            (tab-bar-new-tab)
            (tab-bar-rename-tab (org-queue-chooser--tab-name-for-index idx))
            (let ((task (nth idx lst)))
              (my-display-task-at-marker task)))
        (org-queue-chooser-visit)))))

;; Reorder global queue only (not allowed in subset)
(defun org-queue-chooser--require-global ()
  (when org-queue-chooser--subset-p
    (user-error "Reordering is only available for the global queue")))

(defun org-queue-chooser--move-element (list from to)
  (let* ((len (length list))
         (from (max 0 (min (1- len) from)))
         (to   (max 0 (min (1- len) to))))
    (if (= from to)
        list
      (let* ((elem (nth from list))
             (rest (append (seq-take list from) (seq-drop list (1+ from)))))
        (append (seq-take rest to) (list elem) (seq-drop rest to))))))

(defun org-queue-chooser--adjust-index-after-move (old-idx from to)
  (cond
   ((= old-idx from) to)
   ((and (> from old-idx) (>= to old-idx)) old-idx)
   ((and (< from old-idx) (<= to old-idx)) (1+ old-idx))
   ((and (> from old-idx) (< to old-idx)) (1- old-idx))
   (t old-idx)))

(defun org-queue-chooser-move-up ()
  (interactive)
  (org-queue-chooser--require-global)
  (let* ((from (tabulated-list-get-id)))
    (unless (and (numberp from) (> from 0))
      (user-error "Cannot move further up"))
    (let* ((to (1- from))
           (old-list my-outstanding-tasks-list))
      (setq my-outstanding-tasks-list (org-queue-chooser--move-element old-list from to))
      (setq my-outstanding-tasks-index (org-queue-chooser--adjust-index-after-move
                                        my-outstanding-tasks-index from to))
      (my-save-outstanding-tasks-to-file)
      (my-queue-limit-visible-buffers)
      (org-queue-chooser-refresh)
      (org-queue-chooser--goto-index to)
      (message "Moved to position %d" (1+ to)))))

(defun org-queue-chooser-move-down ()
  (interactive)
  (org-queue-chooser--require-global)
  (let* ((from (tabulated-list-get-id)))
    (unless (and (numberp from) (< from (1- (length my-outstanding-tasks-list))))
      (user-error "Cannot move further down"))
    (let* ((to (1+ from))
           (old-list my-outstanding-tasks-list))
      (setq my-outstanding-tasks-list (org-queue-chooser--move-element old-list from to))
      (setq my-outstanding-tasks-index (org-queue-chooser--adjust-index-after-move
                                        my-outstanding-tasks-index from to))
      (my-save-outstanding-tasks-to-file)
      (my-queue-limit-visible-buffers)
      (org-queue-chooser-refresh)
      (org-queue-chooser--goto-index to)
      (message "Moved to position %d" (1+ to)))))

(defun org-queue-chooser-move-to-position (pos)
  (interactive "nMove to position (1-based): ")
  (org-queue-chooser--require-global)
  (let* ((from (tabulated-list-get-id))
         (len (length my-outstanding-tasks-list))
         (to (1- (max 1 (min len pos)))))
    (unless (numberp from)
      (user-error "No task selected"))
    (when (= from to)
      (message "Already at that position")
      (cl-return-from org-queue-chooser-move-to-position))
    (let ((old-list my-outstanding-tasks-list))
      (setq my-outstanding-tasks-list (org-queue-chooser--move-element old-list from to))
      (setq my-outstanding-tasks-index (org-queue-chooser--adjust-index-after-move
                                        my-outstanding-tasks-index from to))
      (my-save-outstanding-tasks-to-file)
      (my-queue-limit-visible-buffers)
      (org-queue-chooser-refresh)
      (org-queue-chooser--goto-index to)
      (message "Moved to position %d" (1+ to)))))

;; Mark helpers
(defun org-queue-chooser--make-row (i)
  "Return a single tabulated-list entry (ID . VECTOR) for row I."
  (let* ((lst (org-queue-chooser--task-list))
         (task (nth i lst))
         (m (my-extract-marker task))
         (buf (and (markerp m) (marker-buffer m)))
         (title-raw (with-current-buffer (or buf (current-buffer))
                      (save-excursion
                        (when (markerp m)
                          (goto-char (marker-position m))
                          (org-get-heading t t t t)))))
         ;; ASCII-sanitize all text fields
         (title (org-queue-chooser--asciiize
                 (org-queue-chooser--heading-as-plain title-raw)))
         (file (plist-get task :file))
         (file-short (org-queue-chooser--asciiize
                      (and file (file-name-nondirectory file))))
         (preview (org-queue-chooser--asciiize
                   (or (org-queue-chooser--gather-preview task) "")))
         (prio (or (plist-get task :priority)
                   (and (markerp m)
                        (org-with-point-at m
                          (string-to-number
                           (or (org-entry-get nil "PRIORITY")
                               (number-to-string org-priority-default)))))))
         (sched (org-queue-chooser--scheduled-string task))
         (marked (gethash i org-queue-chooser--marks))
         ;; Fixed-width columns
         (mark-col (org-queue-chooser--truncate-pad (if marked "*" " ") org-queue-chooser-mark-width))
         (idx-str   (org-queue-chooser--rjust (format "%d" (1+ i)) org-queue-chooser-index-width))
         (prio-str  (org-queue-chooser--rjust (number-to-string (or prio 0)) org-queue-chooser-priority-width))
         (title-col (org-queue-chooser--truncate-pad (if (and title (> (length title) 0)) title "(untitled)") org-queue-chooser-title-width))
         (preview-col (org-queue-chooser--truncate-pad preview org-queue-chooser-preview-width))
         (sched-col   (org-queue-chooser--truncate-pad (or sched "") org-queue-chooser-sched-width))
         (file-col    (org-queue-chooser--truncate-pad file-short org-queue-chooser-file-width))
         (vec (vector
               (propertize mark-col  'face 'org-queue-chooser-mark-face)
               (propertize idx-str   'face 'org-queue-chooser-index-face)
               (propertize prio-str  'face 'org-queue-chooser-priority-face)
               (propertize title-col 'face 'org-queue-chooser-title-face)
               (propertize preview-col 'face 'org-queue-chooser-preview-face)
               (propertize sched-col 'face 'org-queue-chooser-sched-face)
               (propertize file-col  'face 'org-queue-chooser-file-face))))
    (list i vec)))

(defun org-queue-chooser--reprint-row (i)
  "Reprint row I at its current position preserving tabulated-list properties."
  (let ((inhibit-read-only t))
    (save-excursion
      (org-queue-chooser--goto-index i)
      (let* ((bol (line-beginning-position))
             (eol (line-end-position))
             (row (org-queue-chooser--make-row i))
             (id  (car row))
             (vec (cadr row)))
        ;; Replace the entire line (including newline) with a freshly printed row
        (delete-region bol (min (1+ eol) (point-max)))
        (tabulated-list-print-entry id vec)))))

(defun org-queue-chooser-mark () (interactive)
  (let ((id (tabulated-list-get-id)))
    (when (numberp id)
      (puthash id t org-queue-chooser--marks)
      (org-queue-chooser--reprint-row id))))

(defun org-queue-chooser-unmark () (interactive)
  (let ((id (tabulated-list-get-id)))
    (when (and (numberp id) (gethash id org-queue-chooser--marks))
      (remhash id org-queue-chooser--marks)
      (org-queue-chooser--reprint-row id))))

(defun org-queue-chooser-toggle-mark () (interactive)
  (let ((id (tabulated-list-get-id)))
    (when (numberp id)
      (if (gethash id org-queue-chooser--marks)
          (progn (remhash id org-queue-chooser--marks)
                 (org-queue-chooser--reprint-row id))
        (puthash id t org-queue-chooser--marks)
        (org-queue-chooser--reprint-row id)))))

(defun org-queue-chooser-unmark-all () (interactive)
  (setq org-queue-chooser--marks (make-hash-table :test 'eql))
  ;; One full refresh is fine for clearing everything
  (org-queue-chooser-refresh))

(defun org-queue-chooser-mark-region () (interactive)
  (if (not (use-region-p))
      (message "No active region")
    (let ((beg (region-beginning))
          (end (region-end))
          (ids '()))
      (save-excursion
        (goto-char beg)
        (forward-line (if (save-excursion (goto-char (point-min)) (forward-line 1) (<= beg (point))) 1 0))
        (while (and (<= (point) end) (not (eobp)))
          (let ((id (tabulated-list-get-id)))
            (when (numberp id) (push id ids)))
          (forward-line 1)))
      (setq ids (sort (delete-dups ids) #'<))
      (deactivate-mark)
      ;; Mark and reprint each affected row (faster than full refresh, preserves properties)
      (dolist (id ids)
        (puthash id t org-queue-chooser--marks)
        (org-queue-chooser--reprint-row id)))))

(defun org-queue-chooser--selected-indices ()
  "Return selection as a list of row indices:
- If any marks exist, use them;
- Else if an active region covers rows, use those;
- Else use the current row."
  (let ((ids nil))
    ;; 1) Marks (fast path)
    (maphash (lambda (k v) (when v (push k ids))) org-queue-chooser--marks)
    (setq ids (sort ids #'<))
    ;; 2) Region rows if no marks
    (when (null ids)
      (when (use-region-p)
        (let ((beg (region-beginning))
              (end (region-end)))
          (save-excursion
            (goto-char beg)
            ;; Skip theheader row
            (forward-line (if (save-excursion (goto-char (point-min)) (forward-line 1) (<= beg (point))) 1 0))
            (while (and (<= (point) end) (not (eobp)))
              (let ((id (tabulated-list-get-id)))
                (when (numberp id) (push id ids)))
              (forward-line 1)))
          (setq ids (sort (delete-dups ids) #'<)))))
    ;; 3) Fall back to current row
    (when (null ids)
      (let ((id (tabulated-list-get-id)))
        (when (numberp id) (setq ids (list id)))))
    ids))

;; Bulk operations: schedule spread, priority spread, advance, postpone, add-to-outstanding
(defun org-queue-chooser--apply-to-indices (indices fn)
  (let ((lst (org-queue-chooser--task-list)))
    (dolist (id indices)
      (let ((task (nth id lst)))
        (when task (funcall fn task))))))

(defun org-queue-chooser--task-schedule-set (task date-string)
  (let ((m (my-extract-marker task)))
    (when (and (markerp m) (marker-buffer m))
      (org-with-point-at m
        (org-schedule nil date-string)))))

(defun org-queue-chooser-spread-schedule (&optional items-per-day)
  "Reschedule selection evenly across a period.
Prompts:
- Start date (default: tomorrow)
- Number of days (default: 30)
- Items per day (default: ceil(N/Days), unless prefix argument given)

Selection:
- Marks if any; else rows in active region; else the current row."
  (interactive "P")
  (let* ((ids (org-queue-chooser--selected-indices))
         (n (length ids)))
    (unless (> n 0) (user-error "Nothing selected"))
    (let* ((start (org-read-date nil nil nil "Start date (default: tomorrow)"))
           (start-time (if (string-empty-p start)
                           (time-add (current-time) (days-to-time 1))
                         (org-time-string-to-time start)))
           (days (max 1 (read-number "Number of days: " 30)))
           (per-day (cond
                     (current-prefix-arg (max 1 (prefix-numeric-value current-prefix-arg)))
                     (items-per-day (max 1 items-per-day))
                     (t (max 1 (ceiling (/ (float n) days)))))))
      (cl-loop
       for k from 0
       for id in ids
       do (let* ((day-offset (/ k per-day))
                 (target (time-add start-time (days-to-time day-offset)))
                 (ds (format-time-string "%Y-%m-%d" target))
                 (task (nth id (org-queue-chooser--task-list))))
            (org-queue-chooser--task-schedule-set task ds)))
      (org-queue-chooser-refresh)
      (message "Rescheduled %d item(s) across %d day(s), %d per day"
               n days per-day))))

(defun org-queue-chooser-spread-priorities ()
  "Spread numeric priorities over the selection (1=highest .. 64=lowest).
Selection is: marked rows if any; else rows in active region; else the current row."
  (interactive)
  (let* ((ids (org-queue-chooser--selected-indices)))
    (unless (> (length ids) 0) (user-error "Nothing selected"))
    (let* ((lst (org-queue-chooser--task-list))
           (pairs (mapcar (lambda (id) (cons id (nth id lst))) ids))
           (pr-top (or (and (numberp org-priority-highest) org-priority-highest) 1))
           (pr-bottom (or (and (numberp org-priority-lowest) org-priority-lowest) 64))
           (lo (read-number (format "Start priority (%d = highest): " pr-top) pr-top))
           (hi (read-number (format "End priority (%d = lowest): " pr-bottom) pr-bottom)))
      (when (> lo hi) (cl-rotatef lo hi))
      (let* ((n (length pairs))
             (span (- hi lo))
             (targets (if (= n 1)
                          (list (round (/ (+ lo hi) 2.0)))
                        (cl-loop for i from 0 to (1- n)
                                 for q = (/ (float i) (float (1- n)))
                                 for v = (round (+ lo (* q span)))
                                 collect v))))
        (cl-loop for (id . task) in pairs
                 for newp in targets
                 do (let ((m (my-extract-marker task)))
                      (when (and (markerp m) (marker-buffer m))
                        (org-with-point-at m
                          (org-entry-put nil "PRIORITY" (number-to-string newp)))
                        (plist-put task :priority newp)
                        (plist-put task :flag (my-priority-flag newp)))))
        (org-queue-chooser-refresh)
        (message "Spread priorities %d..%d over %d items" lo hi n)))))

(defun org-queue-chooser-advance-marked ()
  "Advance schedule for selection using my-advance-schedule."
  (interactive)
  (let* ((ids (org-queue-chooser--selected-indices))
         (lst (org-queue-chooser--task-list))
         (count 0))
    (dolist (id ids)
      (let* ((task (nth id lst))
             (m (my-extract-marker task)))
        (when (and (markerp m) (marker-buffer m))
          (org-with-point-at m
            (my-advance-schedule))
          (setq count (1+ count)))))
    (org-queue-chooser-refresh)
    (message "Advanced %d item(s)" count)))

(defun org-queue-chooser-postpone-marked ()
  "Postpone schedule for selection using my-postpone-schedule."
  (interactive)
  (let* ((ids (org-queue-chooser--selected-indices))
         (lst (org-queue-chooser--task-list))
         (count 0))
    (dolist (id ids)
      (let* ((task (nth id lst))
             (m (my-extract-marker task)))
        (when (and (markerp m) (marker-buffer m))
          (org-with-point-at m
            (my-postpone-schedule))
          (setq count (1+ count)))))
    (org-queue-chooser-refresh)
    (message "Postponed %d item(s)" count)))

(defun org-queue-chooser-add-to-outstanding (&optional remove)
  "Add selection to outstanding without rescheduling (SuperMemo-style).
Sets QFORCE and slightly increases priority (toward [#1]).
With prefix arg REMOVE, remove QFORCE instead.
Rebuilds and saves the global queue afterwards."
  (interactive "P")
  (let* ((ids (org-queue-chooser--selected-indices))
         (lst (org-queue-chooser--task-list))
         (count 0))
    (dolist (id ids)
      (let* ((task (nth id lst))
             (m (my-extract-marker task)))
        (when (and (markerp m) (marker-buffer m))
          (org-with-point-at m
            ;; Ensure ID exists so dedupe is reliable
            (org-id-get-create)
            (if remove
                (org-entry-delete nil org-queue-force-outstanding-property)
              (org-entry-put nil org-queue-force-outstanding-property "1")
              ;; Read priority (property or [#N]), then nudge toward 1
              (let* ((cur (or (my-read-numeric-priority-here)
                              (and (numberp org-priority-default) org-priority-default)
                              32))
                     (new (max org-priority-highest (1- cur))))
                (my-set-numeric-priority-here new))))
          (setq count (1+ count)))))
    ;; Rebuild global queue now
    (my-get-outstanding-tasks)
    (my-save-outstanding-tasks-to-file)
    ;; Refresh whichever chooser is visible
    (when (get-buffer "*Org Queue*")
      (with-current-buffer "*Org Queue*"
        (when (derived-mode-p 'org-queue-chooser-mode)
          (org-queue-chooser-refresh))))
    (when (get-buffer "*Org Queue (Subset)*")
      (with-current-buffer "*Org Queue (Subset)*"
        (when (derived-mode-p 'org-queue-chooser-mode)
          (org-queue-chooser-refresh))))
    (message "%s force-outstanding on %d item(s)"
             (if remove "Removed" "Set") count)))

;; Subset chooser from current buffer (visible region respected)
(defun org-queue-open-chooser-from-buffer (&optional outstanding-only)
  "Build a chooser from the current Org buffer.

Selection scope:
- If a region is active: only entries inside the region are used.
- Else if the buffer is narrowed: only the narrowed portion is used.
- Else: the entire current buffer is used.

If OUTSTANDING-ONLY (prefix arg) is non-nil, include only outstanding tasks.
This does not alter the global queue."
  (interactive "P")
  (unless (derived-mode-p 'org-mode)
    (user-error "Not in org-mode buffer"))

  (save-excursion
    (save-restriction
      ;; If the user selected a region, narrow to it. Otherwise respect any
      ;; existing narrowing (we do NOT widen).
      (when (use-region-p)
        (narrow-to-region (region-beginning) (region-end)))

      (let ((tasks '()))
        (goto-char (point-min))
        ;; Scope = nil so org-map-entries respects the current restriction
        ;; (region-narrowing above, or any pre-existing narrowing).
        (org-map-entries
         (lambda ()
           (when (or (not outstanding-only)
                     (my-is-outstanding-task))
             (let* ((marker   (point-marker))
                    (id       (org-entry-get nil "ID"))
                    (priority (my-get-raw-priority-value))
                    (flag     (my-priority-flag priority))
                    (file     (buffer-file-name))
                    (is-todo  (my-is-todo-task))
                    (task     (list :id id :marker marker :priority priority
                                    :flag flag :file file :is-todo is-todo)))
               (push task tasks))))
         nil  ;; matcher
         nil) ;; scope = nil => honors narrowing

        (setq tasks (nreverse tasks))
        (let ((buf (get-buffer-create "*Org Queue (Subset)*")))
          (pop-to-buffer buf)
          (org-queue-chooser-mode)
          (setq org-queue-chooser--tasks tasks
                org-queue-chooser--subset-p t
                org-queue-chooser--marks (make-hash-table :test 'eql))
          (org-queue-chooser-refresh)
          (message "Subset: %d task(s)%s"
                   (length tasks)
                   (if outstanding-only " (outstanding only)" "")))))))

(provide 'org-queue-display)
;;; org-queue-display.el ends here
