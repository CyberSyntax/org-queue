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
Uses the in-memory queue; no rebuild/resync."
  (interactive)
  (my-ensure-task-list-present)
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

(defun my-queue-register-visit (_buf)
  "No-op. Keep native C-x b behavior."
  nil)

(defvar-local org-queue-srs--conceal-overlays nil)

(defun org-queue-srs--clear-conceal ()
  (while org-queue-srs--conceal-overlays
    (delete-overlay (pop org-queue-srs--conceal-overlays))))

(defun org-queue-srs--hide-region (beg end)
  "Hide [BEG,END) using Org’s folding so TAB/S-TAB reveal it."
  (org-flag-region beg end t 'outline))

;; Options
(defcustom org-queue-srs-back-heading-regexp "\\`\\(Back\\|Answer\\)\\'"
  "Child headings treated as answers."
  :type 'string :group 'org-queue)

;; Exact region helpers
(defun org-queue--subtree-end ()
  (save-excursion (org-end-of-subtree t t) (point)))

(defun org-queue--meta-end ()
  (save-excursion
    (org-back-to-heading t)
    (forward-line 1)
    (when (fboundp 'org-end-of-meta-data)
      (org-end-of-meta-data t))
    (point)))

(defun org-queue--first-child-pos ()
  (save-excursion
    (org-back-to-heading t)
    (let* ((lvl (org-current-level)) (lim (org-queue--subtree-end)))
      (goto-char (org-queue--meta-end))
      (catch 'found
        (while (and (outline-next-heading) (< (point) lim))
          (when (> (org-current-level) lvl)
            (throw 'found (line-beginning-position))))
        nil))))

;; Hide Back subtree(s) exactly
(defun org-queue-srs--conceal-back ()
  "Hide Back/Answer child bodies; keep their heading line visible. Return count."
  (save-excursion
    (org-back-to-heading t)
    (let* ((lim (save-excursion (org-end-of-subtree t t) (point)))
           (lvl (org-current-level))
           (count 0))
      (goto-char (org-queue--meta-end))
      (while (and (outline-next-heading) (< (point) lim))
        (when (> (org-current-level) lvl)
          (let* ((title (org-get-heading t t t t))
                 (name  (string-trim (car (split-string title "[ \t:]")))))
            (when (string-match-p org-queue-srs-back-heading-regexp name)
              (setq count (1+ count))
              (let* ((body-beg (save-excursion
                                 (forward-line 1)
                                 (when (fboundp 'org-end-of-meta-data)
                                   (org-end-of-meta-data t))
                                 (point)))
                     (body-end (save-excursion
                                 (or (and (outline-next-heading) (point))
                                     lim))))
                (when (< body-beg body-end)
                  (org-queue-srs--hide-region body-beg body-end)))))))
      count)))

;; Hide body-as-answer when there is no Back child:
(defun org-queue-srs--conceal-entry-body-if-no-back ()
  "Hide body [B,E) exactly when this entry has no Back child."
  (save-excursion
    (org-back-to-heading t)
    (let* ((lim   (save-excursion (org-end-of-subtree t t) (point)))
           (b0    (org-queue--meta-end))
           (first (save-excursion
                    (let ((lvl (org-current-level)))
                      (goto-char (org-queue--meta-end))
                      (catch 'found
                        (while (and (outline-next-heading) (< (point) lim))
                          (when (> (org-current-level) lvl)
                            (throw 'found (line-beginning-position))))
                        nil))))
           (e     (or first lim)))
      (when (< b0 e)
        (org-queue-srs--hide-region b0 e)))))

(defun org-queue-srs--show-front-subtree ()
  "If a child named Front exists, show its subtree (body visible)."
  (save-excursion
    (org-back-to-heading t)
    (let* ((lim (save-excursion (org-end-of-subtree t t) (point)))
           (lvl (org-current-level))
           (rx  (format "^\\*\\{%d,\\}\\s-+Front\\b" (1+ (or lvl 0)))))
      (goto-char (org-queue--meta-end))
      (when (re-search-forward rx lim t)
        (goto-char (match-beginning 0))
        (org-show-subtree)))))

;; Exact main entry-point
(defun org-queue-srs-maybe-conceal-here ()
  "Conceal only the answer for SRS entries; do nothing else."
  (org-queue-srs--clear-conceal)
  (let ((where (org-srs-entry-p (point))))
    (when (and org-queue-srs-conceal-answer (memq where '(current parent)))
      (if (> (org-queue-srs--conceal-back) 0)
          (org-queue-srs--show-front-subtree)
        (org-queue-srs--conceal-entry-body-if-no-back)))))

(defun my-display-task-at-marker (task-or-marker)
  "Display TASK-OR-MARKER robustly. Prefer existing marker; no forced org-id re-resolve."
  (let* ((marker (my-extract-marker task-or-marker)))
    (unless (and (markerp marker)
                 (marker-buffer marker)
                 (buffer-live-p (marker-buffer marker)))
      (user-error "Cannot resolve task marker"))
    (let ((buf (marker-buffer marker))
          (pos (marker-position marker)))
      (switch-to-buffer buf)
      (widen)
      (goto-char pos)
      (when (derived-mode-p 'org-mode)
        (org-back-to-heading t)
        (org-narrow-to-subtree)
        (org-overview)
        (org-reveal t)
        (org-show-entry)
        (org-show-children))
      (org-queue-srs-maybe-conceal-here)
      (add-hook 'kill-buffer-hook #'org-queue-srs--clear-conceal nil t)
      (recenter))))

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

(defun my--org-entry-body-bounds ()
  (save-excursion
    (org-back-to-heading t)
    (let ((beg (progn
                 (when (fboundp 'org-end-of-meta-data)
                   (org-end-of-meta-data t))
                 (point)))
          (end (progn
                 (outline-next-heading)
                 (point))))
      (cons beg end))))

(defun my--strip-clozes-in-string (s)
  "Return S with all {{clozed:...}} unwrapped to inner text."
  (when s
    (replace-regexp-in-string "{{clozed:\\([^}]*\\)}}" "\\1" s)))

(defun org-interactive-cloze ()
  "Create a cloze deletion.
- On a heading line: child heading title is Front (other clozes unwrapped); Back is the selected text in the child body.
- In body: child has Front/Back subheadings; Front = whole entry body with […] replacing selection and all other clozes unwrapped; Back = selected text.
Other cloze markers remain in the original location only."
  (interactive)
  (if (not (use-region-p))
      (message "Please select text to create a cloze")
    (let* ((start0 (region-beginning))
           (end0   (region-end))
           (selected-text (buffer-substring-no-properties start0 end0))
           (heading-pos (save-excursion (goto-char start0) (org-back-to-heading t) (point)))
           (heading-level (save-excursion (goto-char heading-pos) (org-current-level)))
           (parent-priority-range (save-excursion
                                   (goto-char heading-pos)
                                   (my-get-current-priority-range)))
           (on-heading (save-excursion
                         (goto-char start0) (beginning-of-line) (org-at-heading-p)))
           front-title front-block)
      ;; Build Front BEFORE modifying buffer
      (if on-heading
          (let* ((lb (save-excursion (goto-char start0) (line-beginning-position)))
                 (le (save-excursion (goto-char end0)   (line-end-position)))
                 (prefix (buffer-substring-no-properties lb start0))
                 (suffix (buffer-substring-no-properties end0 le)))
            (setq front-title
                  (my--strip-leading-stars
                   (my--strip-clozes-in-string
                    (concat prefix "[...]" suffix)))))
        (let* ((bounds (my--org-entry-body-bounds))
               (bbeg (car bounds))
               (bend (cdr bounds))
               (s (max start0 bbeg))
               (e (min end0 bend))
               (prefix (buffer-substring-no-properties bbeg s))
               (suffix (buffer-substring-no-properties e bend)))
          (setq front-block
                (concat (my--strip-clozes-in-string prefix)
                        "[...]"
                        (my--strip-clozes-in-string suffix)))))
      ;; Replace selection in-place with {{clozed:...}}
      (delete-region start0 end0)
      (insert (format "{{clozed:%s}}" selected-text))
      ;; Create child entry
      (save-excursion
        (goto-char heading-pos)
        (org-end-of-subtree)
        (let ((new-heading-pos (point)))
          (insert "\n" (make-string (1+ heading-level) ?*) " ")
          (if on-heading
              (progn
                (insert (if (> (length (string-trim (or front-title ""))) 0)
                            front-title
                          "(untitled)"))
                (insert "\n")
                (insert selected-text))
            ;; Body: blank title; Front/Back subheads
            (insert "\n") ; end empty title line
            (insert (make-string (+ 2 heading-level) ?*) " Front\n")
            (when front-block (insert front-block))
            (unless (bolp) (insert "\n")) ; exactly one newline before Back
            (insert (make-string (+ 2 heading-level) ?*) " Back\n")
            (insert selected-text))
          ;; Priority, ID, SRS
          (when parent-priority-range
            (goto-char (1+ new-heading-pos))
            (my-set-priority-with-heuristics parent-priority-range))
          (goto-char (1+ new-heading-pos))
          (org-id-get-create)
          (when (fboundp 'org-srs-item-new)
            (org-srs-item-new 'card))))
      (message "Created cloze deletion"))))

(defun org-interactive-cloze-prefix-only ()
  "Create a cloze whose Front shows only the preceding context.
Examples:
  Text:  \"I am a boy.\"
  Region: \"am\"
  Front:  \"I [...]\"
  Back:   \"am\"

Behavior:
- On a heading line: child heading title becomes the preceding text + [...];
  child body becomes the selected text (Back).
- In the body: child gets Front/Back subheadings; Front is the entry body content
  before the selection + [...]; Back is the selected text.
Other cloze markers are unwrapped on the Front."
  (interactive)
  (if (not (use-region-p))
      (message "Please select text to create a cloze")
    (let* ((start0 (region-beginning))
           (end0   (region-end))
           (selected-text (buffer-substring-no-properties start0 end0))
           (heading-pos (save-excursion (goto-char start0) (org-back-to-heading t) (point)))
           (heading-level (save-excursion (goto-char heading-pos) (org-current-level)))
           (parent-priority-range (save-excursion
                                    (goto-char heading-pos)
                                    (my-get-current-priority-range)))
           (on-heading (save-excursion
                         (goto-char start0) (beginning-of-line) (org-at-heading-p)))
           front-title front-block)

      ;; Build Front BEFORE modifying the buffer
      (if on-heading
          (let* ((lb (save-excursion (goto-char start0) (line-beginning-position)))
                 (prefix (buffer-substring-no-properties lb start0)))
            (setq front-title
                  (my--strip-leading-stars
                   (my--strip-clozes-in-string
                    (concat prefix "[...]")))))
        (let* ((bounds (my--org-entry-body-bounds))
               (bbeg (car bounds))
               (s (max start0 bbeg))
               (prefix (buffer-substring-no-properties bbeg s)))
          (setq front-block
                (concat (my--strip-clozes-in-string prefix)
                        "[...]"))))

      ;; Replace selection in-place with {{clozed:...}}
      (delete-region start0 end0)
      (insert (format "{{clozed:%s}}" selected-text))

      ;; Create child entry
      (save-excursion
        (goto-char heading-pos)
        (org-end-of-subtree)
        (let ((new-heading-pos (point)))
          (insert "\n" (make-string (1+ heading-level) ?*) " ")
          (if on-heading
              (progn
                (insert (if (> (length (string-trim (or front-title ""))) 0)
                            front-title
                          "(untitled)"))
                (insert "\n")
                (insert selected-text))
            ;; Body: blank title; Front/Back subheads
            (insert "\n")
            (insert (make-string (+ 2 heading-level) ?*) " Front\n")
            (when front-block (insert front-block))
            (unless (bolp) (insert "\n"))
            (insert (make-string (+ 2 heading-level) ?*) " Back\n")
            (insert selected-text))
          ;; Priority, ID, SRS
          (when parent-priority-range
            (goto-char (1+ new-heading-pos))
            (my-set-priority-with-heuristics parent-priority-range))
          (goto-char (1+ new-heading-pos))
          (org-id-get-create)
          (when (fboundp 'org-srs-item-new)
            (org-srs-item-new 'card))))
      (message "Created cloze (preceding context only)"))))

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

(provide 'org-queue-display)
;;; org-queue-display.el ends here
