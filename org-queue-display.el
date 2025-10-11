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

;; Display and UI faces
(defface org-clozed-face
  '((t (:background "#E67300" :foreground "black")))
  "Face for clozed text in org-mode.")

(defface org-extract-face
  '((t (:background "#44C2FF" :foreground "black")))
  "Face for extracted text in org-mode.")

(defface org-cloze-face
  '((t (:foreground "red" :background "#FF0" :weight bold :slant normal)))
  "Face for the visible cloze placeholder on the Front.")

(defvar-local org-custom-overlays nil
  "List of overlays for custom syntax highlighting.")

;; ID generator for {{type#ID|...|ID}}
(defconst org-custom-syntax--id-length 12)
(defconst org-custom-syntax--id-charset
  "ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789_-")
(defvar org-custom-syntax--rng-seeded nil)

(defun org-custom-syntax--ensure-rng ()
  (unless org-custom-syntax--rng-seeded
    (random t)
    (setq org-custom-syntax--rng-seeded t)))

(defun org-custom-syntax--make-id ()
  "Generate a short ID."
  (org-custom-syntax--ensure-rng)
  (let* ((alphabet org-custom-syntax--id-charset)
         (alen (length alphabet))
         (len org-custom-syntax--id-length))
    (apply #'string
           (cl-loop repeat len
                    collect (aref alphabet (random alen))))))

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
  "Run or focus Anki; never spawn duplicate instances."
  (interactive)
  (if (org-queue-night-shift-p)
      (progn
        (message "Night shift active: not launching Anki.")
        nil)
    (cond
     ;; Android
     ((and (boundp 'my-android-p) my-android-p)
     (message "Please open the Anki app manually on Android."))
     ;; macOS
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
           (start-process
             "anki-activate" nil "sh" "-c"
             "xdotool search --onlyvisible --class anki | head -n1 | xargs -r -I{} xdotool windowactivate --sync {}"))
           (t
            (message "Anki is running; install wmctrl/xdotool to raise it.")))
        (let ((exe (or (executable-find "anki")
                       (executable-find "anki.bin"))))
          (if exe
              (start-process "Anki" nil exe)
            (message "Anki executable not found on PATH."))))))))

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
  "Render {{type#ID|PAYLOAD|ID}} markers by hiding wrappers, even when nested.
- Opening wrapper '{{type#ID|' and closing wrapper '|ID}}' are invisible.
- Only PAYLOAD is visible; it may itself contain nested markers which are processed recursively.
- The payload is given a face based on TYPE."
  (interactive)
  (when (eq major-mode 'org-mode)
    (org-clear-custom-overlays)
    (cl-labels
        ((scan (beg end)
           (save-excursion
             (goto-char beg)
             (while (search-forward "{{" end t)
               (let ((start (match-beginning 0)))
                 (if (looking-at "\\([A-Za-z0-9_-]+\\)#\\([A-Za-z0-9_-]+\\)|")
                     (let ((type (match-string-no-properties 1))
                           (id   (match-string-no-properties 2)))
                       (goto-char (match-end 0))
                       (let ((content-beg (point)))
                         (when (search-forward (format "|%s}}" id) end t)
                           (let* ((close-end (point))
                                  (content-end (- close-end (+ 3 (length id))))
                                  (open-end content-beg)
                                  (close-start (- close-end (+ 3 (length id)))))
                             ;; Recurse first so nested markers get processed
                             (scan content-beg content-end)
                             ;; Overlays: hide wrappers, style payload
                             (let ((ov-open  (make-overlay start open-end))
                                   (ov-close (make-overlay close-start close-end))
                                   (ov-body  (make-overlay content-beg content-end)))
                               (overlay-put ov-open  'invisible t)
                               (overlay-put ov-close 'invisible t)
                               (overlay-put ov-body 'face
                                            (cond
                                             ((string= type "extract") 'org-extract-face)
                                             ((string= type "clozed")  'org-clozed-face)
                                             ((string= type "cloze")   'org-cloze-face)
                                             (t 'org-extract-face)))
                               (push ov-open  org-custom-overlays)
                               (push ov-close org-custom-overlays)
                               (push ov-body  org-custom-overlays))
                             (goto-char close-end)))))
                   (goto-char (1+ start))))))))
      (scan (point-min) (point-max)))))

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
  "Return S with all {{clozed#ID|...|ID}} unwrapped to inner text."
  (when s
    (with-temp-buffer
      (insert s)
      (goto-char (point-min))
      (while (search-forward "{{clozed#" nil t)
        (let ((start (match-beginning 0)))
          (if (looking-at "\\([A-Za-z0-9_-]+\\)|")
              (let ((id (match-string-no-properties 1)))
                (goto-char (match-end 0))
                (let ((content-beg (point)))
                  (if (search-forward (format "|%s}}" id) nil t)
                      (let ((content (buffer-substring-no-properties
                                      content-beg
                                      (- (point) (+ 3 (length id))))))
                        (delete-region start (point))
                        (goto-char start)
                        (insert content))
                    (goto-char (1+ start)))))
            (goto-char (1+ start)))))
      (buffer-string))))

(defcustom org-interactive-cloze-ellipsis "[...]"
  "String used to indicate the cloze gap on the Front."
  :type 'string
  :group 'org)

(defun org-interactive-cloze (&optional front-context)
  "Create a cloze deletion using {{clozed#ID|SELECTION|ID}} 
  and {{cloze#ID|[...]|ID}} for the Front ellipsis.

Front context options:
- 'both   : prefix + […] + suffix (default)
- 'prefix : prefix + […]
- 'suffix : […] + suffix

Behavior:
- On a heading line: child heading title is the Front; child body is the selected text (Back).
- In body: child has Front/Back subheadings; Front is built from the entry body around the selection; Back is the selected text.
Other cloze markers are unwrapped on the Front only."
  (interactive
   (list (when current-prefix-arg
           (intern (completing-read
                    "Front context: "
                    '("both" "prefix" "suffix") nil t nil nil "both")))))
  (setq front-context (or front-context 'both))
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
           (id (org-custom-syntax--make-id))
           (ellipsis org-interactive-cloze-ellipsis)
           (ellipsis-mark (format "{{cloze#%s|%s|%s}}" id ellipsis id))
           front-title front-block)
      ;; Build Front BEFORE modifying buffer
      (if on-heading
          (let* ((lb (save-excursion (goto-char start0) (line-beginning-position)))
                 (le (save-excursion (goto-char end0)   (line-end-position)))
                 (pre-raw (buffer-substring-no-properties lb start0))
                 (suf-raw (buffer-substring-no-properties end0 le))
                 (pre (my--strip-clozes-in-string pre-raw))
                 (suf (my--strip-clozes-in-string suf-raw)))
            (setq front-title
                  (my--strip-leading-stars
                   (pcase front-context
                     ('both   (concat pre ellipsis-mark suf))
                     ('prefix (concat pre ellipsis-mark))
                     ('suffix (concat ellipsis-mark suf))
                     (_       (concat pre ellipsis-mark suf))))))
        (let* ((bounds (my--org-entry-body-bounds))
               (bbeg (car bounds))
               (bend (cdr bounds))
               (s (max start0 bbeg))
               (e (min end0 bend))
               (pre (my--strip-clozes-in-string
                     (buffer-substring-no-properties bbeg s)))
               (suf (my--strip-clozes-in-string
                     (buffer-substring-no-properties e bend))))
          (setq front-block
                (pcase front-context
                  ('both   (concat pre ellipsis-mark suf))
                  ('prefix (concat pre ellipsis-mark))
                  ('suffix (concat ellipsis-mark suf))
                  (_       (concat pre ellipsis-mark suf))))))
      ;; Replace selection with clozed marker
      (delete-region start0 end0)
      (insert (format "{{clozed#%s|%s|%s}}" id selected-text id))
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
      (message "Created cloze (%s)" (symbol-name front-context))
      (save-buffer))))

;; Thin wrappers (no duplicated logic): just pass the option for convenience
(defun org-interactive-cloze-prefix ()
  "Create a cloze whose Front shows only the preceding context."
  (interactive)
  (org-interactive-cloze 'prefix))

(defun org-interactive-cloze-suffix ()
  "Create a cloze whose Front shows only the following context."
  (interactive)
  (org-interactive-cloze 'suffix))
  
(defun org-interactive-extract ()
  "Create an extract using {{extract#ID|SELECTION|ID}} and a child heading."
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
                                   (= (char-before end) ?\n)))
           (id (org-custom-syntax--make-id)))
      (setq cleaned-text (replace-regexp-in-string "\\^{[^}]*}" "" cleaned-text))
      (setq cleaned-text (replace-regexp-in-string "  +" " " cleaned-text))
      (delete-region start end)
      (if ends-with-newline
          (let ((text-without-newline (substring selected-text 0 -1)))
            (insert (format "{{extract#%s|%s|%s}}\n" id text-without-newline id)))
        (insert (format "{{extract#%s|%s|%s}}" id selected-text id)))
      (save-excursion
        (goto-char heading-pos)
        (org-end-of-subtree)
        (let ((new-heading-pos (point)))
          (insert "\n" (make-string (1+ heading-level) ?*) " ")
          (insert "\n" cleaned-text)
          (when parent-priority-range
            (goto-char (1+ new-heading-pos))
            (my-set-priority-with-heuristics parent-priority-range))))
      (message "Created extract from selected text")
      (save-buffer))))

(defun org-remove-all-extract-blocks ()
  "Remove all {{extract#ID|...|ID}} blocks entirely from the current buffer."
  (interactive)
  (when (eq major-mode 'org-mode)
    (org-clear-custom-overlays)
    (save-excursion
      (goto-char (point-min))
      (let ((count 0))
        (while (re-search-forward "{{extract#\\([A-Za-z0-9_-]+\\)|" nil t)
          (let* ((start (match-beginning 0))
                 (id (match-string-no-properties 1)))
            (if (search-forward (format "|%s}}" id) nil t)
                (progn
                  (delete-region start (point))
                  (cl-incf count))
              (goto-char (1+ start)))))
        (goto-char (point-min))
        (while (re-search-forward "\n\n\n+" nil t)
          (replace-match "\n\n"))
        (message "Removed %d extract blocks" count)
        (save-buffer)))
    (org-highlight-custom-syntax)))

;; make the timer buffer-local
(defvar-local org-custom-syntax-timer nil)

(defun org-update-custom-syntax-after-change (_beg _end _len)
  "Update custom syntax highlighting after buffer changes."
  (when (eq major-mode 'org-mode)
    (when org-custom-syntax-timer
      (cancel-timer org-custom-syntax-timer))
    (let ((buf (current-buffer)))
      (setq org-custom-syntax-timer
            (run-with-idle-timer
             0.2 nil
             (lambda (b)
               (when (buffer-live-p b)
                 (with-current-buffer b
                   (org-highlight-custom-syntax))))
             buf)))))

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
