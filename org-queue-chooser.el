;;; org-queue-chooser.el --- Queue chooser UI for org-queue -*- lexical-binding: t; -*-

;;; Commentary:
;; Provides the tabulated-list UI, marking,
;; subset mode, schedule/priority tools, and navigation helpers.

;;; Code:

(require 'tabulated-list)
(require 'org)
(require 'cl-lib)
(require 'seq)
(require 'subr-x)

(require 'org-queue-priority)  ;; for my-queue-spread-priorities, my-get-raw-priority-value
(require 'org-queue-schedule)  ;; for my-advance-schedule / my-postpone-schedule
(require 'org-queue-tasks)     ;; for my-outstanding-* vars and helpers
(require 'org-queue-display)   ;; for my-display-task-at-marker, my-pulse-highlight-current-line

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
  '((t (:inherit (fixed-pitch shadow))))
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
    (define-key map (kbd "S") #'org-queue-chooser-spread-schedule)
    (define-key map (kbd "H") #'org-queue-chooser-spread-priorities)
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
    (define-key map (kbd "G") #'org-queue-chooser-hard-refresh)
    ;; Open chooser in its own tab (foreground)
    (define-key map (kbd "C-c T") #'org-queue-chooser-open-in-tab)
    ;; Reorder tasks (global queue only)
    (define-key map (kbd "M-n")      #'org-queue-chooser-move-down)
    (define-key map (kbd "M-p")      #'org-queue-chooser-move-up)
    (define-key map (kbd "M-<down>") #'org-queue-chooser-move-down)
    (define-key map (kbd "M-<up>")   #'org-queue-chooser-move-up)
    (define-key map (kbd "M-g M-g")  #'org-queue-chooser-move-to-position)
    ;; Quit
    (define-key map (kbd "q") #'quit-window)
    ;; Disable 'n' to prevent conflict with next-line
    (define-key map (kbd "n") #'ignore)
    map)
  "Keymap for org-queue-chooser-mode.")

(define-derived-mode org-queue-chooser-mode tabulated-list-mode "Org-Queue-Chooser"
  "Browse and operate on queue tasks (global or subset)."
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
  (setq tabulated-list-use-header-line nil)
  ;; fixed-width alignment and other settings identical to your current code...
  (setq-local truncate-lines t)
  (setq-local line-move-visual nil)
  (setq-local word-wrap nil)
  (setq-local auto-hscroll-mode 1)
  (setq-local hscroll-margin 5)
  (setq-local hscroll-step 2)
  (tabulated-list-init-header))

(defvar-local org-queue-chooser--tasks nil
  "If non-nil, this chooser shows these tasks instead of the global queue.")

(defvar-local org-queue-chooser--subset-p nil
  "Non-nil when displaying a subset (does not allow reordering of the global queue).")

(defvar-local org-queue-chooser--marks (make-hash-table :test 'eql)
  "Row marks for bulk operations.")

(defun org-queue-chooser--task-list ()
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
  "One-line preview for TASK body only (no children/meta), without modifying buffers."
  (let* ((m (my-extract-marker task))
         (buf (and (markerp m) (marker-buffer m))))
    (when (and (markerp m) buf (buffer-live-p buf))
      (with-current-buffer buf
        (save-restriction
          (widen)
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
                          (or (and (outline-next-heading) (point)) (point-max))))
                   (raw (buffer-substring-no-properties
                         start (min end (+ start 400))))
                   ;; Strip #+ lines in string space only
                   (raw-no-meta (string-join
                                 (cl-remove-if
                                  (lambda (l) (string-match-p "^\\s-*#\\+" l))
                                  (split-string raw "\n"))
                                 " "))
                   (plain (org-queue-chooser--org-to-plain raw-no-meta)))
              (string-trim (replace-regexp-in-string "[ \t\n\r]+" " " plain)))))))))

(defun org-queue-chooser--scheduled-string (task)
  "Return YYYY-MM-DD or empty."
  (let* ((m (my-extract-marker task))
         (buf (and (markerp m) (marker-buffer m))))
    (when (and (markerp m) buf (buffer-live-p buf))
      (with-current-buffer buf
        (save-restriction
          (widen)
          (save-excursion
            (goto-char (marker-position m))
            (let ((tm (org-get-scheduled-time nil)))
              (if tm (format-time-string "%Y-%m-%d" tm) ""))))))))

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
             ;; Prefer stored heading; fallback to live heading widened
             (title-raw (or (plist-get task :heading)
                            (when (and (markerp m) buf (buffer-live-p buf))
                              (with-current-buffer buf
                                (save-restriction
                                  (widen)
                                  (save-excursion
                                    (goto-char (marker-position m))
                                    (org-get-heading t t t t)))))))
             (title (org-queue-chooser--asciiize
                     (org-queue-chooser--heading-as-plain (or title-raw ""))))
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
  (my-launch-anki)
  (let ((inhibit-read-only t))
    (setq tabulated-list-entries (org-queue-chooser--entries))
    (tabulated-list-print t)))

(defun org-queue-chooser-hard-refresh ()
  "Force reindex + rebuild the global queue, then refresh chooser.
In subset mode, only refresh the subset view (no global rebuild)."
  (interactive)
  (my-launch-anki)
  (if org-queue-chooser--subset-p
      (progn
        (org-queue-chooser-refresh)
        (message "Subset view: refreshed entries (global hard refresh not applied)."))
    ;; Global queue: reindex + rebuild + refresh
    (org-queue-reindex-files)            ;; optional: log how many files found
    (my-get-outstanding-tasks)           ;; rebuild queue from files
    (my-save-outstanding-tasks-to-file)  ;; update cache
    (setq org-queue-chooser--tasks nil
          org-queue-chooser--subset-p nil
          org-queue-chooser--marks (make-hash-table :test 'eql))
    (org-queue-chooser-refresh)
    (org-queue-chooser--goto-index my-outstanding-tasks-index)
    (message "Queue reindexed and rebuilt: %d tasks"
             (length my-outstanding-tasks-list))))

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
  (my-launch-anki)
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
  (my-launch-anki)
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
         (title-raw (plist-get task :heading))
         (title (org-queue-chooser--asciiize
                 (org-queue-chooser--heading-as-plain (or title-raw ""))))
         (file (and task (plist-get task :file)))
         (base (cond ((and title (> (length title) 0)) title)
                     (file (file-name-nondirectory file))
                     (t "Untitled"))))
    (format "#%d %s" (1+ idx)
            (if (> (length base) 40)
                (concat (substring base 0 39) "...")
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
  "Move the selected task to POS (1-based) in the global queue."
  (interactive "nMove to position (1-based): ")
  (org-queue-chooser--require-global)
  (let* ((from (tabulated-list-get-id))
         (len (length my-outstanding-tasks-list))
         (to  (1- (max 1 (min len pos)))))
    (unless (numberp from)
      (user-error "No task selected"))
    (if (= from to)
        (message "Already at that position")
      (let ((old-list my-outstanding-tasks-list))
        (setq my-outstanding-tasks-list
              (org-queue-chooser--move-element old-list from to))
        (setq my-outstanding-tasks-index
              (org-queue-chooser--adjust-index-after-move
               my-outstanding-tasks-index from to))
        (my-save-outstanding-tasks-to-file)
        (my-queue-limit-visible-buffers)
        (org-queue-chooser-refresh)
        (org-queue-chooser--goto-index to)
        (message "Moved to position %d" (1+ to))))))

;; Mark helpers
(defun org-queue-chooser--make-row (i)
  "Return a single tabulated-list entry (ID . VECTOR) for row I."
  (let* ((lst (org-queue-chooser--task-list))
         (task (nth i lst))
         (m (my-extract-marker task))
         (buf (and (markerp m) (marker-buffer m)))
         (title-raw (or (plist-get task :heading)
                        (when (and (markerp m) buf (buffer-live-p buf))
                          (with-current-buffer buf
                            (save-restriction
                              (widen)
                              (save-excursion
                                (goto-char (marker-position m))
                                (org-get-heading t t t t)))))))
         (title (org-queue-chooser--asciiize
                 (org-queue-chooser--heading-as-plain (or title-raw ""))))
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

  (my-launch-anki)
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

(provide 'org-queue-chooser)
;;; org-queue-chooser.el ends here
