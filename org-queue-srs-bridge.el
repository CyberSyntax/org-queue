;;; org-queue-srs-bridge.el --- Integration between org-queue tasks and org-srs reviews -*- lexical-binding: t -*-

;; This package provides seamless integration between tasks and spaced repetition.

;;; Code:

(require 'org)
(require 'cl-lib)
(require 'org-queue-config)
(require 'org-queue-utils)

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
  "Rate current entry as 'again' using org-srs-review-rate-entry.
Advances the interleave phase after consuming the head, then reassigns the top."
  (interactive)
  (if (not (require 'org-srs nil t))
      (message "org-srs package not available")
    (let ((orig-buf (current-buffer)))
      (condition-case err
          (progn
            (org-srs-review-rate-entry :again)
            ;; Consuming head: advance phase once
            (let* ((ratio (or (and (boundp 'org-queue-srs-mix-ratio)
                                   org-queue-srs-mix-ratio)
                              '(1 . 4))))
              (org-queue--advance-mix-phase (car ratio) (cdr ratio)))
            (ignore-errors (org-queue--micro-update-current! 'review))
            ;; Save only the current buffer; maintenance disables this.
            (save-buffer)
            (message "Rated as '%s'" "again"))
        (error
         (message "Error rating entry: %s" (error-message-string err)))))))

(defun org-queue-srs-rate-good ()
  "Rate current entry as 'good' using org-srs-review-rate-entry.
Advances the interleave phase after consuming the head, then reassigns the top."
  (interactive)
  (if (not (require 'org-srs nil t))
      (message "org-srs package not available")
    (let ((orig-buf (current-buffer)))
      (condition-case err
          (progn
            (org-srs-review-rate-entry :good)
            ;; Consuming head: advance phase once (OK)
            (let* ((ratio (or (and (boundp 'org-queue-srs-mix-ratio)
                                   org-queue-srs-mix-ratio)
                              '(1 . 4))))
              (org-queue--advance-mix-phase (car ratio) (cdr ratio)))
            (ignore-errors (org-queue--micro-update-current! 'review))
            ;; Save only the current buffer; maintenance disables this.
            (save-buffer)
            (message "Rated as '%s'" "good"))
        (error
         (message "Error rating entry: %s" (error-message-string err)))))))

;; Rate AGAIN, then prompt for priority (single save; final 'priority update)
(defun org-queue-srs-rate-again-and-prioritize ()
  "Rate current SRS entry as 'again', then interactively choose a priority range.
Saves are batched; ends with a final micro-update tagged 'priority."
  (interactive)
  (org-queue--with-batched-saves
    ;; 1) Rate as AGAIN (advances mix phase, emits 'review update inside)
    (org-queue-srs-rate-again)
    ;; 2) Prompt for priority range exactly like pressing ","
    (when (derived-mode-p 'org-mode)
      (save-excursion
        (org-back-to-heading t)
        (call-interactively 'my-set-priority-with-heuristics)
        (ignore-errors (my-ensure-priority-set))
        ;; 3) Reflect final state after priority change
        (ignore-errors (org-queue--micro-update-current! 'priority))))))

;; Rate GOOD, then prompt for priority (single save; final 'priority update)
(defun org-queue-srs-rate-good-and-prioritize ()
  "Rate current SRS entry as 'good', then interactively choose a priority range.
Saves are batched; ends with a final micro-update tagged 'priority."
  (interactive)
  (org-queue--with-batched-saves
    ;; 1) Rate as GOOD (advances mix phase, emits 'review update inside)
    (org-queue-srs-rate-good)
    ;; 2) Prompt for priority range exactly like pressing ","
    (when (derived-mode-p 'org-mode)
      (save-excursion
        (org-back-to-heading t)
        (call-interactively 'my-set-priority-with-heuristics)
        (ignore-errors (my-ensure-priority-set))
        ;; 3) Reflect final state after priority change
        (ignore-errors (org-queue--micro-update-current! 'priority))))))

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
          (message "Created org-srs card")
          ;; Save only the current buffer; maintenance disables this.
          (save-buffer))
      (error
       (user-error "Failed to create SRS card: %s" (error-message-string err))))))

(defun org-queue-srs-item-create-card-and-clean-stamp ()
  "Convert current non‑SRS entry into an SRS card.
If :LAST_REPEAT: exists on this heading, remove it.
Single save; final micro-update tagged 'create."
  (interactive)
  (org-queue--with-batched-saves
    (save-excursion
      (org-back-to-heading t)
      (let ((was (org-entry-get nil "LAST_REPEAT")))
        (when (and was (not (string-empty-p was)))
          (ignore-errors (org-entry-delete nil "LAST_REPEAT"))
          (message "Removed :LAST_REPEAT: from current heading")))
      (org-queue-srs-item-create-card)
      (ignore-errors (org-queue--micro-update-current! 'create)))))

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
             (star (cl-find-if (lambda (r)
                                 (string= (string-trim (or (plist-get r :mark) "")) "*"))
                               rows))
             (times (delq nil (mapcar (lambda (r) (plist-get r :time)) rows))))
        (or (and star (plist-get star :time))
            (car (sort (copy-sequence times) #'time-less-p)))))))

(defun org-queue-collect-srs-due-items ()
  "Collect SRS entries due now (timestamp <= now) as task plists.

Priority policy for SRS tasks:
- If a numeric PRIORITY property is present, use it.
- Else if a numeric [#N] cookie is present in the heading, use that.
- Else default to the highest numeric priority (org-priority-highest, or 1)."
  (if (org-queue-night-shift-p)
      nil
    (let ((now (current-time))
          (items '()))
      (org-queue-map-entries
       (lambda ()
         (when (eq (org-srs-entry-p (point)) 'current)
           (let ((due (org-queue-srs-next-due-time (point))))
             (when (and due (not (time-less-p now due))) ; due <= now
               (let* ((marker (point-marker))
                      (id (or (org-entry-get nil "ID") (org-id-get-create)))
                      ;; Priority: property > cookie > highest(=1)
                      (priority
                       (let* ((ps (org-entry-get nil "PRIORITY"))
                              (cookie (and (fboundp 'my--org-heading-get-cookie-priority)
                                           (my--org-heading-get-cookie-priority))))
                         (cond
                          ((and ps (string-match-p "^[0-9]+$" ps)) (string-to-number ps))
                          ((numberp cookie) cookie)
                          (t (or (and (numberp org-priority-highest) org-priority-highest) 1)))))
                      (file (buffer-file-name))
                      (heading (org-get-heading t t t t))
                      (pos (point))
                        (task (list :id id :marker marker :priority priority
                                    :file file :is-todo nil
                                  :heading heading :pos pos
                                  :srs t :srs-due due)))
                 (push task items)))))) nil)
      (sort items (lambda (a b)
                    (time-less-p (plist-get a :srs-due)
                                 (plist-get b :srs-due)))))))

(defun org-queue-collect-srs-today ()
  "Return a cons (DUE-NOW . PENDING-TODAY) of SRS entries for today.

Priority policy for SRS tasks:
- If a numeric PRIORITY property is present, use it.
- Else if a numeric [#N] cookie is present in the heading, use that.
- Else default to the highest numeric priority (org-priority-highest, or 1)."
  (let* ((now (current-time))
         (today (format-time-string "%Y-%m-%d" now))
         (night (org-queue-night-shift-p))
         (due-now '())
         (pending '()))
    (org-queue-map-entries
     (lambda ()
       (when (eq (org-srs-entry-p (point)) 'current)
         (let ((due (org-queue-srs-next-due-time (point))))
           (when due
             (let ((due-date (format-time-string "%Y-%m-%d" due)))
               (when (string= due-date today)
                 (let* ((marker (point-marker))
                        (id (or (org-entry-get nil "ID") (org-id-get-create)))
                        ;; Priority: property > cookie > highest(=1)
                        (priority
                         (let* ((ps (org-entry-get nil "PRIORITY"))
                                (cookie (and (fboundp 'my--org-heading-get-cookie-priority)
                                             (my--org-heading-get-cookie-priority))))
                           (cond
                            ((and ps (string-match-p "^[0-9]+$" ps)) (string-to-number ps))
                            ((numberp cookie) cookie)
                            (t (or (and (numberp org-priority-highest) org-priority-highest) 1)))))
                        (flag (and (fboundp 'my-priority-flag) (my-priority-flag priority)))
                        (file (buffer-file-name))
                        (heading (org-get-heading t t t t))
                        (pos (point))
                        (task (list :id id :marker marker :priority priority
                                    :flag flag :file file :is-todo nil
                                    :heading heading :pos pos
                                    :srs t :srs-due due :available-at due)))
                   (cond
                    ;; Night shift: never put into DUE-NOW; all of today's go to pending
                    (night (push task pending))
                    ;; Daytime:
                    ((not (time-less-p now due)) (push task due-now))  ;; due <= now
                    (t (push task pending)))))))))) ;; due > now
     nil)
    ;; Sort both lists by due time ascending (stable)
    (setq due-now (sort due-now (lambda (a b)
                                  (time-less-p (plist-get a :srs-due)
                                               (plist-get b :srs-due)))))
    (setq pending (sort pending (lambda (a b)
                                  (time-less-p (plist-get a :srs-due)
                                               (plist-get b :srs-due)))))
    (cons due-now pending)))

(provide 'org-queue-srs-bridge)
;;; org-queue-srs-bridge.el ends here
