;;; org-queue-keys.el --- Global keymap for org-queue (C-c q) -*- lexical-binding: t; -*-

;;; Code:

(require 'org)
(require 'org-queue-config)
(require 'org-queue-utils)
(require 'org-queue-tasks)
(require 'org-queue-schedule)
(require 'org-queue-chooser)
(require 'org-queue-display)
(require 'org-queue-srs-bridge)

;; One canonical, global prefix map
(defvar org-queue-prefix-map (make-sparse-keymap)
  "Prefix keymap for org-queue commands.")

(dolist (binding '(("," my-set-priority-with-heuristics)
                   ("+" my-increase-priority-range)
                   ("-" my-decrease-priority-range)
                   ("a" my-advance-schedule)
                   ("A" my-launch-anki)
                   ("c" org-queue-show-top)                 ;; Show best next
                   ("D" org-demote-subtree)
                   ("g" org-queue-rebuild-soft)             ;; Promote pending (no file scan)
                   ("G" org-queue-hard-refresh)             ;; Reindex + rebuild
                   ("n" org-queue-stamp-and-show-top)       ;; Stamp and show new top
                   ("p" my-postpone-schedule)
                   ("P" org-promote-subtree)
                   ("s" my-schedule-command)
                   ("u" org-show-parent-heading-cleanly)
                   ("w" widen-and-recenter)
                   ("x" org-interactive-extract)
                   ("X" org-remove-all-extract-blocks)))
  (define-key org-queue-prefix-map (kbd (car binding)) (cadr binding)))

;; Optional commands
(when (require 'org-web-tools nil t)
  (define-key org-queue-prefix-map (kbd "l") #'org-web-tools-insert-link-for-url)
  (define-key org-queue-prefix-map (kbd "I") #'org-web-tools-insert-web-page-as-entry))

;; SRS review commands - numeric (Anki muscle memory)
(when (fboundp 'org-queue-srs-rate-again)
  (define-key org-queue-prefix-map (kbd "1") #'org-queue-srs-rate-again))
(when (fboundp 'org-queue-srs-rate-good)
  (define-key org-queue-prefix-map (kbd "3") #'org-queue-srs-rate-good))

;; Bind S if SRS item creation is available
(when (fboundp 'org-queue-srs-item-create-card)
  (define-key org-queue-prefix-map (kbd "S") #'org-queue-srs-item-create-card))

;; Cloze helpers (z belongs to cloze; Snooze is on 'n')
(when (fboundp 'org-interactive-cloze)
  (define-key org-queue-prefix-map (kbd "z")  #'org-interactive-cloze)
  (define-key org-queue-prefix-map (kbd "Z")  #'org-interactive-cloze-prefix)
  (define-key org-queue-prefix-map (kbd "M-z") #'org-interactive-cloze-suffix))

;; Install the prefix globally (fixed; no defcustom)
(global-set-key (kbd "C-c q") org-queue-prefix-map)

(provide 'org-queue-keys)
;;; org-queue-keys.el ends here
