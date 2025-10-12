;;; org-queue-keys.el --- Global keymap for org-queue (C-c q) -*- lexical-binding: t; -*-

;;; Code:

(require 'org)
(require 'org-queue-config)
(require 'org-queue-utils)
(require 'org-queue-tasks)
(require 'org-queue-schedule)
(require 'org-queue-display)
(require 'org-queue-srs-bridge)

;; One canonical, global prefix map
(defvar org-queue-prefix-map (make-sparse-keymap)
  "Prefix keymap for org-queue commands.")

(dolist (binding '(("c" org-queue-show-top)
                   ("s" org-queue-schedule-and-prioritize)
                   ("," org-queue-prioritize-and-stamp)
                   ("+" org-queue-increase-priority-range-and-stamp)
                   ("-" org-queue-decrease-priority-range-and-stamp)
                   ("a" org-queue-advance-schedule-and-stamp)
                   ("p" org-queue-postpone-schedule-and-stamp)
                   ("A" my-launch-anki)
                   ("D" org-demote-subtree)
                   ("P" org-promote-subtree)
                   ("u" org-show-parent-heading-cleanly)
                   ("w" widen-and-recenter)
                   ("x" org-queue-extract-and-stamp)
                   ("X" org-queue-remove-all-extracts-and-stamp)
                   ("S" org-queue-srs-item-create-card-and-clean-stamp)
                   ("z" org-queue-cloze-and-stamp)
                   ("Z" org-queue-cloze-prefix-and-stamp)
                   ("M-z" org-queue-cloze-suffix-and-stamp)))
  (define-key org-queue-prefix-map (kbd (car binding)) (cadr binding)))

;; Optional commands
(when (require 'org-web-tools nil t)
  (define-key org-queue-prefix-map (kbd "l") #'org-web-tools-insert-link-for-url)
  (define-key org-queue-prefix-map (kbd "I") #'org-web-tools-insert-web-page-as-entry))

;; SRS review commands - numeric (Anki muscle memory)
(when (fboundp 'org-queue-srs-rate-again)
  (define-key org-queue-prefix-map (kbd "1") #'org-queue-srs-rate-again-and-prioritize))
(when (fboundp 'org-queue-srs-rate-good)
  (define-key org-queue-prefix-map (kbd "3") #'org-queue-srs-rate-good-and-prioritize))

;; Install the prefix globally
(global-set-key (kbd "C-;") org-queue-prefix-map)

(provide 'org-queue-keys)
;;; org-queue-keys.el ends here
