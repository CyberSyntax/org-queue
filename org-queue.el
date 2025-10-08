;;; org-queue.el --- Org-mode task queue management -*- lexical-binding: t -*-

;; Load modular components
(require 'org-queue-srs-bridge)
(require 'org-queue-config)
(require 'org-queue-utils)
(require 'org-queue-priority)
(require 'org-queue-schedule)
(require 'org-queue-tasks)
(require 'org-queue-display)
(require 'org-queue-chooser)
(require 'org-queue-gptel-bridge)

;; Global keymap
(defvar org-queue-prefix-map (make-sparse-keymap)
  "Prefix keymap for org-queue commands.")

(dolist (binding '(("," my-set-priority-with-heuristics)
                   ("s" my-schedule-command)
                   ("f" my-show-next-outstanding-task)
                   ("F" my-show-next-outstanding-task-in-new-tab)
                   ("b" my-show-previous-outstanding-task)
                   ("B" my-show-previous-outstanding-task-in-new-tab)
                   ("c" my-show-current-outstanding-task)
                   ("C" my-show-current-outstanding-task-in-new-tab) 
                   ("Q" org-queue-open-chooser)
                   ("q" org-queue-open-chooser-from-buffer)
                   ("r" my-remove-current-task)
                   ("R" my-reset-and-show-current-outstanding-task)
                   ("G" org-queue-hard-refresh)
                   ("i" my-increase-priority-range)
                   ("d" my-decrease-priority-range)
                   ("D" org-demote-subtree)
                   ("a" my-advance-schedule)
                   ("A" my-launch-anki)
                   ("p" my-postpone-schedule)
                   ("P" org-promote-subtree)
                   ("m" my-move-current-task-to-position)
                   ("w" widen-and-recenter)
                   ("u" org-show-parent-heading-cleanly)
                   ("x" org-interactive-extract)
                   ("X" org-remove-all-extract-blocks)))
  (let ((key (car binding))
        (command (cadr binding)))
    (define-key org-queue-prefix-map (kbd key) command)))

;; Optional commands
(when (require 'org-web-tools nil t)
  (define-key org-queue-prefix-map (kbd "l") #'org-web-tools-insert-link-for-url)
  (define-key org-queue-prefix-map (kbd "I") #'org-web-tools-insert-web-page-as-entry))

(when (require 'gptel nil t)
  (define-key org-queue-prefix-map (kbd "g") #'gptel))

;; SRS review commands - bind if functions exist
(when (fboundp 'org-queue-srs-rate-again)
  (define-key org-queue-prefix-map (kbd "1") #'org-queue-srs-rate-again))
(when (fboundp 'org-queue-srs-rate-good)
  (define-key org-queue-prefix-map (kbd "3") #'org-queue-srs-rate-good))

;; Bind S if SRS item creation is available
(when (fboundp 'org-queue-srs-item-create-card)
  (define-key org-queue-prefix-map (kbd "S") #'org-queue-srs-item-create-card))

;; Bind cloze keys if cloze is available
(when (fboundp 'org-interactive-cloze)
  (define-key org-queue-prefix-map (kbd "z") #'org-interactive-cloze)
  (define-key org-queue-prefix-map (kbd "Z") #'org-interactive-cloze-prefix))

;; Bind the prefix globally to C-c q
(global-set-key (kbd "C-c q") org-queue-prefix-map)

(provide 'org-queue)
;;; org-queue.el ends here
