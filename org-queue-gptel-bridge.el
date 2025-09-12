;;; org-queue-gptel-bridge.el --- gptel integration for org-queue -*- lexical-binding: t -*-

;;; Code:

;; Launch/focus Anki whenever gptel sends a request.
;; No effect (and no errors) if gptel isn't installed/loaded.
(with-eval-after-load 'gptel
  (when (fboundp 'gptel-send)
    (defun org-queue--launch-anki-after-gptel-send (&rest _args)
      (when (fboundp 'my-launch-anki)
        (ignore-errors (my-launch-anki))))
    ;; Prevent duplicate advice across reloads
    (unless (advice-member-p #'org-queue--launch-anki-after-gptel-send 'gptel-send)
      (advice-add 'gptel-send :after #'org-queue--launch-anki-after-gptel-send))))

(provide 'org-queue-gptel-bridge)
;;; org-queue-gptel-bridge.el ends here
