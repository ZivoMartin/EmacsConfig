;;; init-magit-mode.el --- Git support with Magit -*- lexical-binding: t; -*-

;;; Commentary:
;; Full Magit integration: Git status, staging, diffs, history, and more.
;; Provides convenient keybindings and optional GitHub integration via Forge.

;;; Code:

(use-package magit
  :ensure t
  :bind (("C-x g" . magit-status)
         ("C-x M-g" . magit-dispatch)
         ("C-c g l" . magit-log)
         ("C-c g b" . magit-blame))
  :config
  ;; Save buffers before committing
  (setq magit-save-repository-buffers 'dontask)
  ;; Don't ask to stage everything if nothing is staged
  (setq magit-no-confirm '(stage-all-changes))
  ;; Prefer opening status in full frame, then restoring window config after quit
  (setq magit-display-buffer-function #'magit-display-buffer-fullframe-status-v1))

(provide 'init-magit-mode)
;;; init-magit-mode.el ends here
