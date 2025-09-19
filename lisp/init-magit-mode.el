;; init-magit-mode.el --- Git support with Magit -*- lexical-binding: t; -*-
;;; Commentary:
;; Full Magit integration: Git status, staging, diffs, history, and more.
;; Provides convenient keybindings and optional GitHub integration via Forge.

;;; Code:

(autoload 'martin-global-mode "init-bindings" nil t)
(autoload 'martin-mode "init-bindings" nil t)

(use-package magit
  :ensure t
  :config
  (define-key magit-file-section-map (kbd "C-j") #'previous-line)
  
  (setq magit-refresh-status-buffer t)
  (setq magit-auto-revert-mode t)
  
  (setq magit-save-repository-buffers 'dontask)
  (setq magit-no-confirm '(stage-all-changes)))

(defun my-magit-diff-file (file)
  "Run magit-diff on FILE against HEAD."
  (interactive "fSelect file or directory: ")
  (magit-diff-dwim (list file)))

(provide 'init-magit-mode)
;;; init-magit-mode.el ends here
