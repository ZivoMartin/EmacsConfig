;;; init-magit.el --- Defines all the magit preferences -*- lexical-binding: t; -*-

;;; Commentary:

;;; Code:

(require 'init-martin-mode)

(defun overwrite-git-commit-keymap ()
  "We just replace the `git-commit-mode` M\-p with the `martin-mode` one."
  (keymap-set git-commit-mode-map "M-p" (keymap-lookup martin-mode-keymap "M-p"))
  (keymap-set git-commit-mode-map "M-<up>" 'git-commit-prev-message))

(use-package magit
  :hook ((git-commit-mode . overwrite-git-commit-keymap))
  :config
  (setq magit-no-confirm '(stage-all-changes)))

(provide 'init-magit)
;;; init-magit.el ends here
