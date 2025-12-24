;;; init-dockerfile.el --- Defines all the dockerfile preferences -*- lexical-binding: t; -*-

;;; Commentary:

;;; Code:

(use-package dockerfile-mode
  :ensure t
  :mode ("Dockerfile\\'" . dockerfile-mode)
  :config
  (setq dockerfile-mode-command "docker"))

(provide 'init-dockerfile)
;;; init-dockerfile.el ends here
