(require 'init-elpa)
(require-package 'company)
(require 'company)

(setq company-tooltip-align-annotations t)
(add-hook 'prog-mode-hook 'company-mode)

(use-package company
  :ensure t
  :hook (emacs-lisp-mode . company-tng-mode)
  :config
  (setq company-idle-delay 0.1
	company-minimum-prefix-length 1))

(add-hook 'after-init-hook 'global-company-mode)


(provide 'init-company-mode)
