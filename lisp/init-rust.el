;;; init-rust.el --- Defines all the rust preferences -*- lexical-binding: t; -*-

;;; Commentary:

;;; Code:

(use-package rust-mode
  :defer t
  :mode ("\\.rs\\'" . rust-mode)
  :hook (rust-mode . eglot-ensure)
  :config
  (setq rust-format-on-save t)
  (setq flycheck-disabled-checkers '(rust-cargo)))

(provide 'init-rust)
;;; init-rust.el ends here
