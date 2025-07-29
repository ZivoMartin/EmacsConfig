;;; init-rust.el --- Rust programming configuration -*- lexical-binding: t; -*-

;;; Commentary:
;; Provides Rust language support via `rust-mode` and `eglot`.
;; Syntax highlighting, LSP, auto-formatting, and inline documentation.

;;; Code:

(use-package rust-mode
  :mode ("\\.rs\\'" . rust-mode)
  :hook (rust-mode . my/rust-setup))

(defun my/rust-setup ()
  "Setup rust mode with LSP and formatting."
  (eglot-ensure)                            ;; Start LSP (eglot + rust-analyzer)
  (setq indent-tabs-mode nil)              ;; Use spaces
  (setq rust-format-on-save t))            ;; Format using rustfmt on save

(provide 'init-rust)
;;; init-rust.el ends here
