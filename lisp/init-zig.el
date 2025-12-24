;;; init-zig.el --- Zig configuration using Eglot

;;; Commentary:
;; Provides Zig IDE features via eglot and zls.
;; Includes jump to definition, diagnostics, and formatting on save.

;;; Code:


(use-package lsp-mode
  :commands (lsp lsp-deferred)
  :init
  (setq lsp-keymap-prefix "C-c l"))

(use-package zig-mode
  :mode "\\.zig\\'"
  :hook
  ((zig-mode . lsp-deferred)
   (zig-mode . zig--enable-format-on-save))
  :config
  (defun zig--enable-format-on-save ()
    "Format Zig buffers on save."
    (add-hook 'before-save-hook #'zig-format-buffer nil t)))
 
(provide 'init-zig)
;;; init-zig.el ends here
