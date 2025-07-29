;;; init-zig.el --- Zig configuration using Eglot

;;; Commentary:
;; Provides Zig IDE features via eglot and zls.
;; Includes jump to definition, diagnostics, and formatting on save.

;;; Code:

(use-package zig-mode
  :mode "\\.zig\\'"
  :hook ((zig-mode . eglot-ensure)
         (zig-mode . my/zig-format-on-save)))

;; Formatting on save using zig fmt
(defun my/zig-format-on-save ()
  "Format Zig code with `zig fmt` on save."
  (add-hook 'before-save-hook #'zig-format-buffer nil t))

(provide 'init-zig)
;;; init-zig.el ends here
