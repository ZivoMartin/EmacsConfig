;;; init-zig.el --- Minimal emacs zig config -*- lexical-binding: t; -*-

;;; Commentary:

;;; Code:

(require 'init-use-package)
(require 'init-eglot)

(defun zig--enable-format-on-save ()
    "Format Zig buffers on save."
    (add-hook 'before-save-hook #'zig-format-buffer nil t))

(use-package zig-mode
  :mode "\\.zig\\'"
  :hook
  ((zig-mode . eglot-ensure)
   (zig-mode . zig--enable-format-on-save)))

(provide 'init-zig)
;;; init-zig.el ends here
