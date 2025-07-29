;;; init-nasm.el --- NASM assembly support -*- lexical-binding: t; -*-

;;; Commentary:
;; Setup for editing NASM assembly files:
;; - Syntax highlighting with `nasm-mode`
;; - Automatic use for `.asm` and `.nasm` files
;; - Readable indentation settings

;;; Code:

(use-package nasm-mode
  :mode ("\\.\\(asm\\|nasm\\)\\'" . nasm-mode)
  :hook (nasm-mode . (lambda ()
                       (setq tab-width 8)
                       (setq indent-tabs-mode nil))))  ;; Use spaces for alignment

(provide 'init-nasm)
;;; init-nasm.el ends here
