;;; init-make.el --- Minimal Makefile integration for Emacs -*- lexical-binding: t; -*-

;;; Commentary:
;; This file provides a simple integration with GNU Make.
;; It automatically sets `compile-command` to run `make`
;; in the nearest directory containing a Makefile, and binds
;; `C-c C-c` to quickly compile the project.
;;
;; Usage:
;; - Place this file somewhere in your load path.
;; - Add (require 'init-make) in your init.el.
;; - When you open a file inside a project with a Makefile,
;;   `C-c C-c` (or `M-x compile`) will run `make` in the right directory.

;;; Code:

(defun my-set-makefile ()
  "Search for the nearest Makefile upwards from the current buffer's directory.
If found, configure `compile-command` so that `M-x compile`
or `C-c m` runs `make` in the directory containing that Makefile."
  (interactive)
  (let ((makefile (locate-dominating-file default-directory "Makefile")))
    (when makefile
      (setq-local compile-command (format "make -C %s" makefile)))))

;; Automatically run `my-set-makefile` when opening any file.
(add-hook 'find-file-hook #'my-set-makefile)

(defun my-compile-make ()
  "Run make immediately using the current `compile-command`."
  (interactive)
  (my-set-makefile) ;; ensure Makefile is detected
  (compile compile-command)) ;; use it directly, no prompt


;; Bind C-c C-c globally to run make without confirmation
(global-set-key (kbd "C-c C-c") #'my-compile-make)

(provide 'init-make)
