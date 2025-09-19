;;; init.el --- Personal Emacs configuration -*- lexical-binding: t; -*-

;;; Commentary:
;; This is my personal Emacs configuration, split into modular files.

;;; Code:

(eval-when-compile
  (add-to-list 'load-path (expand-file-name "lisp" user-emacs-directory)))

 (setq custom-file (expand-file-name "custom.el" user-emacs-directory))
(load custom-file 'noerror 'nomessage)

;; (require 'init-var)
(require 'init-elpa)

(require 'init-exec-path)
(require 'init-editing)

(require 'init-lisp)
(require 'init-nasm)
(require 'init-rust)
(require 'init-ocaml)
(require 'init-java)
(require 'init-javascript)
(require 'init-react)
(require 'init-c)
(require 'init-zig)
(require 'init-haskell)
(require 'init-miking)
(require 'init-markdown)
(require 'init-latex)
(require 'init-pdf)
(require 'init-make)
(require 'init-text)

(require 'init-ui)
(require 'init-miscellaneous)
(require 'init-electric)
(require 'init-magit-mode)
(require 'init-company-mode)
(require 'init-vterm)
(require 'init-multi-cursor)
(require 'init-org-mode)
(require 'init-projectile)
(require 'init-isearch)
(require 'init-drag-stuff)
(require 'init-bindings)
;; (require 'init-evil)

(provide 'init)
;;; init.el ends here
