;;; init.el --- Configure my personal emacs config  -*- lexical-binding: t; -*-

;;; Commentary:

;;; Code:

;; Prioritary packages

(require 'init-general-settings)
(require 'init-use-package)
(require 'init-martin-group)
(require 'init-martin-mode)

;; Emacs plugins

(require 'init-eglot)
(require 'init-magit)
(require 'init-vterm)
(require 'init-isearch)
(require 'init-rainbow-delimiters)
(require 'init-org)
;; (require 'init-projectile)
;; (require 'init-multi-cursor)
;; (require 'init-company)
;; (require 'init-dashboard)
(require 'init-electric)
(require 'init-minibuffer)
(require 'init-pdf)
(require 'init-search-engine)
(require 'init-expand-region)

;; Emacs customisation

(require 'init-ui)
(require 'init-backups)
(require 'init-indents)

;; Languages customisations

(require 'init-elisp)
;; (require 'init-nasm)
;; (require 'init-rust)
;; (require 'init-ocaml)
;; (require 'init-java)
;; (require 'init-javascript)
;; (require 'init-react)
;; (require 'init-c)
(require 'init-zig)
;; (require 'init-haskell)
;; (require 'init-miking)
;; (require 'init-markdown)
;; (require 'init-latex)
(require 'init-make)
;; (require 'init-dockerfile)

(provide 'init)
;;; init.el ends here
(put 'erase-buffer 'disabled nil)
