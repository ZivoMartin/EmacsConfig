;;; init.el --- Configure my personal emacs config  -*- lexical-binding: t; -*-

;;; Commentary:

;;; Code:

(message "Loading main config...")

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
(require 'init-projectile)
(require 'init-multi-cursor)
(require 'init-company)
(require 'init-yasnippet)
(require 'init-dashboard)
(require 'init-electric)
(require 'init-minibuffer)
(require 'init-pdf)
(require 'init-search-engine)
(require 'init-expand-region)
(require 'init-orderless)

;; Emacs customisation

(require 'init-ui)
(require 'init-dark-themes)
(require 'init-light-themes)
(require 'init-backups)
(require 'init-indents)

;; Languages setup

(require 'init-elisp)
(require 'init-nasm)
(require 'init-rust)
(require 'init-ocaml)
(require 'init-c)
(require 'init-zig)
(require 'init-miking)
(require 'init-markdown)
(require 'init-make)
(require 'init-dockerfile)

(message "Load has been successfull.")

(provide 'init)
;;; init.el ends here

