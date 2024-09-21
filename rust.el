;; Ajout des chemins de chargement pour les paquets
(add-to-list 'load-path (expand-file-name "lisp" user-emacs-directory))

;; Chargement des paquets pour Emacs 24 et supérieur
(when (>= emacs-major-version 24)
  (require 'package)
  (add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/") t)
  (when (< emacs-major-version 27) (package-initialize)))

;; Configuration de use-package
(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))
(require 'use-package)

;; Configuration de lsp-mode pour Rust
(use-package lsp-mode
  :ensure t
  :commands lsp
  :hook ((rust-mode . lsp)
         (lsp-mode . lsp-ui-mode)
         (lsp-mode . company-mode))
  :custom
  (lsp-rust-analyzer-server-command '("rust-analyzer"))
  (lsp-rust-analyzer-cargo-watch-command "clippy")
  (lsp-rust-analyzer-diagnostics-enable-experimental t)
  (lsp-rust-analyzer-cargo-load-out-dirs-from-check t)
  (lsp-rust-analyzer-proc-macro-enable t)
  (read-process-output-max (* 1024 1024))
  (lsp-ui-sideline-enable nil)
  (lsp-ui-doc-enable nil)
  (company-idle-delay 0.2)
  (company-minimum-prefix-length 1))

;; Configuration de rust-mode
(use-package rust-mode
  :ensure t
  :mode "\\.rs\\'"
  :hook ((rust-mode . (lambda ()
                        (setq indent-tabs-mode nil)
                        (prettify-symbols-mode)))
         (rust-mode . lsp))
  :custom
  (rust-format-on-save t))

;; Configuration de rust-ts-mode avec eglot
(use-package rust-ts-mode
  :ensure t
  :after (eglot)
  :hook ((rust-ts-mode . eglot-ensure)
         (rust-ts-mode . company-tng-mode)
         (rust-ts-mode . (lambda ()
                           (eglot-inlay-hints-mode -1))))
  :config
  (add-to-list 'auto-mode-alist '("\\.rs\\'" . rust-ts-mode))
  (add-to-list 'eglot-server-programs '(rust-ts-mode . ("rust-analyzer"))))

;; Configuration de company-mode
(use-package company
  :ensure t
  :hook (emacs-lisp-mode . company-tng-mode)
  :config
  (setq company-idle-delay 0.5
        company-minimum-prefix-length 2))

;; Configuration des autres paquets utiles
(use-package magit
  :ensure t)

(use-package projectile
  :ensure t
  :bind-keymap
  ("C-c p" . projectile-command-map))

(use-package eglot
  :ensure t
  :demand t
  :bind (:map eglot-mode-map
              ("<f6>" . eglot-format-buffer)
              ("C-c a" . eglot-code-actions)
              ("C-c d" . eldoc)
              ("C-c r" . eglot-rename))
  :config
  (setq eglot-ignored-server-capabilities '(:inlayHintProvider)))

(use-package eldoc
  :ensure t
  :custom (eldoc-echo-area-use-multiline-p 'truncate-sym-name-if-fit))

(use-package vertico
  :ensure t
  :config
  (vertico-mode 1))

(use-package rainbow-mode
  :ensure t)

(use-package markdown-mode
  :ensure t)

;; Configuration générale
(setq-default fill-column 80)
(add-hook 'prog-mode-hook
          (lambda ()
            (display-line-numbers-mode)
            (display-fill-column-indicator-mode)))

(global-set-key (kbd "<f5>") 'recompile)
(global-set-key (kbd "C-c i") 'imenu)
(global-set-key (kbd "C-M-.") 'xref-find-definitions-other-window)

(load-theme 'tango-dark)

;; Configuration des polices
(defun brw/big-font ()
  "Set a larger font for streaming"
  (interactive)
  (set-face-attribute 'default nil :height 140))


(defun brw/normal-font ()
  "Set a more usable font for not streaming"
  (interactive)
  (set-face-attribute 'default nil :height 110))

(brw/normal-font)

(set-face-attribute 'vertico-current nil :background "#d0d0FF")
(set-face-attribute 'completions-common-part nil :foreground "#0000FF")

(tool-bar-mode -1)
(menu-bar-mode -1)
(scroll-bar-mode -1)

(setq scroll-step 1
      scroll-conservatively 1)

(global-set-key (kbd "M-1") 'other-window)
(global-set-key (kbd "M-2") 'other-window)

(winner-mode t)
(require 'multiple-cursors)
(global-set-key (kbd "M-n") 'mc/edit-lines)

(global-set-key (kbd "C-x DEL")
                (lambda ()
                  (interactive)
                  (execute-kbd-macro (kbd "C-SPC C-a DEL"))))
(put 'upcase-region 'disabled nil)

;; Sauvegarde des fichiers et auto-save
(setq backup-directory-alist
      `(("." . "~/.emacs.d/backups"))
      auto-save-file-name-transforms
      `((".*" "~/.emacs.d/auto-save-list/" t)))

(setq custom-file "~/.emacs.d/custom.el")
(load custom-file t)
