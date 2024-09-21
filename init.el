(add-to-list 'load-path (expand-file-name "lisp" user-emacs-directory))

(when (>= emacs-major-version 24)
  (progn
    ;; load emacs 24's package system.
    (require 'package)
    ;; Add MELPA repository.
    (add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/") t))
  (when (< emacs-major-version 27) (package-initialize)))

(require 'init-elpa)
(require 'init-exec-path)
(require 'init-editing)
(require 'init-miscellaneous)
(require 'init-company-mode)
(require 'init-ui)
(require 'init-ocaml)

(provide 'init)
(require 'package)
(add-to-list 'package-archives
             '("melpa" . "https://melpa.org/packages/") t)

(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))

(require 'use-package)

(use-package rust-mode
  :ensure t
  :mode "\\.rs\\'"
  :hook (rust-mode . lsp))

(use-package lsp-mode
  :ensure t
  :commands lsp
  :hook ((lsp-mode . lsp-ui-mode)
         (lsp-mode . company-mode))
  :custom
  (lsp-rust-analyzer-server-command '("rust-analyzer"))
  (lsp-ui-sideline-enable nil)
  (lsp-ui-doc-enable nil)
  (company-idle-delay 0.2)
  (company-minimum-prefix-length 1)
  (lsp-rust-analyzer-cargo-watch-command "clippy")
  (lsp-rust-analyzer-diagnostics-enable-experimental t)
  (lsp-rust-analyzer-cargo-load-out-dirs-from-check t)
  (lsp-rust-analyzer-proc-macro-enable t))


(unless (version< emacs-version "24")
  (add-to-list 'load-path "/home/martin/.emacs.d/zig-mode/")
  (autoload 'zig-mode "zig-mode" nil t)
  (add-to-list 'auto-mode-alist '("\\.zig\\'" . zig-mode)))

(setq custom-file "~/.emacs.d/custom.el")
(load custom-file t)

(setq backup-directory-alist
      `(("." . "~/.emacs.d/backups"))
      auto-save-file-name-transforms
      `((".*" "~/.emacs.d/auto-save-list/" t)))


(use-package magit
  :ensure t)

(use-package company
  :ensure t
  :hook (emacs-lisp-mode . company-tng-mode)
  :config
  (setq company-idle-delay 0.5
	company-minimum-prefix-length 2))

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

(setq read-process-output-max (* 1024 1024))

(use-package rust-mode
  :ensure t)

(add-to-list 'load-path "rust-mode")
(autoload 'rust-mode "rust-mode" nil t)
(add-to-list 'auto-mode-alist '("\\.rs\\'" . rust-mode))

(setq rust-format-on-save t)
(add-hook 'rust-mode-hook
          (lambda () (prettify-symbols-mode)))
(add-hook 'rust-mode-hook
          (lambda () (setq indent-tabs-mode nil)))

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
(add-hook 'rust-mode-hook #'lsp)

(use-package vertico
  :ensure t
  :config
  (vertico-mode 1))

(use-package rainbow-mode
  :ensure t)

(use-package markdown-mode
  :ensure t)

(setq-default fill-column 80)
(add-hook 'prog-mode-hook
	  (defun brw/prog-mode-hook ()
	    (display-line-numbers-mode)
	    (display-fill-column-indicator-mode)))

(global-set-key (kbd "<f5>") 'recompile)
(global-set-key (kbd "C-c i") 'imenu)
(global-set-key (kbd "C-M-.") 'xref-find-definitions-other-window)
 
(load-theme 'tango-dark)

;;; fonts
(defun brw/big-font ()
  "Set a larger font for streaming"
  (interactive)
  (set-face-attribute 'default nil :height 140))

(defun brw/normal-font ()
  "Set a more usable font for not streaming"
  (interactive)
  (set-face-attribute 'default nil :height 110))

(brw/normal-font)

;; try to fix the unreadable completion colors
(set-face-attribute 'vertico-current nil :background "#d0d0FF")
(set-face-attribute 'completions-common-part nil :foreground "#0000FF")

;; disable all bars
(tool-bar-mode -1)
(menu-bar-mode -1)
(scroll-bar-mode -1)

;; improve scrolling behavior
(setq scroll-step 1
      scroll-conservatively 1)

;; pretend like I have winum
(global-set-key (kbd "M-1") 'other-window)
(global-set-key (kbd "M-2") 'other-window)

;; enable undoing window changes
(winner-mode t)
(require 'multiple-cursors)

(global-set-key (kbd "M-n") 'mc/edit-lines)


(global-set-key (kbd "C-x DEL")
                (lambda ()
                  (interactive)
                  (execute-kbd-macro (kbd "C-SPC C-a DEL"))))
(put 'upcase-region 'disabled nil)


(defun rust-analyzer-add-missing-imports ()
  "Runs the 'rust-analyzer.add-missing-imports' code action."
  (interactive)
  (let ((action (seq-find
                 (lambda (action)
                   (string-equal (gethash "command" action) "rust-analyzer.add-missing-imports"))
                 (lsp-code-actions-at-point))))
    ))

(define-key rust-mode-map (kbd "C-c C-i") 'rust-analyzer-add-missing-imports)
(unless (package-installed-p 'flycheck)
  (package-refresh-contents)
  (package-install 'flycheck))

(add-hook 'after-init-hook #'global-flycheck-mode)
(put 'downcase-region 'disabled nil)

(add-hook 'rust-mode-hook
          (lambda ()
            (local-set-key (kbd "C-c C-c") 'compile)))
