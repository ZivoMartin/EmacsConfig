(add-to-list 'load-path (expand-file-name "lisp" user-emacs-directory))

(require 'package)
(require 'init-elpa)
(require 'init-exec-path)
(require 'init-editing)
(require 'init-miscellaneous)
(require 'init-company-mode)
(require 'init-ui)
(require 'init-package-import)
(require 'init-ocaml)
(require 'init-zig)
(require 'init-vue)
(require 'init-nasm)
(require 'init-rust)
(require 'init-lisp)
(require 'init-haskell)

(provide 'init)


(setq custom-file "~/.emacs.d/custom.el")
(load custom-file t)

(setq backup-directory-alist
      `(("." . "~/.emacs.d/backups"))
      auto-save-file-name-transforms
      `((".*" "~/.emacs.d/auto-save-list/" t)))


(setq read-process-output-max (* 1024 1024))

(setq-default fill-column 80)
(add-hook 'prog-mode-hook
	      (defun brw/prog-mode-hook ()
	        (display-line-numbers-mode)
	        (display-fill-column-indicator-mode)))


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
(global-set-key (kbd "C-%") 'replace-string)
(global-set-key (kbd "C-ù") 'query-replace)


(global-set-key (kbd "C-x DEL")
                (lambda ()
                  (interactive)
                  (execute-kbd-macro (kbd "C-SPC C-a DEL"))))
(put 'upcase-region 'disabled nil)


(defun open-vterm-split ()
  (interactive)
  (split-window-right)   
  (other-window 1)       
  (vterm))               

(global-set-key (kbd "C-c t") 'open-vterm-split)

(defun open-new-vterm ()
  "Crée un nouveau vterm avec un nom unique."
  (interactive)
  (let ((buffer (generate-new-buffer-name "vterm"))) ;; Génère un nom unique (ex: vterm<2>)
    (vterm buffer))) ;; Ouvre un nouveau buffer avec ce nom

(global-set-key (kbd "C-c n") 'open-new-vterm) ;; Associe à `C-c n`

(defhydra hydra-resize-window (:hint nil)
  "
Resize window:
  _<left>_: shrink-horizontally   _<right>_: enlarge-horizontally
  _<up>_: enlarge-vertically      _<down>_: shrink-vertically
  _q_: quit
"
  ("<left>"  (shrink-window-horizontally 2))
  ("<right>" (enlarge-window-horizontally 2))
  ("<up>"    (enlarge-window 2))
  ("<down>"  (shrink-window 2))
  ("q" nil))

(global-set-key (kbd "C-<") 'hydra-resize-window/body)

(use-package wrap-region
  :ensure t
  :config
  (wrap-region-global-mode t))

(defun reload-init-file ()
  (interactive)
  (load-file user-init-file))

(require 'cc-mode)
(require 'semantic)
(semantic-mode 1)

(defun jump-to-c-symbol-direct ()
  "Jump directly to the definition of the symbol at point using Semantic."
  (interactive)
  (let ((tag (thing-at-point 'symbol t)))
    (if tag
        (semantic-ia-fast-jump (point))
      (message "No symbol at point."))))

(add-hook 'c-mode-common-hook
          (lambda ()
            (local-set-key (kbd "C-<return>") #'jump-to-c-symbol-direct)))
