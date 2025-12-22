;;; init-martin-mode.el --- Definition of the keybindings -*- lexical-bindings: t; -*-

;;; Commentary:

;;; Code:

(require 'init-utils)
(require 'subword)
(require 'init-vterm)
(require 'init-martin-group)
(require 'init-indents)
(require 'init-delete-pair)
(require 'init-projectile)
(require 'init-expand-region)
(require 'init-org)
(require 'init-yasnippet)

(makunbound 'martin-mode-keymap)
(defvar martin-mode-keymap
  (let ((map (make-sparse-keymap)))
    
    ;; Cursor movments

    (keymap-set map "C-;" 'right-char)
    (keymap-set map "C-j" 'left-char)
    (keymap-set map "C-k" 'next-line)
    (keymap-set map "C-l" 'previous-line)

    (keymap-set map "M-;" 'subword-forward)
    (keymap-set map "M-j" 'subword-backward)
    (keymap-set map "M-k" 'forward-paragraph)
    (keymap-set map "M-l" 'backward-paragraph)

    (keymap-set map "C-e" 'end-of-line)
    (keymap-set map "C-a" 'beginning-of-line)
    (keymap-set map "M-a" 'back-to-indentation)

    (keymap-set map "C-M-;" 'forward-sexp)
    (keymap-set map "C-M-j" 'backward-sexp)

    ;; Goto

    (keymap-set map "M-g M-g" 'goto-line)
    (keymap-set map "M-g M-f" 'move-to-column)
    (keymap-set map "M-[" 'beginning-of-buffer)
    (keymap-set map "M-{" 'end-of-buffer)

    ;; Scrolling

    (keymap-set map "C-M-l" 'martin-scroll-down)
    (keymap-set map "C-M-k" 'martin-scroll-up)
    (keymap-set map "C-," 'recenter-top-bottom)

    ;; Deletions

    (keymap-set map "C-p" 'delete-backward-char)
    (keymap-set map "M-p" 'subword-backward-kill)
    (keymap-set map "C-M-p" 'kill-symbol-at-point)

    (keymap-set map "C-d" 'delete-char)
    (keymap-set map "M-d" 'kill-word)

    (keymap-set map "C-x C-p" 'martin-kill-line-left)
    (keymap-set map "C-x p" 'martin-kill-line-left)
    (keymap-set map "C-'" 'kill-line)
    (keymap-set map "M-'" 'kill-whole-line)
    
    (keymap-set map "M-/" 'delete-horizontal-space)
    
    (keymap-set map "C-v" 'erase-buffer)
    (keymap-set map "C-c C-d" 'martin-delete-pair-smart)

    ;; Files

    (keymap-set map "C-f" 'find-file)
    (keymap-set map "C-b" 'switch-to-buffer)

    ;; Windows managment

    (keymap-set map "M-o" 'other-window)
    (keymap-set map "C-M-o" 'martin-previous-window)
    (keymap-set map "C-0" 'delete-window)
    (keymap-set map "C-1" 'delete-other-windows)
    (keymap-set map "M-i" 'delete-other-windows)
    (keymap-set map "C-2" 'split-window-below)
    (keymap-set map "C-3" 'split-window-right)

    ;; Selection

    (keymap-set map "C-M-v" 'martin-select-buffer)
    (keymap-set map "M-SPC" 'er/expand-region)
    (keymap-set map "C-M-SPC" 'er/contract-region)


    ;; (keymap-set map "<up>" 'swap-line-up)
    ;; (keymap-set map "<down>" 'swap-line-down)

    ;; Copying

    (keymap-set map "C-M-'" 'martin-copy-line)
    (keymap-set map "M-v" 'martin-copy-buffer)

    ;; Down and Up cases
    
    (keymap-set map "C-u" 'martin-downcase-whole-word)
    (keymap-set map "M-u" 'martin-upcase-whole-word)
    (keymap-set map "C-M-u" 'martin-capitalize-whole-word)

    ;; File editting

    (keymap-set map "C-<tab>" 'indent-rigidly)
    (keymap-set map "C-M-<tab>" 'martin-force-tab-insert)
    (keymap-set map "M-S-SPC" 'martin-insert-space)
    (keymap-set map "C-r" 'query-replace)
    (keymap-set map "M-r" 'replace-string)
    (keymap-set map "C-." 'comment-line)
    (keymap-set map "C-z" 'save-buffer)
    
    ;; Zooming
    
    (keymap-set map "C-=" 'text-scale-increase)
    (keymap-set map "C--" 'text-scale-decrease)

    ;; Projectile

    (keymap-set map "C-c C-f" 'projectile-find-file)
    (keymap-set map "C-M-r" 'projectile-replace)
    (keymap-set map "C-M-g" 'projectile-grep)
    (keymap-set map "M-e" 'projectile-switch-project)

    ;; Multi Cursors

    (keymap-set map  "C-S-k" 'mc/mark-next-like-this)
    (keymap-set map  "C-S-l" 'mc/mark-previous-like-this)
    (keymap-set map  "C-:" 'mc/mark-next-like-this)
    (keymap-set map  "C-S-j" 'mc/mark-previous-like-this)
    (keymap-set map  "M-<left>" 'mc/mark-all-like-this)
    (keymap-set map  "M-<down>" 'set-rectangular-region-anchor)

    ;; Vterm
    (keymap-set map "C-c t" 'martin-vterm-force-other-window)
    (keymap-set map "C-c n" 'martin-vterm-force-new)
    
    (keymap-set map "C-c C-t" 'martin-vterm-other-window)
    (keymap-set map "C-c C-n" 'martin-vterm-new)
    (keymap-set map "C-t" 'martin-vterm-copy-mode)

    ;; Magit
    
    (keymap-set map "C-x C-g" 'magit-status)

    ;; Org

    (keymap-set map "C-n" 'martin-org-todo)
    (keymap-set map "M-n" 'martin-org-insert-heading)
    
    (keymap-set map "M-q" 'org-agenda-list)
    (keymap-set map "M-t" 'org-todo-list)

    ;; Search

    (keymap-set map "C-s" 'isearch-forward)
    (keymap-set map "M-s" 'isearch-backward)

    ;; Yasnippet

    (keymap-set map "<return>" 'yas-expand)
    
    map)
  "The keymap containing all my keybindings.")

(define-minor-mode martin-mode
  "Martin mode defines all the keybindings using `martin-mode-keymap`."
  :global t
  :keymap martin-mode-keymap
  :group 'martin)

(martin-mode 1)

(provide 'init-martin-mode)
;;; init-martin-mode.el ends here
