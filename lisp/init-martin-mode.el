;;; init-martin-mode.el --- Definition of the keybindings -*- lexical-bindings: t; -*-

;;; Commentary:

;;; Code:

(require 'init-utils)
(require 'subword)
(require 'init-vterm)
(require 'init-martin-group)
(require 'init-indents)
(require 'init-delete-pair)

(makunbound 'martin-mode-keymap)
(defvar martin-mode-keymap
  (let ((map (make-sparse-keymap)))
    
    ;; Cursor movments

    (keymap-set map "C-;" 'forward-char)
    (keymap-set map "C-j" 'backward-char)
    (keymap-set map "C-k" 'next-line)
    (keymap-set map "C-l" 'previous-line)

    (keymap-set map "M-;" 'subword-forward)
    (keymap-set map "M-j" 'subword-backward)
    (keymap-set map "M-k" 'forward-paragraph)
    (keymap-set map "M-l" 'backward-paragraph)

    (keymap-set map "C-e" 'end-of-line)
    (keymap-set map "C-a" 'beginning-of-line)

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
    (keymap-set map "C-x p" 'kill-line-left)
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
    ;; (use-package expand-region)
    ;; (require 'expand-region)
    ;; (define-key map (kbd "M-SPC") 'er/expand-region)
    ;; (define-key map (kbd "M-C-SPC") 'er/contract-region)


    ;; arrows
    ;;     (define-key map (kbd "<up>")
    ;;             (my/vterm-dispatch #'swap-line-up "<up>" nil nil nil))
    ;; (define-key map (kbd "<down>")
    ;;             (my/vterm-dispatch #'swap-line-down "<down>" nil nil nil))
    ;; (define-key map (kbd "<left>")
    ;;             (my/vterm-dispatch #'undo-fu-only-undo "<left>" nil nil nil))
    ;; (define-key map (kbd "<right>")
    ;;             (my/vterm-dispatch #'undo-fu-only-redo "<right>" nil nil nil))


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

    ;; Vterm

    (keymap-set map "C-c C-t" 'vterm-other-window)
    (keymap-set map "C-c C-n" 'vterm)
    (keymap-set map "C-t" 'martin-vterm-copy-mode)

    ;; Magit
    
    (keymap-set map "C-x C-g" 'magit-status)

    ;; Search

    (keymap-set map "C-s" 'isearch-forward)
    (keymap-set map "M-s" 'isearch-backward)
    
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
