;;; init-bindings.el --- Custom key bindings configuration -*- lexical-binding: t; -*-

;;; Commentary:
;; Defines global keybindings for movement, editing, navigation, and search.
;; These override some default Emacs bindings to fit personal preferences.

;;; Code:

;; Movements
(subword-mode 1)
(global-set-key (kbd "C-h") 'backward-char)
(global-set-key (kbd "C-l") 'forward-char)
(global-set-key (kbd "C-k") 'next-line)
(global-set-key (kbd "C-j") 'previous-line)

(global-set-key (kbd "M-h") 'subword-backward)
(global-set-key (kbd "M-l") 'subword-forward)
(global-set-key (kbd "M-j") 'backward-paragraph)
(global-set-key (kbd "M-k") 'forward-paragraph)

;; Delete 
(global-set-key (kbd "C-p") 'delete-backward-char)
(global-set-key (kbd "M-p") 'backward-kill-word)
(global-set-key (kbd "C-S-p") 'backward-kill-word)        
(global-set-key (kbd "C-ù") 'kill-line)
(global-set-key (kbd "M-ù") 'kill-whole-line)

;; Returns
(global-set-key (kbd "<return>") 'other-window) ;;

;; Arrows
(global-set-key (kbd "<up>")    'scroll-down-line)
(global-set-key (kbd "<down>")  'scroll-up-line)
(global-set-key (kbd "<left>")  'previous-buffer)
(global-set-key (kbd "<right>") 'next-buffer)

;; Searching
(global-set-key (kbd "C-M-s") 'isearch-forward-regexp)
(global-set-key (kbd "C-M-r") 'isearch-backward-regexp)
(global-set-key (kbd "C-s") 'isearch-forward)
(global-set-key (kbd "C-r") 'isearch-backward)

;; Others
(global-set-key (kbd "C-,") 'recenter-top-bottom)
(global-set-key (kbd "C-c h") 'help-command)
(global-set-key (kbd "C-%") 'query-replace)
(global-set-key (kbd "C-x DEL")
                (lambda ()
                  (interactive)
                  (execute-kbd-macro (kbd "C-SPC C-a DEL"))))


;; Line commenting
(defun toggle-comment-on-line ()
  "Comment or uncomment current line."
  (interactive)
  (comment-or-uncomment-region (line-beginning-position) (line-end-position)))
(global-set-key (kbd "C-;") 'toggle-comment-on-line)

;; eglot
(defun my/eglot-keybindings ()
  "Custom keybindings for Eglot-enabled buffers."
  (local-set-key (kbd "C-<return>") #'xref-find-definitions)
  (local-set-key (kbd "C-c C-b")    #'xref-pop-marker-stack))

(add-hook 'eglot-managed-mode-hook #'my/eglot-keybindings)


(provide 'init-bindings)
;;; init-bindings.el ends here
