;;; init-bindings.el --- Custom key bindings as an activatable minor mode -*- lexical-binding: t; -*-

;;; Commentary:
;; This merges normal bindings and vterm bindings.
;; On conflict, dispatch based on whether we are in vterm-mode.

;;; Code:

(eval-when-compile (require 'cl-lib))
(require 'subword)
(require 'org)
(require 'vterm)

;;;; Helpers & state ---------------------------------------------------------

(defvar-local martin--subword-was-active nil
  "Whether `subword-mode' was active before enabling `martin-mode'.")

(defun swap-line-up ()
  "Swap current line with the one above."
  (interactive)
  (let ((col (current-column)))
    (transpose-lines 1)
    (forward-line -2)
    (move-to-column col)))

(defun swap-line-down ()
  "Swap current line with the one below."
  (interactive)
  (let ((col (current-column)))
    (forward-line 1)
    (transpose-lines 1)
    (forward-line -1)
    (move-to-column col)))

(defun swap-word-left ()
  "Swap current word with the one on the left."
  (interactive)
  (transpose-words -1))

(defun swap-word-right ()
  "Swap current word with the one on the right."
  (interactive)
  (transpose-words 1))

(defun my-scroll-up () (interactive) (scroll-up-line 3))
(defun my-scroll-down () (interactive) (scroll-down-line 3))
(defun my/insert-space-no-move () (interactive) (save-excursion (insert " ")))
(defun toggle-comment-on-line () (interactive)
  (comment-or-uncomment-region (line-beginning-position) (line-end-position)))

(defun my/eglot-keybindings ()
  "Custom keybindings for Eglot-enabled buffers."
  (local-set-key (kbd "C-<return>") #'xref-find-definitions)
  (local-set-key (kbd "C-c C-b")    #'xref-go-back))
(add-hook 'eglot-managed-mode-hook #'my/eglot-keybindings)

;; Helper for vterm binding dispatch
(defun my/vterm-dispatch (emacs-fn vterm-key &optional shift meta ctrl)
  "Call VTERM-KEY in vterm/vterm-copy-mode or EMACS-FN otherwise."
  (lambda ()
    (interactive)
    (if (and (derived-mode-p 'vterm-mode)
             (not (bound-and-true-p vterm-copy-mode)))
        (vterm-send-key vterm-key shift meta ctrl)
      (call-interactively emacs-fn))))

(defun my-beginning-of-line ()
  "Move to the first non-whitespace character."
  (interactive)
  (let ((pos (point)))
    (back-to-indentation)
    (when (= pos (point))
      (beginning-of-line))))

;;;; Keymap -----------------------------------------------------------------

(defvar martin-mode-map
  (let ((map (make-sparse-keymap)))
    ;; Movements (conflicts handled)
    (define-key map (kbd "C-h")
      (my/vterm-dispatch #'backward-char "<left>"))
    (define-key map (kbd "C-l")
      (my/vterm-dispatch #'forward-char "<right>"))
    (define-key map (kbd "C-k") #'next-line)
    (define-key map (kbd "C-j") #'previous-line)

    (define-key map (kbd "M-h")
      (my/vterm-dispatch #'subword-backward "<left>" nil nil t))
    (define-key map (kbd "M-l")
      (my/vterm-dispatch #'subword-forward "<right>" nil nil t))
    (define-key map (kbd "M-j") #'backward-paragraph)
    (define-key map (kbd "M-k") #'forward-paragraph)

    (define-key map (kbd "C-M-l") #'forward-sexp)
    (define-key map (kbd "C-M-h") #'backward-sexp)

    (global-set-key (kbd "s-a") #'my-beginning-of-line)

    ;; Zoom
    (define-key map (kbd "C-=") #'text-scale-increase)
    (define-key map (kbd "C--") #'text-scale-decrease)

    ;; Saving
    (define-key map (kbd "C-z") #'save-buffer)
    (define-key map (kbd "C-x s") #'ignore)
    (define-key map (kbd "C-x C-s") #'ignore)

    ;; Goto
    (define-key map (kbd "M-g M-g") #'goto-line)
    (define-key map (kbd "M-g M-f") #'move-to-column)

    ;; Deletion (conflicts handled)
    (dolist (key '("DEL" "C-DEL" "M-DEL"
                   "<deletechar>" "<backspace>"
                   "C-<deletechar>" "M-<deletechar>"
                   "C-<backspace>"  "M-<backspace>"))
      (define-key map (kbd key) #'ignore))

    (define-key map (kbd "C-p")
      (my/vterm-dispatch #'delete-backward-char "<backspace>"))
    (define-key map (kbd "M-p")
      (my/vterm-dispatch #'subword-backward-kill "w" nil nil t))
    (define-key map (kbd "C-S-p")
      (my/vterm-dispatch #'backward-kill-word "w" nil nil t))
    (define-key map (kbd "C-ù")
      (my/vterm-dispatch #'kill-line "k" nil nil t))
    (define-key map (kbd "M-ù") #'kill-whole-line)

    ;; Caps mapping
    (define-key map (kbd "<capslock>") #'event-apply-control-modifier)

    ;; Window/frame (conflicts handled)
    (define-key map (kbd "<return>")
       (if (> (length (display-monitor-attributes-list)) 1)
           #'other-frame
         #'other-window))
    
    (define-key map (kbd "M-o") #'other-window)
    (define-key map (kbd "C-&") #'delete-window)
    (define-key map (kbd "C-é") #'split-window-right)
    (define-key map (kbd "C-\"") #'split-window-below)
    (define-key map (kbd "C-²") #'delete-other-windows)

    ;; Remove old window bindings
    (dolist (k '("C-x 0" "C-x 1" "C-x 2" "C-x 3"))
      (define-key map (kbd k) #'ignore))

    ;; Arrows
    (define-key map (kbd "<up>")
      (my/vterm-dispatch #'swap-line-up "<up>" nil nil nil))
    (define-key map (kbd "<down>")
      (my/vterm-dispatch #'swap-line-down "<down>" nil nil nil))
    (define-key map (kbd "<left>")
       (my/vterm-dispatch #'undo-fu-only-undo "<left>" nil nil nil))
    (define-key map (kbd "<right>")
       (my/vterm-dispatch #'undo-fu-only-redo "<right>" nil nil nil))
    (define-key map (kbd "C-/")
                (lambda ()
                  (interactive)
                  (if (derived-mode-p 'vterm-mode)
                    (vterm-undo)
                    (ignore))))

    ;; Scrolling & buffer nav
    (define-key map (kbd "s-k") #'my-scroll-up)
    (define-key map (kbd "s-j") #'my-scroll-down)
    (define-key map (kbd "s-h") #'previous-buffer)
    (define-key map (kbd "s-l") #'next-buffer)

    (define-key map (kbd "C-S-SPC") #'my/insert-space-no-move)
    (define-key map (kbd "C-,")     #'recenter-top-bottom)
    (define-key map (kbd "C-c h")   #'help-command)
    (define-key map (kbd "C-%")     #'query-replace)

    ;; Vterm management
    (declare-function my/vterm-split-right "init-vterm")
    (declare-function my/vterm-new-buffer "init-vterm")
    (declare-function my/open-emacs-on-laptop "init-vterm")
    (define-key map (kbd "C-c t") #'my/vterm-split-right)
    (define-key map (kbd "C-c n") #'my/vterm-new-buffer)
    (define-key map (kbd "C-c C-t") #'my/open-emacs-on-laptop)
    (define-key map (kbd "C-t") #'vterm-copy-mode)

    ;; Projectile
    (define-key map (kbd "C-f") #'projectile-find-file)
    (define-key map (kbd "C-b") #'projectile-switch-to-buffer)
    (define-key map (kbd "s-g") #'projectile-grep)

    ;; Org
    (define-key map (kbd "C-!") #'org-todo)
    (define-key map (kbd "M-a") #'org-agenda-list)
    (define-key map (kbd "M-t") #'org-todo-list)

    ;; Region helpers
    (dolist (k '("C-x p" "C-x C-p"))
      (define-key map (kbd k)
        (lambda () (interactive)
          (execute-kbd-macro (kbd "C-SPC C-a C-p")))))

    ;; Commenting
    (define-key map (kbd "C-;") #'toggle-comment-on-line)
    map)
  "Keymap for `martin-mode'.")

;;;; Minor modes -------------------------------------------------------------

(define-minor-mode martin-mode
  "Toggle personal keybindings in the current buffer."
  :init-value nil
  :lighter " ⌨"
  :keymap martin-mode-map
  (if martin-mode
      (subword-mode 1)
    (subword-mode 0)))

(define-globalized-minor-mode martin-global-mode
  martin-mode
  (lambda () (martin-mode 1))
  :group 'convenience)

(martin-global-mode 1)

(provide 'init-bindings)
;;; init-bindings.el ends here
