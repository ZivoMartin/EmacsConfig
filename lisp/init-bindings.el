;;; init-bindings.el --- Custom key bindings as an activatable minor mode -*- lexical-binding: t; -*-

;;; Commentary:
;; This rewrites the original global keybindings into a toggleable minor mode.
;; Enable per buffer with:
;;   (martin-mode 1)
;; Or globally with:
;;   (martin-global-mode 1)

;;; Code:

(eval-when-compile (require 'cl-lib))
(require 'subword)
(require 'org)

;;;; Helpers & state ---------------------------------------------------------

(defvar-local martin--subword-was-active nil
  "Whether `subword-mode' was active before enabling `martin-mode'.")

;;;; Swapping & misc commands ------------------------------------------------

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

(defun my-scroll-up ()
  "Scroll up 3 lines."
  (interactive)
  (scroll-up-line 3))

(defun my-scroll-down ()
  "Scroll down 3 lines."
  (interactive)
  (scroll-down-line 3))

(defun my/insert-space-no-move ()
  "Insert a space at point without moving point after insertion."
  (interactive)
  (save-excursion (insert " ")))

(defun toggle-comment-on-line ()
  "Comment or uncomment current line."
  (interactive)
  (comment-or-uncomment-region (line-beginning-position)
                               (line-end-position)))

(defun my/eglot-keybindings ()
  "Custom keybindings for Eglot-enabled buffers."
  (local-set-key (kbd "C-<return>") #'xref-find-definitions)
  (local-set-key (kbd "C-c C-b")    #'xref-go-back))

(add-hook 'eglot-managed-mode-hook #'my/eglot-keybindings)

;;;; Keymap -----------------------------------------------------------------

(defvar martin-mode-map
  (let ((map (make-sparse-keymap)))
    ;; Movements
    (define-key map (kbd "C-h") #'backward-char)
    (define-key map (kbd "C-l") #'forward-char)
    (define-key map (kbd "C-k") #'next-line)
    (define-key map (kbd "C-j") #'previous-line)

    (define-key map (kbd "M-h") #'subword-backward)
    (define-key map (kbd "M-l") #'subword-forward)
    (define-key map (kbd "M-j") #'backward-paragraph)
    (define-key map (kbd "M-k") #'forward-paragraph)

    ;; Zoom
    (define-key map (kbd "C-+") #'text-scale-increase)

    ;; Saving
    (define-key map (kbd "C-z") #'save-buffer)
    (define-key map (kbd "C-x s") #'ignore)
    (define-key map (kbd "C-x C-s") #'ignore)

    ;; Goto
    (define-key map (kbd "M-g M-g") #'goto-line)
    (define-key map (kbd "M-g M-f") #'move-to-column)

    ;; Delete overrides -> ignore
    (dolist (key '("DEL" "C-DEL" "M-DEL"
                   "<deletechar>" "<backspace>"
                   "C-<deletechar>" "M-<deletechar>"
                   "C-<backspace>"  "M-<backspace>"))
      (define-key map (kbd key) #'ignore))

    (define-key map (kbd "C-p")   #'delete-backward-char)
    (define-key map (kbd "M-p")   #'subword-backward-kill)
    (define-key map (kbd "C-S-p") #'backward-kill-word)

    (define-key map (kbd "C-ù") #'kill-line)
    (define-key map (kbd "M-ù") #'kill-whole-line)

    ;; Caps mapping (might be system/TTY dependent)
    (define-key map (kbd "<capslock>") #'event-apply-control-modifier)

    ;; Window/frame
    (define-key map (kbd "<return>")
                (if (> (length (display-monitor-attributes-list)) 1)
                    #'other-frame
                  #'other-window))
    (define-key map (kbd "s-o") #'other-window)
    (define-key map (kbd "C-&") #'delete-window)
    (define-key map (kbd "C-é") #'split-window-right)
    (define-key map (kbd "C-\"") #'split-window-below)
    (define-key map (kbd "C-²") #'delete-other-windows)

    ;; Removing old windows bindings

    (define-key map (kbd "C-x 0") #'ignore)
    (define-key map (kbd "C-x 1") #'ignore)
    (define-key map (kbd "C-x 2") #'ignore)
    (define-key map (kbd "C-x 3") #'ignore)

    ;; Arrows -> swap things
    (define-key map (kbd "<up>")    #'swap-line-up)
    (define-key map (kbd "<down>")  #'swap-line-down)
    (define-key map (kbd "<left>")  #'swap-word-left)
    (define-key map (kbd "<right>") #'swap-word-right)

    ;; Scrolling & buffer nav (super-based)
    (define-key map (kbd "s-k") #'my-scroll-up)
    (define-key map (kbd "s-j") #'my-scroll-down)
    (define-key map (kbd "s-h") #'previous-buffer)
    (define-key map (kbd "s-l") #'next-buffer)


    (define-key map (kbd "C-S-SPC") #'my/insert-space-no-move)
    (define-key map (kbd "C-,")     #'recenter-top-bottom)
    (define-key map (kbd "C-c h")   #'help-command)
    (define-key map (kbd "C-%")     #'query-replace)

    ;; Vterm
    (declare-function my/vterm-split-right "init-vterm")
    (declare-function my/vterm-new-buffer "init-vterm")
    (declare-function my/open-emacs-on-laptop "init-vterm")

    (define-key map (kbd "C-c t") #'my/vterm-split-right)
    (define-key map (kbd "C-c n") #'my/vterm-new-buffer)
    (define-key map (kbd "C-c C-t") #'my/open-emacs-on-laptop)

    ;; Projectile
    (define-key map (kbd "C-f") #'find-file)
    (define-key map (kbd "C-b") #'switch-to-buffer)

    (define-key map (kbd "C-c p p") #'projectile-switch-project)
    (define-key map (kbd "C-c p g") #'projectile-grep)
    (define-key map (kbd "C-c p r") #'projectile-replace)
    (define-key map (kbd "C-c p s") #'projectile-ripgrep)
    (define-key map (kbd "C-c p b") #'projectile-switch-to-buffer)
    (define-key map (kbd "C-c p k") #'projectile-kill-buffers)

    ;; Org
    (define-key map (kbd "C-!") #'org-todo)
    (define-key map (kbd "M-a") #'org-agenda-list)
    (define-key map (kbd "M-t") #'org-todo-list)

    ;; region from bol to previous line
    (define-key map (kbd "C-x p")
      (lambda ()
        (interactive)
        (execute-kbd-macro (kbd "C-SPC C-a C-p"))))

    (define-key map (kbd "C-x C-p")
      (lambda ()
        (interactive)
        (execute-kbd-macro (kbd "C-SPC C-a C-p"))))

    ;; Line commenting
    (define-key map (kbd "C-;") #'toggle-comment-on-line)

    map)
  "Keymap for `martin-mode'.")

;;;; Minor modes -------------------------------------------------------------

;;;###autoload
(define-minor-mode martin-mode
  "Toggle personal keybindings in the current buffer.
When enabled, installs a set of custom keybindings.
It also turns on `subword-mode' for the buffer and remembers/restores its prior
state when disabling."
  :init-value nil
  :lighter " ⌨"
  :keymap martin-mode-map
  (if martin-mode
      ;; Enabling
      (progn
        (when (fboundp 'subword-mode)
          (setq martin--subword-was-active (bound-and-true-p subword-mode))
          (subword-mode 1)))
    ;; Disabling
    (when (and (fboundp 'subword-mode)
               (not martin--subword-was-active))
      (subword-mode 0))))

;;;###autoload
(define-globalized-minor-mode martin-global-mode
  martin-mode
  (lambda () (martin-mode 1))
  :group 'convenience)

(martin-global-mode 1)

(provide 'init-bindings)
;;; init-bindings.el ends here
