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
(defun my/toggle-comment-on-line ()
  "Comment or uncomment region, or current line if no region is active."
  (interactive)
  (if (use-region-p)
      ;; Use region
      (comment-or-uncomment-region (region-beginning) (region-end))
    ;; Fallback to current line
    (comment-or-uncomment-region
     (line-beginning-position)
     (line-beginning-position 2))))

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

(defun my/org-or-nothing (f)
  "Return a command that calls F if in Org mode, or does nothing otherwise."
  (lambda ()
    (interactive)
    (when (derived-mode-p 'org-mode)
      (funcall f))))

(defun my-beginning-of-line ()
  "Move to the first non-whitespace character."
  (interactive)
  (let ((pos (point)))
    (back-to-indentation)
    (when (= pos (point))
      (beginning-of-line))))

(declare-function my/vterm-find-file "init-vterm")

(defun my/find-file ()
  "Call `my/vterm-find-file` if in vterm, else `find-file`."
  (interactive)
  (if (derived-mode-p 'vterm-mode)
      (my/vterm-find-file)
    (call-interactively #'find-file)))

;;;; Keymap -----------------------------------------------------------------

(defvar martin-mode-map
  (let ((map (make-sparse-keymap)))

    ;; --------------------
    ;; Movements (conflicts handled)
    ;; --------------------
    (define-key map (kbd "C-;")
                (my/vterm-dispatch #'forward-char "<right>"))
    (define-key map (kbd "C-j")
                (my/vterm-dispatch #'backward-char "<left>"))
    (define-key map (kbd "C-k") #'next-line)
    (define-key map (kbd "C-l") #'previous-line)

    (define-key map (kbd "M-j")
                (my/vterm-dispatch #'subword-backward "<left>" nil nil t))
    (define-key map (kbd "M-;")
                (my/vterm-dispatch #'subword-forward "<right>" nil nil t))
    (define-key map (kbd "M-l") #'backward-paragraph)
    (define-key map (kbd "M-k") #'forward-paragraph)

    (define-key map (kbd "C-M-;") #'forward-sexp)
    (define-key map (kbd "C-M-j") #'backward-sexp)

    ;; --------------------
    ;; Selections
    ;; --------------------

    ;; Character-level selection
    (define-key map (kbd "C-:")
                (my/vterm-dispatch
                 (lambda ()
                   (interactive)
                   (unless (region-active-p)
                     (set-mark (point)))
                   (forward-char))
                 "right" t))

    (define-key map (kbd "C-S-j")
                (my/vterm-dispatch
                 (lambda ()
                   (interactive)
                   (unless (region-active-p)
                     (set-mark (point)))
                   (backward-char))
                 "left" t))

    ;; Line-level selection
    (define-key map (kbd "C-S-k")
                (my/vterm-dispatch
                 (lambda ()
                   (interactive)
                   (unless (region-active-p)
                     (set-mark (point)))
                   (next-line))
                 "down" t))

    (define-key map (kbd "C-S-l")
                (my/vterm-dispatch
                 (lambda ()
                   (interactive)
                   (unless (region-active-p)
                     (set-mark (point)))
                   (previous-line))
                 "up" t))

    ;; Subword-level selection
    (define-key map (kbd "M-S-j")
                (my/vterm-dispatch
                 (lambda ()
                   (interactive)
                   (unless (region-active-p)
                     (set-mark (point)))
                   (subword-backward))
                 "left" t))

    (define-key map (kbd "M-:")
                (my/vterm-dispatch
                 (lambda ()
                   (interactive)
                   (unless (region-active-p)
                     (set-mark (point)))
                   (subword-forward))
                 "right" t))

    ;; Paragraph-level selection
    (define-key map (kbd "M-S-l")
                (my/vterm-dispatch
                 (lambda ()
                   (interactive)
                   (unless (region-active-p)
                     (set-mark (point)))
                   (backward-paragraph))
                 "up" t))

    (define-key map (kbd "M-S-k")
                (my/vterm-dispatch
                 (lambda ()
                   (interactive)
                   (unless (region-active-p)
                     (set-mark (point)))
                   (forward-paragraph))
                 "down" t))

    (global-set-key (kbd "s-q") #'my-beginning-of-line)

    ;; Upper / lower
    (define-key map (kbd "C-u") #'downcase-region)
    (define-key map (kbd "M-u") #'upcase-region)
    (define-key map (kbd "C-M-u") #'capitalize-region)

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
    (define-key map (kbd "C-<") #'beginning-of-buffer)
    (define-key map (kbd "C->") #'end-of-buffer)

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
    (define-key map (kbd "C-'")
                (my/vterm-dispatch #'kill-line "k" nil nil t))
    (define-key map (kbd "M-'") #'kill-whole-line)

    ;; Caps mapping
    (define-key map (kbd "<capslock>") #'event-apply-control-modifier)

    ;; Window/frame (conflicts handled)
    (define-key map (kbd "<return>")
                (if (> (length (display-monitor-attributes-list)) 1)
                    #'other-frame
                  #'other-window))
    
    (define-key map (kbd "M-o") #'other-window)
    (define-key map (kbd "C-1") #'delete-window)
    (define-key map (kbd "C-2") #'split-window-right)
    (define-key map (kbd "C-3") #'split-window-below)
    (define-key map (kbd "C-<escape>") #'delete-other-windows)

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

    ;; Scrolling & buffer nav
    (define-key map (kbd "s-k") #'my-scroll-up)
    (define-key map (kbd "s-l") #'my-scroll-down)
    (define-key map (kbd "M-r") #'previous-buffer)
    (define-key map (kbd "C-r") #'next-buffer)

    (define-key map (kbd "C-S-SPC") #'my/insert-space-no-move)
    (define-key map (kbd "C-,")     #'recenter-top-bottom)
    (define-key map (kbd "C-c h")   #'help-command)
    (define-key map (kbd "C-\"")     #'query-replace)

    ;; Vterm management
    (declare-function my/vterm-split-right "init-vterm")
    (declare-function my/vterm-new-buffer "init-vterm")
    (declare-function my/open-emacs-on-laptop "init-vterm")
    (define-key map (kbd "C-c t") #'my/vterm-split-right)
    (define-key map (kbd "C-c n") #'my/vterm-new-buffer)
    (define-key map (kbd "C-c C-t") #'my/open-emacs-on-laptop)
    (define-key map (kbd "C-t") #'vterm-copy-mode)

    (define-key map (kbd "C-f") #'my/find-file)
    (define-key map (kbd "C-x f") #'projectile-find-file)

    (define-key map (kbd "C-b") #'switch-to-buffer)
    (define-key map (kbd "C-x C-f") #'my/find-file)
    (define-key map (kbd "C-9") #'rename-file)

    ;; Projectile
    (define-key map (kbd "s-g") #'projectile-grep)
    (define-key map (kbd "M-e") #'projectile-switch-project)

    ;; Org
    (define-key map (kbd "C-n") (my/org-or-nothing #'org-todo))
    (define-key map (kbd "M-n") (my/org-or-nothing #'org-insert-heading))
    
    (define-key map (kbd "M-q") #'org-agenda-list)
    (define-key map (kbd "M-t") #'org-todo-list)


    ;; Magit
    (define-key map (kbd "M-g M-h") #'magit-status)
    (global-set-key (kbd "C-c C-s") #'magit-stash)
    (declare-function my-magit-diff-file "init-magit")
    (global-set-key (kbd "C-c C-d") #'my-magit-diff-file)

    ;; Multi-cursor
    (define-key map (kbd "C-<left>")  #'mc/edit-lines)
    (define-key map (kbd "C-<right>")  #'mc/edit-ends-of-lines)
    (define-key map (kbd "M-<left>")  #'mc/edit-beginnings-of-lines)
    (define-key map (kbd "C-<down>")  #'mc/mark-next-like-this)
    (define-key map (kbd "C-<up>")  #'mc/mark-previous-like-this)
    (define-key map (kbd "M-<right>")  #'mc/mark-all-like-this)
    (define-key map (kbd "M-<down>")  #'set-rectangular-region-anchor)
    (define-key map (kbd "M-<up>")  #'mc/mark-more-like-this-extended)

    ;; Region helpers
    (dolist (k '("C-x p" "C-x C-p"))
      (define-key map (kbd k)
                  (lambda () (interactive)
                    (execute-kbd-macro (kbd "C-SPC C-a C-p")))))

    ;; Commenting
    (define-key map (kbd "C-.") #'my/toggle-comment-on-line)
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
