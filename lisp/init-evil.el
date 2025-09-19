;;; init-evil.el --- Custom Evil-flavored key bindings as an activatable minor mode -*- lexical-binding: t; -*-

;;; Commentary:
;; Toggle per buffer with:
;;   (martin-mode 1)
;; Or globally with:
;;   (martin-global-mode 1)
;; Keys are Evil-native (mostly normal/visual). Insert stays mostly untouched.

;;; Code:

(eval-when-compile (require 'cl-lib))
(require 'subword)
(require 'org)
(require 'evil)
(evil-mode 1)

;; Optional integrations (load if present)
(with-eval-after-load 'projectile (require 'projectile))
(with-eval-after-load 'vterm (require 'vterm))
;; (with-eval-after-load 'evil-collection (evil-collection-init))

;;;; Minor mode & keymap ------------------------------------------------------

(defvar martin-mode-map (make-sparse-keymap)
  "Keymap for `martin-mode'.")

(define-minor-mode martin-mode
  "Martin's Evil-flavored keybindings."
  :lighter " 𝕄"
  :keymap martin-mode-map)

(define-globalized-minor-mode martin-global-mode martin-mode
  (lambda () (martin-mode 1)))

;;;; Helpers / misc commands --------------------------------------------------
;; NOTE: comments in English per your preference.

(defun swap-line-up ()
  "Swap current line with the one above, preserving column."
  (interactive)
  (let ((col (current-column)))
    (transpose-lines 1)
    (forward-line -2)
    (move-to-column col)))

(defun swap-line-down ()
  "Swap current line with the one below, preserving column."
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
  ;; Keep these non-Evil so they work in any state that respects C-*.
  (local-set-key (kbd "C-<return>") #'xref-find-definitions)
  (local-set-key (kbd "C-c C-b")    #'xref-go-back))
(add-hook 'eglot-managed-mode-hook #'my/eglot-keybindings)

;; vterm helpers (safe stubs; only run if vterm is available)
(defun my/vterm-split-right ()
  "Split window right and open vterm."
  (interactive)
  (if (featurep 'vterm)
      (progn (split-window-right) (other-window 1) (vterm))
    (user-error "vterm not available")))

(defun my/vterm-new-buffer ()
  "Create or switch to a new vterm buffer."
  (interactive)
  (if (featurep 'vterm)
      (vterm (generate-new-buffer-name "*vterm*"))
    (user-error "vterm not available")))

(defun my/open-emacs-on-laptop ()
  "Example placeholder command. Replace with your real action."
  (interactive)
  (message "Placeholder: open Emacs on laptop."))

;;;; Convenience macro for Evil bindings --------------------------------------

(defmacro martin--evil-keys (states keymap &rest pairs)
  "Bind PAIRS of KEY DEF in STATES for KEYMAP using `evil-define-key'."
  (declare (indent defun))
  `(progn
     ,@(cl-loop for (k d) on pairs by #'cddr
                collect `(evil-define-key ',states ,keymap (kbd ,k) ,d))))

;;;; Normal & Visual state bindings -------------------------------------------

(martin--evil-keys (normal visual) martin-mode-map
  ;; Window/frame navigation & management
  "<return>" (if (> (length (display-monitor-attributes-list)) 1)
                 #'other-frame
               #'other-window)
  "&"        #'delete-window
  "\""       #'split-window-below
  "é"        #'split-window-right
  "²"        #'delete-other-windows

  ;; Save buffer
  "z"        #'save-buffer

  ;; Scrolling / buffer cycling (super keys)
  "s-k"      #'my-scroll-down
  "s-j"      #'my-scroll-up
  "s-h"      #'previous-buffer
  "s-l"      #'next-buffer

  ;; Swap things with arrows
  "<up>"     #'swap-line-up
  "<down>"   #'swap-line-down
  "<left>"   #'swap-word-left
  "<right>"  #'swap-word-right

  ;; Word/paragraph motions (keep your M-* + C-*)
  "M-h"      #'subword-backward
  "M-l"      #'subword-forward
  "M-j"      #'forward-paragraph
  "M-k"      #'backward-paragraph

  "C-h"      #'backward-word
  "C-l"      #'forward-word
  "C-j"      #'forward-paragraph
  "C-k"      #'backward-paragraph

  ;; Recenter, help, replace
  ","        #'recenter-top-bottom
  "C-c h"    #'help-command
  "%"        #'query-replace

  ;; Projectile (bindings valid when projectile is loaded)
  "f"        #'find-file
  "b"        #'switch-to-buffer
  "C-c p p"  #'projectile-switch-project
  "C-c p g"  #'projectile-grep
  "C-c p r"  #'projectile-replace
  "C-c p s"  #'projectile-ripgrep
  "C-c p b"  #'projectile-switch-to-buffer
  "C-c p k"  #'projectile-kill-buffers

  ;; Vterm helpers
  "C-c t"    #'my/vterm-split-right
  "C-c n"    #'my/vterm-new-buffer
  "C-c C-t"  #'my/open-emacs-on-laptop

  ;; Org
  "!"        #'org-todo
  "M-a"      #'org-agenda-list
  "M-t"      #'org-todo-list

  ;; Goto
  "M-g M-g"  #'goto-line
  "M-g M-f"  #'move-to-column

  ;; Line comment toggle
  ";"        #'toggle-comment-on-line

  ;; Select from BOL to previous line (macro)
  "C-x p"    (lambda () (interactive) (execute-kbd-macro (kbd "C-SPC C-a C-p")))
  "C-x C-p"  (lambda () (interactive) (execute-kbd-macro (kbd "C-SPC C-a C-p")))

  ;; Custom space insert without moving point
  "C-S-SPC"  #'my/insert-space-no-move

  ;; French-keyboard kills
  "ù"        #'kill-line
  "C-ù"      #'kill-whole-line)

(define-key evil-insert-state-map (kbd "<return>") #'evil-normal-state)

(provide 'init-evil)
;;; init-evil.el ends here
