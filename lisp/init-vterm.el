;;; init-vterm.el --- VTerm configuration and helpers -*- lexical-binding: t; -*-

;;; Commentary:
;; - Split terminal to the right (C-c t)
;; - Open a new vterm buffer (C-c n)
;; - Override vterm key handling to match my global bindings by sending
;;   terminal keys where direct buffer editing would fail (read-only).

;;; Code:

;; Open a vterm in a right split
(defun my/vterm-split-right ()
  "Split the window to the right and open a new vterm."
  (interactive)
  (when (split-window-right)
    (other-window 1)
    (vterm)))

; Open a uniquely named vterm buffer
(defun my/vterm-new-buffer ()
  "Open a new vterm buffer with a unique name."
  (interactive)
  (let ((buffer (generate-new-buffer-name "vterm")))
    (vterm buffer)))

;; --- Find the labtop screen -----------------
(defun my/laptop-monitor-attrs ()
  "Return labtop screen attributes."
  (seq-find (lambda (attrs)
              (when-let ((name (alist-get 'name attrs)))
                (string-match-p "eDP" name)))
            (display-monitor-attributes-list)))

(defun my/make-frame-on-monitor (attrs &optional params)
  "Create a frame on monitor attributes attrs.
ATTRS are the attributes of the laptop screen."
  (let* ((geom (alist-get 'geometry attrs))
         (left (nth 0 geom))
         (top  (nth 1 geom)))
    (make-frame (append `((left . ,left)
                          (top  . ,top)
                          (fullscreen . maximized))
                        params))))

(defun my/open-emacs-on-laptop ()
  "Open Emacs frame with vterm inside on labtop."
  (interactive)
  (let* ((attrs (or (my/laptop-monitor-attrs)
                    (car (display-monitor-attributes-list))))
         (frame (my/make-frame-on-monitor attrs '((name . "laptop")))))
    (select-frame-set-input-focus frame)
    (when (require 'vterm nil t)
      (vterm))))

(use-package vterm
  :ensure t
  :demand t
  :commands (vterm)
  :init
  ;; Let Emacs/vterm see these keys (remove from vterm's exceptions)
  (with-eval-after-load 'vterm
    (dolist (key '("C-h" "C-l" "<return>" "C-p" "M-p" "C-S-p"
                   "M-h" "M-l" "C-ù" "M-ù" "S-<return>"))
      (setq vterm-keymap-exceptions (delete key vterm-keymap-exceptions))))
  :config
  ;; ---------------------------------------------------------------------------
  ;; Emulation map that overrides ALL minor modes (including martin-mode)
  ;; ---------------------------------------------------------------------------

  ;; Buffer-local flag to enable our emulation map in vterm buffers only.
  (defvar-local my/vterm-override-active nil
    "When non-nil, enable `my/vterm-override-map` via `emulation-mode-map-alists`.")

  ;; The actual override keymap used in vterm buffers.
  (defvar my/vterm-override-map
    (let ((map (make-sparse-keymap)))
      ;; Movement
      (define-key map (kbd "C-h") #'vterm-send-left)
      (define-key map (kbd "C-l") #'vterm-send-right)

      ;; Subword / paragraph movement (send to shell)
      (define-key map (kbd "M-h") #'vterm-send-M-b)
      (define-key map (kbd "M-l") #'vterm-send-M-f)

      (define-key map (kbd "<up>")   #'vterm-send-up)
      (define-key map (kbd "<down>") #'vterm-send-down)
      (define-key map (kbd "<right>") #'vterm-send-right)
      (define-key map (kbd "<left>") #'vterm-send-left)
      
      ;; Deletion
      (define-key map (kbd "C-p")   #'vterm-send-backspace) ; delete char backward
      (define-key map (kbd "M-p")   (lambda () (interactive) (vterm-send-key "<backspace>" nil t))) ; M-BS (shell-level)
      (define-key map (kbd "C-S-p") #'vterm-send-C-w) ; delete word backward (shell)

      ;; Kill lines in shell
      (define-key map (kbd "C-ù")   (lambda () (interactive) (vterm-send-key "C-k" t))) ; C-k
      (define-key map (kbd "M-ù")   (lambda () (interactive) (vterm-send-key "C-u" t))) ; C-u

      (define-key map (kbd "C-%") #'kill-line)

      ;; Enter: switch frame; Shift+Enter sends RET to shell
      (define-key map (kbd "<return>")   #'other-frame)
      (define-key map (kbd "S-<return>") #'vterm-send-return)
      map)
    "Keymap that should override minor modes inside `vterm-mode` buffers.")

  ;; Install our emulation map once. This gives it higher precedence than minor modes.
  (add-to-list 'emulation-mode-map-alists
               `((my/vterm-override-active . ,my/vterm-override-map)))

  ;; Keep your extra vterm tweaks here.
  (defun my/vterm-setup-keybindings ()
    "Per-buffer vterm setup. martin-mode stays on; these keys override it in vterm."
    ;; Activate the emulation map only in this vterm buffer.
    (setq-local my/vterm-override-active t)
    ;; Keep subword-mode active in copy-mode for consistency.
    (subword-mode 1))
  (add-hook 'vterm-mode-hook #'my/vterm-setup-keybindings))

(provide 'init-vterm)
;;; init-vterm.el ends here
