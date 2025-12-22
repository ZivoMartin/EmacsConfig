;;; init-vterm.el --- Defines all the VTERM preferences -*- lexical-bindings: t; -*-

;;; Commentary:

;;; Code:

(require 'init-use-package)
(require 'init-martin-group)

(defvar vterm-path (expand-file-name "emacs-libvterm" manual-installation-directory))

(defmacro martin-vterm-send-key (name key &rest flags)
  "Create a function with the name NAME sending KEY to vterm.
FLAGS can include :shift, :meta, :ctrl."
  (let ((shift (if (memq :shift flags) t nil))
        (meta  (if (memq :meta flags)  t nil))
        (ctrl  (if (or (memq :ctrl flags)
                       (memq :control flags))
                   t nil)))
    `(defun ,name ()
       ,(format "Send %s to vterm." key)
       (interactive)
       (vterm-send-key ,key ,shift ,meta ,ctrl))))

(martin-vterm-send-key martin-vterm-delete-char "d" :ctrl)
(martin-vterm-send-key martin-vterm-delete-word "d" :meta)
(martin-vterm-send-key martin-vterm-backspace "<backspace>")
(martin-vterm-send-key martin-vterm-meta-backspace "<backspace>" :meta)

(martin-vterm-send-key martin-vterm-up "<up>")
(martin-vterm-send-key martin-vterm-down "<down>")
(martin-vterm-send-key martin-vterm-right "<right>")
(martin-vterm-send-key martin-vterm-left "<left>")

(martin-vterm-send-key martin-vterm-control-up "<up>" :ctrl)
(martin-vterm-send-key martin-vterm-control-down "<down>" :ctrl)
(martin-vterm-send-key martin-vterm-control-right "<right>" :ctrl)
(martin-vterm-send-key martin-vterm-control-left "<left>" :ctrl)

(martin-vterm-send-key martin-vterm-interrupt "c" :ctrl)
(martin-vterm-send-key martin-vterm-clear "l" :ctrl)

(martin-vterm-send-key martin-vterm-beginning-of-line "a" :ctrl)
(martin-vterm-send-key martin-vterm-end-of-line "e" :ctrl)

(use-package vterm
  :load-path vterm-path)

(defvar martin-vterm-override-mode-map
  (let ((map (make-sparse-keymap)))
    (keymap-set map "C-p" #'martin-vterm-backspace)
    (keymap-set map "M-p" #'martin-vterm-meta-backspace)
    
    (keymap-set map "C-d" #'martin-vterm-delete-char)
    (keymap-set map "M-d" #'martin-vterm-delete-word)
    
    (keymap-set map "C-;" #'martin-vterm-right)
    (keymap-set map "C-l" #'martin-vterm-up)
    (keymap-set map "C-k" #'martin-vterm-down)
    (keymap-set map "C-j" #'martin-vterm-left)

    (keymap-set map "M-;" #'martin-vterm-control-right)
    (keymap-set map "M-l" #'martin-vterm-control-up)
    (keymap-set map "M-k" #'martin-vterm-control-down)
    (keymap-set map "M-j" #'martin-vterm-control-left)

    (keymap-set map "C-c C-c" #'martin-vterm-interrupt)
    (keymap-set map "C-v" #'martin-vterm-clear)

    (keymap-set map "C-a" #'martin-vterm-beginning-of-line)
    (keymap-set map "C-e" #'martin-vterm-end-of-line)

    map)
  "Keymap for `martin-vterm-override-mode`.")

(defun martin-vterm-copy-mode ()
  "Toggle vterm copy mode if in a vterm buffer."
  (interactive)
  (when (derived-mode-p 'vterm-mode)
    (vterm-copy-mode 'toggle)))

(defun martin-vterm-base-name (&optional name)
  "Return the base (non-starred) vterm name.
If NAME is nil, use the current buffer name."
  (let ((base (or name (buffer-name))))
    (if (string-prefix-p "vterm-" base)
        base
      (concat "vterm-" base))))

(defun martin-vterm-new (&optional force-create name)
  "Create a new vterm buffer in the current window.
If NAME is provided, use it as the base name.
FORCE-CREATE forces vterm to create a new buffer."
  (interactive)
  (let* ((vterm-buffer-name (martin-vterm-base-name name))
         (host-name (buffer-name))
         (buffer-exists (get-buffer vterm-buffer-name)))

    (if (and (not force-create) buffer-exists)
        (switch-to-buffer vterm-buffer-name)
      (vterm (generate-new-buffer-name vterm-buffer-name)))))

(defun martin-vterm-other-window (&optional force-create name)
  "Create a new vterm buffer in another window.
If NAME is provided, use it as the base name.
FORCE-CREATE forces vterm to create a new buffer."
  (interactive)
  (let ((win (split-window-right)))
    (set-window-buffer win (buffer-name))
    (other-window 1)
    (martin-vterm-new force-create name)))

(defun martin-vterm-force-new (&optional name)
  "Force the creation of a new vterm buffer with the name NAME on the current buffer."
  (interactive)
  (martin-vterm-new t name))

(defun martin-vterm-force-other-window (&optional name)
  "Force the creation of a new vterm buffer with the name NAME on another window."
  (interactive)
  (martin-vterm-other-window t name))

(define-minor-mode martin-vterm-override-mode
  "Emulation layer overriding `martin-mode` in vterm buffers."
  :group martin
  :init-value nil
  :lighter nil)

(defvar martin-vterm-override--emulation-alist
  `((martin-vterm-override-mode . ,martin-vterm-override-mode-map))
  "Entry is the `emulation-mode-map-alist`.")

(defun martin--vterm-enter ()
  "Enable vterm override when entering a vterm buffer."
  (unless vterm-copy-mode
    (martin-vterm-override-mode 1)))

(defun martin--vterm-handle-copy-mode ()
  "Enable or disable vterm override depending on copy mode."
  (if vterm-copy-mode
      (martin-vterm-override-mode -1)
    (martin-vterm-override-mode 1)))

(add-hook 'vterm-mode-hook #'martin--vterm-enter)
(add-hook 'vterm-copy-mode-hook #'martin--vterm-handle-copy-mode)

(unless (member 'martin-vterm-override--emulation-alist
                emulation-mode-map-alists)
  (push 'martin-vterm-override--emulation-alist
        emulation-mode-map-alists))

(provide 'init-vterm)
;;; init-vterm.el ends here
