;;; init-vterm.el --- Defines all the VTERM preferences -*- lexical-bindings: t; -*-

;;; Commentary:

;;; Code:

(require 'init-use-package)
(require 'init-martin-group)

(defvar vterm-path (expand-file-name "emacs-libvterm" manual-installation-directory))


(defun martin-vterm-backspace ()
  "Send backspace to vterm."
  (interactive)
  (message "heyyy")
  (vterm-send-key "<backspace>"))

(defun martin-vterm-meta-backspace ()
  "Send meta-backspace to vterm."
  (interactive)
  (vterm-send-key "<backspace>" nil t))

(defun martin-vterm-up ()
  "Send up to vterm."
  (interactive)
  (vterm-send-key "<up>"))

(defun martin-vterm-down ()
  "Send down to vterm."
  (interactive)
  (vterm-send-key "<down>"))

(defun martin-vterm-right ()
  "Send right to vterm."
  (interactive)
  (vterm-send-key "<right>"))

(defun martin-vterm-left ()
  "Send left to vterm."
  (interactive)
  (vterm-send-key "<left>"))

(defun martin-vterm-interrupt ()
  "Send left to vterm."
  (interactive)
  (vterm-send-key "c" nil nil t))

(defun martin-vterm-clear ()
  "Clear the vterm buffer."
  (interactive)
  (vterm-send-key "l" nil nil t))

(use-package vterm
  :load-path vterm-path)

(defvar martin-vterm-override-mode-map
  (let ((map (make-sparse-keymap)))
    (keymap-set map "C-p" #'martin-vterm-backspace)
    (keymap-set map "M-p" #'martin-vterm-meta-backspace)
    
    (keymap-set map "C-;" #'martin-vterm-right)
    (keymap-set map "C-l" #'martin-vterm-up)
    (keymap-set map "C-k" #'martin-vterm-down)
    (keymap-set map "C-j" #'martin-vterm-left)

    (keymap-set map "C-c" #'martin-vterm-interrupt)
    (keymap-set map "C-v" #'martin-vterm-clear)

    map)
  "Keymap for `martin-vterm-override-mode`.")

(defun martin-vterm-copy-mode ()
  "Activate or desactivate copy mode, does nothing if we are not inside a vterm buffer, does nothing."
  (when derived-mode-p 'vterm-mode
	(vterm-copy-mode (if (bound-and-true-p vterm-copy-mode)
			     (t) (-1)))))



(define-minor-mode martin-vterm-override-mode
  "Emulation layer overriding `martin-mode` in vterm buffers."
  :group martin)

(defvar martin-vterm-override--emulation-alist
  `((martin-vterm-override-mode . ,martin-vterm-override-mode-map))
  "Entry is the `emulation-mode-map-alist`.")

(defun martin-enable-vterm-override ()
  "Activate the overwrite mode as an emulation keymap."
  (martin-vterm-override-mode 1)
  (add-to-list 'emulation-mode-map-alists
               'martin-vterm-override--emulation-alist))

(add-hook 'vterm-mode-hook #'martin-enable-vterm-override)


(provide 'init-vterm)
;;; init-vterm.el ends here
