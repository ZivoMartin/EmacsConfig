;;; init-minibuffer.el --- Defines all the minibuffer preferences -*- lexical-bindings: t; -*-

;;; Commentary:

;;; Code:

(defvar martin-minibuffer-override-mode-map
  (let ((map (make-sparse-keymap)))
    (keymap-set map "C-l" 'previous-line-or-history-element)
    (keymap-set map "C-k" 'next-line-or-history-element)
    (keymap-set map "M-l" 'scroll-up-command)
    (keymap-set map "M-k" 'scroll-down-command)
    map)
  "Overriding `martin-mode`.")

(define-minor-mode martin-minibuffer-override-mode
  "Emulation layer overriding `martin-mode` in minibuffers."
  :group martin)

(defvar martin-minibuffer-override--emulation-alist
  `((martin-minibuffer-override-mode . ,martin-minibuffer-override-mode-map))
  "Entry is the `emulation-mode-map-alist`.")

(defun martin-enable-minibuffer-override ()
  "Activate the overwrite mode as an emulation keymap."
  (martin-minibuffer-override-mode 1)
  (add-to-list 'emulation-mode-map-alists
               'martin-minibuffer-override--emulation-alist))

(add-hook 'minibuffer-mode-hook #'martin-enable-minibuffer-override)

(provide 'init-minibuffer)
;;; init-minibuffer.el ends here
