;;; init-vterm.el -- Summary: config for vterm ;;; -*- lexical-binding: t; -*-

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


(use-package vterm
  :ensure t)

(require 'vterm)

(defun my/vterm-find-file ()
  "Run `find-file` starting from the current vterm directory."
  (interactive)
  (when (eq major-mode 'vterm-mode)
    (let* ((dir (vterm--get-pwd))
           (default-directory (or dir default-directory)))
      (call-interactively #'find-file))))

(provide 'init-vterm)
;;; init-vterm.el ends here
