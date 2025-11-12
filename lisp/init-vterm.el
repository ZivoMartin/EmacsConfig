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

;; --- Find the labtop screen -----------------
(defun my/laptop-monitor-attrs ()
  "Return labtop screen attributes."
  (seq-find (lambda (attrs)
              (when-let ((name (alist-get 'name attrs)))
                (string-match-p "eDP" name)))
            (display-monitor-attributes-list)))

(defun my/make-frame-on-monitor (attrs &optional params)
  "Create a new frame positioned on a monitor described by ATTRS.

ATTRS is an alist of monitor attributes, as returned by
`display-monitor-attributes-list'.  Optional PARAMS is an alist of
additional frame parameters to merge with the defaults."
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
