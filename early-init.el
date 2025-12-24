;;; early-init.el --- Simply extends the load-path with the lisp folder -*- lexical-binding: t; -*-

;;; Commentary:

;;; Code:

(add-to-list 'load-path (expand-file-name "lisp" user-emacs-directory))

(add-to-list
 'elisp-flymake-byte-compile-load-path
 (expand-file-name "lisp" (file-name-directory load-file-name)))

(provide 'early-init)
;;; early-init.el ends here
