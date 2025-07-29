;;; init-lisp.el --- Configuration for Lisp programming -*- lexical-binding: t; -*-

;;; Commentary:
;; Basic setup for Emacs Lisp development:
;; - Real-time syntax checking via Flymake
;; - Live function/variable documentation via Eldoc

;;; Code:

;;; Enable real-time syntax checking with Flymake (built-in)
(add-hook 'emacs-lisp-mode-hook #'flymake-mode)

;;; Show function signature and doc in minibuffer
(add-hook 'emacs-lisp-mode-hook #'eldoc-mode)

(provide 'init-lisp)
;;; init-lisp.el ends here
