;;; init-elisp.el --- Minimal emacs lisp config -*- lexical-bindings: t; -*-

;;; Commentary:

;;; Code:

(add-hook 'emacs-lisp-mode-hook #'flymake-mode)
(add-hook 'emacs-lisp-mode-hook #'eldoc-mode)
(add-hook 'emacs-lisp-mode-hook #'checkdoc-minor-mode)

(provide 'init-elisp)
;;; init-elisp.el ends here
