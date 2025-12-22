;;; init-company.el --- Defines all the company preferences -*- lexical-bindings: t; -*-

;;; Commentary:

;;; Code:

(use-package company
  :demand t
  :config
  (global-company-mode 1)
  (let ((map company-active-map))
    (keymap-set map "C-p" nil)
    (keymap-set map "M-p" nil)
    (keymap-set map "M-n" 'company-select-previous-or-abort)))



(provide 'init-company)
;;; init-company.el ends here
