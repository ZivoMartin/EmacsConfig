;;; init-company-mode.el --- Company-mode autocompletion setup -*- lexical-binding: t; -*-

;;; Commentary:
;; Configure `company-mode` for autocompletion in programming and text modes.
;; This sets a low delay, minimum prefix length, and enables global mode.

;;; Code:

(require 'init-elpa)

(use-package company
  :defer 1
  :hook
  ((prog-mode . company-mode)
   (text-mode . company-mode))
  :init
  (setq company-idle-delay 0.1
        company-minimum-prefix-length 1
        company-tooltip-align-annotations t
        company-show-numbers t
        company-dabbrev-ignore-case t
        company-dabbrev-downcase nil)
  :config
  (global-company-mode 1))

(provide 'init-company-mode)
;;; init-company-mode.el ends here
