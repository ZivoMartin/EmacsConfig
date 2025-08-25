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

(with-eval-after-load 'company
  ;; Make C-p delete one char even when the popup is active
  (define-key company-active-map (kbd "C-p")
    (lambda () (interactive)
      (company-abort)
      (call-interactively #'delete-backward-char)))

  ;; Make M-p delete a subword even when the popup is active
  (define-key company-active-map (kbd "M-p")
    (lambda () (interactive)
      (company-abort)
      (call-interactively #'subword-backward-kill)))

  ;; Navigate candidates with C-j / C-k
  (define-key company-active-map (kbd "C-k") #'company-select-next)
  (define-key company-active-map (kbd "C-j") #'company-select-previous)

  ;; Keep your variant too
  (define-key company-active-map (kbd "C-S-p")
    (lambda () (interactive)
      (company-abort)
      (call-interactively #'backward-kill-word))))



(provide 'init-company-mode)
;;; init-company-mode.el ends here
