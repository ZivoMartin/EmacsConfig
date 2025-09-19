;;; init-company-mode.el --- Company-mode autocompletion setup -*- lexical-binding: t; -*-

;;; Commentary:
;; Configure `company-mode` and `yasnippet` for autocompletion.

;;; Code:

;; Company
(use-package company
  :ensure t
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

;; Yasnippet
(use-package yasnippet
  :ensure t
  :defer t
  :config
  (yas-global-mode 1))

(use-package yasnippet-snippets
  :ensure t
  :after yasnippet)

;; Extra keybindings with company
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

  ;; Alternative variant
  (define-key company-active-map (kbd "C-S-p")
    (lambda () (interactive)
      (company-abort)
      (call-interactively #'backward-kill-word)))

  ;; Enable yasnippet in company
  (with-eval-after-load 'yasnippet
    (defvar company-mode/enable-yas t
      "Enable yasnippet for all backends.")

    (defun company-mode/backend-with-yas (backend)
      (if (or (not company-mode/enable-yas)
              (and (listp backend) (member 'company-yasnippet backend)))
          backend
        (append (if (consp backend) backend (list backend))
                '(:with company-yasnippet))))

    (setq company-backends
          (mapcar #'company-mode/backend-with-yas company-backends))))

(use-package vertico
  :ensure t
  :init (vertico-mode))



(provide 'init-company-mode)
;;; init-company-mode.el ends here

