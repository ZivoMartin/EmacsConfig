;;; init-electric.el --- Configuration for electric pair mode -*- lexical-binding: t; -*-

;;; Commentary:
;; Enable and configure `electric-pair-mode` to insert matching delimiters
;; like (), [], "", etc. Also disables pairing in unwanted modes like shell
;; or minibuffer, and adds custom pairing behavior.

;;; Code:

;; Enable automatic insertion of matching pairs like (), {}, [], "", etc.
(electric-pair-mode 1)

;; Disable pairing in modes where it would be annoying, like minibuffer or shell
(setq electric-pair-inhibit-predicate
      (lambda (c)
        (or (char-equal c ?\')                     ; Don’t auto-pair single quotes
            (minibufferp)                          ; Disable in minibuffer
            (memq major-mode '(shell-mode eshell-mode)))))  ; Disable in shell modes

;; Prevent breaking a pair when deleting in the middle of one
(setq electric-pair-preserve-balance t)

;; Allow automatic pairing of additional characters (e.g., < and >)
(setq electric-pair-text-pairs '((?< . ?>)))

;; Do not insert a newline between pairs automatically
(setq electric-pair-open-newline-between-pairs nil)

(provide 'init-electric)
;;; init-electric.el ends here
