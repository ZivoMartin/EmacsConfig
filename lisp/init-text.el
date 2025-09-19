;;; init-text.el --- Minimal English spelling for .txt/.org -*- lexical-binding: t; -*-

;;; Commentary:
;; Minimal setup for Spelling (Flyspell via Hunspell)

;;; Code:

(use-package flyspell :ensure t)

(defun martin/text-setup ()
  "Setup for prose buffers (.txt)."

  ;; Spelling
  (when (executable-find "hunspell")
    (setq-local ispell-program-name (executable-find "hunspell")
                ispell-dictionary "en_US")
    (flyspell-mode 1)))

(add-hook 'text-mode-hook #'martin/text-setup)

(setq flyspell-issue-message-flag nil
      flyspell-issue-welcome-flag nil)

(provide 'init-text)
;;; init-text.el ends here
