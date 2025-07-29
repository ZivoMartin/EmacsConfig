;;; init-elpa.el --- Package manager configuration -*- lexical-binding: t; -*-

;;; Commentary:
;; Initialize the package manager and ensure `use-package` is installed.

;;; Code:

(require 'package)

;; Define the list of package repositories (association list of name . URL)
(setq package-archives
      '(("melpa" . "https://melpa.org/packages/")
        ("gnu"   . "https://elpa.gnu.org/packages/")))

;; Initialize the package system, but only if it hasn't already been initialized
(unless package--initialized
  (package-initialize))

;; Installing use-package if not already installed
(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))

(require 'use-package)

;; Always ensure packages are installed when using `use-package`
(setq use-package-always-ensure t)

(provide 'init-elpa)
;;; init-elpa.el ends here
