;;; init-elpa.el --- Package manager configuration -*- lexical-binding: t; -*-

;;; Commentary:
;; Initialize the package manager and ensure `use-package` is installed.

;;; Code:

(require 'package)

(setq package-archives
      '(("melpa" . "https://melpa.org/packages/")
        ("gnu"   . "https://elpa.gnu.org/packages/")))

(setq package-enable-at-startup nil)

;; Ensure use-package is installed once
(unless (package-installed-p 'use-package)
  (package-install 'use-package))

(require 'use-package)

;; Automatically `:ensure t` for all packages
(setq use-package-always-ensure t)

(provide 'init-elpa)
;;; init-elpa.el ends here
