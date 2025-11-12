;;; init-elpa.el --- Package manager configuration -*- lexical-binding: t; -*-

;;; Commentary:
;; Initialize the package manager and ensure `use-package` is installed.

;;; Code:

(require 'package)

(setq package-archives
      '(("melpa" . "https://melpa.org/packages/")
        ("gnu"   . "https://elpa.gnu.org/packages/")))

;; Only refresh if we’ve never done it before
(unless package-archive-contents
  (package-refresh-contents))

;; Ensure use-package is installed once
(unless (package-installed-p 'use-package)
  (package-install 'use-package))

(require 'use-package)
(setq use-package-always-ensure nil) 

(provide 'init-elpa)
;;; init-elpa.el ends here
