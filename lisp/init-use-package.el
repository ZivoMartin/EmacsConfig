;;; init-use-package.el --- Configure use-package -*- lexical-bindings: t; -*-

;;; Commentary:

;;; Code:

(require 'package)
(require 'use-package)

(setq package-archives
      '(("melpa" . "https://melpa.org/packages/")
        ("gnu"   . "https://elpa.gnu.org/packages/")))

(setq use-package-always-ensure t)
(setq use-package-always-demand t)

(unless (package-installed-p 'use-package)
  (package-install 'use-package))

(provide 'init-use-package)
;;; init-use-package.el ends here
