;;; init-react.el --- Minimal React setup (highlighting, errors, format-on-save)

;;; Commentary:
;; This config enables:
;; - Syntax highlighting for JS/TS + JSX/TSX via major modes
;; - Error highlighting via Flycheck (ESLint/tsc if available)
;; - Auto-formatting on save via Prettier
;;
;; Requirements on your system:
;;   npm i -g prettier eslint    ; or use project-local binaries
;;
;; Load with: (load "/path/to/init-react.el")

;;; Code:


(dolist (pkg '(web-mode typescript-mode flycheck prettier-js))
  (unless (package-installed-p pkg)
    (package-install pkg)))

;; --- File associations & indentation ----------------------------------------
;; Use web-mode for React-y files (JSX/TSX). Typescript-mode for plain .ts.
(add-to-list 'auto-mode-alist '("\\.jsx\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.tsx\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.js\\'"  . web-mode))       ; JSX-friendly .js
(add-to-list 'auto-mode-alist '("\\.ts\\'"  . typescript-mode))

;; Treat .js files as JSX inside web-mode.
(setq web-mode-content-types-alist '(("jsx" . "\\.js\\'")))

;; Indentation defaults (2 spaces is common in React projects).
(setq web-mode-markup-indent-offset 2
      web-mode-css-indent-offset    2
      web-mode-code-indent-offset   2)
(add-hook 'typescript-mode-hook (lambda () (setq-local typescript-indent-level 2)))

;; --- Error highlighting (Flycheck) ------------------------------------------
;; Enable Flycheck globally; prefer ESLint for JS/TS when present.
(add-hook 'after-init-hook #'global-flycheck-mode)
(with-eval-after-load 'flycheck
  ;; Do not use JSHint; prefer ESLint.
  (setq-default flycheck-disabled-checkers '(javascript-jshint))
  ;; Make ESLint run in web-mode buffers (JSX/TSX).
  (flycheck-add-mode 'javascript-eslint 'web-mode))

;; --- Auto-format on save (Prettier) -----------------------------------------
(defun my/enable-prettier-on-save ()
  "Enable Prettier format-on-save if the 'prettier' binary exists."
  (when (executable-find "prettier")
    (prettier-js-mode 1)))

(add-hook 'web-mode-hook         #'my/enable-prettier-on-save)
(add-hook 'typescript-mode-hook  #'my/enable-prettier-on-save)
(add-hook 'js-mode-hook          #'my/enable-prettier-on-save) ; Just in case

(provide 'init-react)
;;; init-react.el ends here
