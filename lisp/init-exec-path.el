;;; init-exec-path.el --- Import shell environment variables -*- lexical-binding: t; -*-

;;; Commentary:
;; Use `exec-path-from-shell` to import shell environment variables
;; when running Emacs in a GUI (macOS, Linux with X11, etc.).
;; This ensures tools like compilers and language servers work correctly.

;;; Code:

(use-package exec-path-from-shell
  :if (memq window-system '(mac ns x))  ;; Only apply in GUI Emacs
  :config
  (exec-path-from-shell-initialize))   ;; Import env vars like PATH, etc.

(provide 'init-exec-path)
;;; init-exec-path.el ends here
