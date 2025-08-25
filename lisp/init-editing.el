;;; init-editing.el --- General editing enhancements -*- lexical-binding: t; -*-

;;; Commentary:
;; This file contains settings to make editing smoother.
;; Includes persistent cursor positions, backups, auto-save control,
;; and indentation preferences.

;;; Code:

(require 'init-elpa)

;; Installed by default, will bring you back where you were when come backing in a file
(require 'saveplace)

(save-place-mode 1)

;; keep track of saved places in ~/.emacs.d/places
(setq save-place-file (concat user-emacs-directory "places"))

;; Emacs can automatically create backup files. This tells Emacs to
;; put all backups in ~/.emacs.d/backups. More info:
;; http://www.gnu.org/software/emacs/manual/html_node/elisp/Backup-Files.html
(setq backup-directory-alist `(("." . ,(concat user-emacs-directory
                                               "backups"))))
(setq auto-save-default nil)

;; Indentations
(setq-default indent-tabs-mode nil)  ;; Use spaces, no tab
(setq-default tab-width 4)           ;; Tabs are 4 espaces longs
(setq-default c-basic-offset 4)      ;; Indentation size while programming
(setq c-default-style "linux")       ;; Indentation style

(use-package vertico
  :ensure t
  :init (vertico-mode))

(use-package savehist
  :init
  (setq savehist-additional-variables '(kill-ring))
  (savehist-mode 1))

;; Basic sexp navigation using built-in functions
(global-set-key (kbd "C-M-l") #'forward-sexp)    ;; jump to closing
(global-set-key (kbd "C-M-h") #'backward-sexp)   ;; jump to opening
(global-set-key (kbd "M-a")   #'backward-up-list)
(global-set-key (kbd "M-e")   #'down-list)

;; Enable these only in programming modes
(add-hook 'prog-mode-hook
          (lambda ()
            (show-paren-mode 1)))

(defun open-thing-at-point ()
  "Open the file or URL at point.
- If it's a URL, open it in the browser.
- If it's a path to an existing file, open it in Emacs.
- Otherwise, show an informative message."
  (interactive)
  (let ((thing (string-trim (or (thing-at-point 'url t)
                                (thing-at-point 'filename t) ""))))
    (cond
     ((string-match-p "\\`\\(https?\\|file\\)://" thing)
      (browse-url thing))
     ((file-exists-p (expand-file-name thing))
      (find-file (expand-file-name thing)))
     (t
      (message "No valid file or URL at point.")))))

(global-set-key (kbd "M-<return>") #'open-thing-at-point)

(setq lsp-idle-delay 3 lsp-completion-max-results 30)

(global-auto-revert-mode 1)

(provide 'init-editing)
;;; init-editing.el ends here
