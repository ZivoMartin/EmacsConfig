;;; init-editing.el --- General editing enhancements -*- lexical-binding: t; -*-

;;; Commentary:
;; This file contains settings to make editing smoother.
;; Includes persistent cursor positions, backups, auto-save control,
;; and indentation preferences.

;;; Code:

(require 'init-elpa)

;; Installed by default, will bring you back where you were when come backing in a file
(require 'saveplace)
(setq-default save-place t)

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

;;; Better parentheses and pairs navigation 
(use-package smartparens
  :init
  (require 'smartparens-config) ; Loads default pair definitions
  :hook (prog-mode . smartparens-mode)
  :bind
  (("C-M-l" . sp-forward-sexp)   ;; jump to closing
   ("C-M-h" . sp-backward-sexp)  ;; jump to opening
   ("M-a"   . sp-beginning-of-sexp)
   ("M-e"   . sp-end-of-sexp)))


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

(provide 'init-editing)
;;; init-editing.el ends here
