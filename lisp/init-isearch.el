;;; init-isearch.el --- Minimal tailored isearch behavior  -*- lexical-binding: t; -*-

;;; Commentary:

;;; Code:

;; Exit only if we really have a confirmed match; otherwise abort back to start
(defun my/isearch-exit-or-abort ()
  "In isearch: exit when a real match is selected; otherwise abort."
  (interactive)
  (if (and isearch-success
           (> (length isearch-string) 0))
      (isearch-exit)
    (isearch-abort)))

(defun my-isearch-exit-and-next-line ()
  "Quit isearch and move cursor down."
  (interactive)
  (my/isearch-exit-or-abort)
  (next-line))

(defun my-isearch-exit-and-previous-line ()
  "Quit isearch and move cursor up."
  (interactive)
  (my/isearch-exit-or-abort)
  (previous-line))

(defun my-isearch-exit-and-forward-char ()
  "Quit isearch and move cursor right."
  (interactive)
  (my/isearch-exit-or-abort)
  (forward-char))

(defun my-isearch-exit-and-backward-char ()
  "Quit isearch and move cursor left."
  (interactive)
  (my/isearch-exit-or-abort)
  (backward-char))


;; Global bindings

(with-eval-after-load 'isearch
  (let ((map isearch-mode-map))
    (define-key map (kbd "M-s") nil)

    (global-set-key (kbd "C-s")   #'isearch-forward)
    (global-set-key (kbd "s-s")   #'isearch-backward)

    (define-key map (kbd "M-s") #'isearch-repeat-backward)

    (define-key map (kbd "RET")       #'my/isearch-exit-or-abort)
    (define-key map (kbd "<return>")  #'my/isearch-exit-or-abort)

    (define-key map (kbd "C-g")       #'my/isearch-exit-or-abort)

    (define-key isearch-mode-map (kbd "C-k") #'my-isearch-exit-and-next-line)
    (define-key isearch-mode-map (kbd "C-j") #'my-isearch-exit-and-previous-line)
    (define-key isearch-mode-map (kbd "C-l") #'my-isearch-exit-and-forward-char)
    (define-key isearch-mode-map (kbd "C-h") #'my-isearch-exit-and-backward-char)

    (define-key map (kbd "C-p")       #'isearch-del-char)

    (define-key map (kbd "<backspace>") #'isearch-abort)
    (define-key map (kbd "DEL")         #'isearch-abort)
  ))

(provide 'init-isearch)
;;; init-isearch.el ends here
