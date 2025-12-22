;;; init-isearch.el --- Defines all the isearch preferences -*- lexical-bindings: t; -*-

;;; Commentary:

;;; Code:

(defun martin-isearch-exit-or-abort ()
  "In isearch: exit when a real match is selected; otherwise abort."
  (interactive)
  (if (and isearch-success
           (> (length isearch-string) 0))
      (isearch-exit)
    (isearch-abort)))

(with-eval-after-load 'isearch
  (let ((map isearch-mode-map))
    (keymap-set map "M-s" 'isearch-repeat-backward)

    (keymap-set map "C-p" 'isearch-delete-char)
    (keymap-set map "C-g" 'martin-isearch-exit-or-abort)
    (keymap-set map "C-j" nil)
    ))

(provide 'init-isearch)
;;; init-isearch.el ends here
