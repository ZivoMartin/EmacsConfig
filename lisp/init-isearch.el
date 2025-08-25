;;; init-isearch.el --- Minimal tailored isearch behavior  -*- lexical-binding: t; -*-

;; Global bindings

(with-eval-after-load 'isearch
  (let ((map isearch-mode-map))
    (define-key map (kbd "M-s") nil)

    (global-set-key (kbd "C-s")   #'isearch-forward)
    (global-set-key (kbd "s-s") #'isearch-backward)

    (define-key map (kbd "C-s") #'isearch-repeat-forward)
    (define-key map (kbd "s-s") #'isearch-repeat-backward)

    (define-key map (kbd "RET")       #'isearch-exit)
    (define-key map (kbd "<return>")  #'isearch-exit)
    (define-key map (kbd "C-g")       #'isearch-exit)

    (defun my-isearch-exit-and-next-line ()
      "Quit isearch and move cursor down."
      (interactive)
      (isearch-exit)
      (next-line))

    (defun my-isearch-exit-and-previous-line ()
      "Quit isearch and move cursor up."
      (interactive)
      (isearch-exit)
      (previous-line))

    (defun my-isearch-exit-and-forward-char ()
      "Quit isearch and move cursor right."
      (interactive)
      (isearch-exit)
      (forward-char))

    (defun my-isearch-exit-and-backward-char ()
      "Quit isearch and move cursor left."
      (interactive)
      (isearch-exit)
      (backward-char))

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
