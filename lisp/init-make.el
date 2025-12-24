;;; init-make.el --- Defines all the make preferences -*- lexical-binding: t; -*-

;;; Commentary:

;;; Code:

(defun my-set-makefile ()
  "Search for the nearest Makefile upwards from the current buffer's directory.
If found, configure `compile-command` so that `M-x compile`
or `C-c m` runs `make` in the directory containing that Makefile."
  (interactive)
  (let ((makefile (locate-dominating-file default-directory "Makefile")))
    (when makefile
      (setq-local compile-command (format "make -C %s" makefile)))))

(add-hook 'find-file-hook #'my-set-makefile)

(defun my-compile-make ()
  "Run make immediately using the current `compile-command`."
  (interactive)
  (my-set-makefile)
  (compile compile-command))


(provide 'init-make)
;;; init-make.el ends here
