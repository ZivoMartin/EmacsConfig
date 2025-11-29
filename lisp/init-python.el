;;; init-python.el --- Python configuration -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

(use-package python
  :ensure nil
  :hook (python-mode . eglot-ensure)
  :config
  (setq python-indent-guess-indent-offset-verbose nil)
  (add-hook 'python-mode-hook
            (lambda ()
              (add-hook 'before-save-hook
                        (lambda ()
                          (when (eq major-mode 'python-mode)
                            (let ((p (point)))
                              (shell-command-on-region
                               (point-min) (point-max)
                               "black -q -"
                               (current-buffer) t "*black-error*" t)
                              (goto-char p))))
                        nil t))))

(provide 'init-python)
;;; init-python.el ends here
