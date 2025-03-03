(setq vue-mode-packages
  '(vue-mode))

(setq vue-mode-excluded-packages '())

(defun vue-mode/init-vue-mode ()
  "Initialize my package"
  (use-package vue-mode))

(add-to-list 'auto-mode-alist '("\\.vue\\'" . web-mode))
(provide 'init-vue)
