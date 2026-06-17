;;; init-shell.el --- Defines all the SHELL preferences -*- lexical-binding: t; -*-

;;; Commentary:

;;; Code:

(defun martin-shell-base-name (&optional name)
  "Return the base (non-starred) shell name.
If NAME is nil, use the current buffer name."
  (let ((base (or name (buffer-name))))
    (if (string-prefix-p "shell-" base)
        base
      (concat "shell-" base))))

(defvar martin-default-shell nil
  "Name of the buffer of the default shell to open.")

(defun martin-shell-new (&optional force-create not-use-default name)
  "Create a new shell buffer in the current window.
If NAME is provided, use it as the base name.
NOT-USE-DEFAULT should be true if you want to ignore the default value,
we update it anyway.
FORCE-CREATE forces shell to create a new buffer."
  (interactive)

  (let* ((shell-buffer-name (martin-shell-base-name name)))

    (setq martin-default-shell
          (if (or not-use-default
                  force-create
                  (not martin-default-shell))
              shell-buffer-name
            martin-default-shell)) ;; Decides the name of the shell bufferr

    (if (and (not force-create)
             (get-buffer martin-default-shell))
        (switch-to-buffer martin-default-shell)
      (shell (generate-new-buffer-name martin-default-shell)))))

(defun martin-shell-other-window (&optional force-create not-use-default name)
  "Create a new shell buffer in another window.
If NAME is provided, use it as the base name.
NOT-USE-DEFAULT should be true if you want to ignore the default value.
FORCE-CREATE forces shell to create a new buffer."
  (interactive)
  (let ((win (split-window-right)))
    (set-window-buffer win (current-buffer))
    (other-window 1)
    (martin-shell-new force-create not-use-default name)))

(defun martin-shell-force-new (&optional name)
  "Force the creation of a new shell buffer with the name NAME in the current window."
  (interactive)
  (martin-shell-new t nil name))

(defun martin-shell-force-other-window (&optional name)
  "Force the creation of a new shell buffer with the name NAME in another window."
  (interactive)
  (martin-shell-other-window t nil name))

(defun martin-shell-try-new (&optional name)
  "Try to open the shell buffer of the current window with the name NAME.
Open a new one otherwise."
  (interactive)
  (martin-shell-new nil t name))

(defun martin-shell-try-other-window (&optional name)
  "Try to open the shell buffer of the current window in another window with the name NAME, open a new one otherwise."
  (interactive)
  (martin-shell-other-window nil t name))

(with-eval-after-load 'shell
  (define-key shell-mode-map (kbd "C-o")   #'comint-previous-input)
  (define-key shell-mode-map (kbd "C-M-o") #'comint-next-input))

(provide 'init-shell)
;;; init-shell.el ends here
