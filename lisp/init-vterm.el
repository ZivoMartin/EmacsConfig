;;; init-vterm.el --- Defines all the VTERM preferences -*- lexical-binding: t; -*-

;;; Commentary:

;;; Code:

(require 'init-use-package)
(require 'init-martin-group)

(defvar vterm-path (expand-file-name "emacs-libvterm" manual-installation-directory))

(defmacro martin-vterm-send-key (name key &rest flags)
  "Create a function with the name NAME sending KEY to vterm.
FLAGS can include :shift, :meta, :ctrl."
  (let ((shift (if (memq :shift flags) t nil))
        (meta  (if (memq :meta flags)  t nil))
        (ctrl  (if (or (memq :ctrl flags)
                       (memq :control flags))
                   t nil)))
    `(defun ,name ()
       ,(format "Send %s to vterm." key)
       (interactive)
       (vterm-send-key ,key ,shift ,meta ,ctrl))))

(martin-vterm-send-key martin-vterm-delete-char "d" :ctrl)
(martin-vterm-send-key martin-vterm-delete-word "d" :meta)
(martin-vterm-send-key martin-vterm-backspace "<backspace>")
(martin-vterm-send-key martin-vterm-meta-backspace "<backspace>" :meta)

(martin-vterm-send-key martin-vterm-up "<up>")
(martin-vterm-send-key martin-vterm-down "<down>")
(martin-vterm-send-key martin-vterm-right "<right>")
(martin-vterm-send-key martin-vterm-left "<left>")

(martin-vterm-send-key martin-vterm-control-up "<up>" :ctrl)
(martin-vterm-send-key martin-vterm-control-down "<down>" :ctrl)
(martin-vterm-send-key martin-vterm-control-right "<right>" :ctrl)
(martin-vterm-send-key martin-vterm-control-left "<left>" :ctrl)

(martin-vterm-send-key martin-vterm-delete-line "k" :ctrl)
(martin-vterm-send-key martin-vterm-delete-line "u" :ctrl)

(martin-vterm-send-key martin-vterm-interrupt "c" :ctrl)
(martin-vterm-send-key martin-vterm-clear "l" :ctrl)

(martin-vterm-send-key martin-vterm-beginning-of-line "a" :ctrl)
(martin-vterm-send-key martin-vterm-end-of-line "e" :ctrl)

(use-package vterm
  :load-path vterm-path)

(with-eval-after-load 'vterm
  (set-face-attribute 'default nil :height 170))

(defvar martin-vterm-override-mode-map
  (let ((map (make-sparse-keymap)))
    (keymap-set map "C-p" #'martin-vterm-backspace)
    (keymap-set map "M-p" #'martin-vterm-meta-backspace)
    
    (keymap-set map "C-d" #'martin-vterm-delete-char)
    (keymap-set map "M-d" #'martin-vterm-delete-word)
    
    (keymap-set map "C-;" #'martin-vterm-right)
    (keymap-set map "C-l" #'martin-vterm-up)
    (keymap-set map "C-k" #'martin-vterm-down)
    (keymap-set map "C-j" #'martin-vterm-left)

    (keymap-set map "M-;" #'martin-vterm-control-right)
    (keymap-set map "M-l" #'martin-vterm-control-up)
    (keymap-set map "M-k" #'martin-vterm-control-down)
    (keymap-set map "M-j" #'martin-vterm-control-left)

    (keymap-set map "C-'" #'martin-vterm-delete-line)
    (keymap-set map "M-'" #'martin-vterm-delete-whole-line)


    (keymap-set map "C-c C-c" #'martin-vterm-interrupt)
    (keymap-set map "C-v" #'martin-vterm-clear)

    (keymap-set map "C-a" #'martin-vterm-beginning-of-line)
    (keymap-set map "C-e" #'martin-vterm-end-of-line)

    map)
  "Keymap for `martin-vterm-override-mode`.")

(defun martin-vterm-copy-mode-or-insert ()
  "Toggle vterm copy mode if in a vterm buffer.
Insert 4 spaces otherwise"
  (interactive)
  (if (derived-mode-p 'vterm-mode)
      (vterm-copy-mode 'toggle)
    (insert "    ")))

(defun martin-vterm-base-name (&optional name)
  "Return the base (non-starred) vterm name.
If NAME is nil, use the current buffer name."
  (let ((base (or name (buffer-name))))
    (if (string-prefix-p "vterm-" base)
        base
      (concat "vterm-" base))))

(defvar martin-default-vterm nil
  "Name of the buffer of the default vterm to open.")

(defun martin-vterm-new (&optional force-create not-use-default name)
  "Create a new vterm buffer in the current window.
If NAME is provided, use it as the base name.
NOT-USE-DEFAULT should be true if you want to ignore the default value,
we update it anyway.
FORCE-CREATE forces vterm to create a new buffer."
  (interactive)
  (let* ((vterm-buffer-name (martin-vterm-base-name name)))

    (setq martin-default-vterm
          (if (or (or not-use-default force-create) (not martin-default-vterm))
              vterm-buffer-name martin-default-vterm))

    (if (and (not force-create) (get-buffer martin-default-vterm))
        (switch-to-buffer martin-default-vterm)
      (vterm (generate-new-buffer-name martin-default-vterm)))))

(defun martin-vterm-other-window (&optional force-create not-use-default name)
  "Create a new vterm buffer in another window.
If NAME is provided, use it as the base name.
NOT-USE-DEFAULT should be true if you want to ignore the default value,
FORCE-CREATE forces vterm to create a new buffer."
  (interactive)
  (let ((win (split-window-right)))
    (set-window-buffer win (current-buffer))
    (other-window 1)
    (martin-vterm-new force-create not-use-default name)))

(defun martin-vterm-force-new (&optional name)
  "Force the creation of a new vterm buffer with the name NAME on the current buffer."
  (interactive)
  (martin-vterm-new t nil name))

(defun martin-vterm-force-other-window (&optional name)
  "Force the creation of a new vterm buffer with the name NAME on another window."
  (interactive)
  (martin-vterm-other-window t nil name))

(defun martin-vterm-try-new (&optional name)
  "Try to open the vterm buffer of the current window with the name NAME, open a new one otherwise."
  (interactive)
  (martin-vterm-new nil t name))

(defun martin-vterm-try-other-window (&optional name)
  "Try to open the vterm buffer of the current window on another window with the name NAME, open a new one otherwise."
  (interactive)
  (martin-vterm-other-window nil t name))

(define-minor-mode martin-vterm-override-mode
  "Emulation layer overriding `martin-mode` in vterm buffers."
  :group martin
  :init-value nil
  :lighter nil)

(defvar martin-vterm-override--emulation-alist
  `((martin-vterm-override-mode . ,martin-vterm-override-mode-map))
  "Entry is the `emulation-mode-map-alist`.")

(defun martin--vterm-enter ()
  "Enable vterm override when entering a vterm buffer."
  (face-remap-add-relative 'default :height 120)
  (unless vterm-copy-mode
    (martin-vterm-override-mode 1)))

(defun martin--vterm-handle-copy-mode ()
  "Enable or disable vterm override depending on copy mode."
  (if vterm-copy-mode
      (martin-vterm-override-mode -1)
    (martin-vterm-override-mode 1)))

(add-hook 'vterm-mode-hook #'martin--vterm-enter)
(add-hook 'vterm-copy-mode-hook #'martin--vterm-handle-copy-mode)

(unless (member 'martin-vterm-override--emulation-alist
                emulation-mode-map-alists)
  (push 'martin-vterm-override--emulation-alist
        emulation-mode-map-alists))

(defvar martin-vterm-prompt-regexp "➜"
  "Regexp matching the vterm prompt.")

(defun martin-vterm-get-output ()
  "Return the output between the last two prompt, excluding the command."
  (save-excursion
    (let (prompt2 prompt1 output-start output-end)

      (goto-char (point-max))
      (unless (re-search-backward martin-vterm-prompt-regexp nil t)
        (error "No prompt found"))
      (setq prompt2 (point))

      (unless (re-search-backward martin-vterm-prompt-regexp nil t)
        (error "Only one prompt found"))
      (setq prompt1 (point))

      (goto-char prompt1)
      (setq output-start (1+ (line-end-position)))

      (setq output-end prompt2)

      (buffer-substring-no-properties output-start output-end))))

(defun martin-vterm-copy-output ()
  "Copy output between the last two prompt, excluding the command."
  (interactive)
  (save-excursion
    (let ((text (martin-vterm-get-output)))
      (kill-new text)
      (message "Last command output copied"))))

;; This is bad but it works on my machine 100% of the time, so no need of changing
;; it for now.
(defun martin-vterm-pwd ()
  "Manually execute pwd on the current vterm buffer and return the output."
   (interactive)
  (let ((start (point-max)))
    (vterm-send-string "pwd")
    (vterm-send-return)
    (let ((output (martin-vterm-get-output)))
      (aset output (1- (length output)) ?/)
      output)))

(defun martin-vterm-find-file ()
  "If in a vterm buffer, call find file from the vterm position.
Otherwise call the regular find file."
  (interactive)
  (if (string-prefix-p "vterm" (buffer-name))

      (let ((default-directory (martin-vterm-pwd)))
        (call-interactively #'find-file))

      (call-interactively #'find-file)))

(provide 'init-vterm)
;;; init-vterm.el ends here
