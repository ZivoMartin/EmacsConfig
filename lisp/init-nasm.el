;; Activer asm-mode pour les fichiers .asm
(add-to-list 'auto-mode-alist '("\\.asm\\'" . asm-mode))

;; Personnalisation d'asm-mode
(defun my-asm-mode-hook ()
  "Configuration personnalisée pour asm-mode."
  (setq tab-width 4)         ;; Largeur de tabulation à 4 espaces
  (setq indent-tabs-mode nil) ;; Utiliser des espaces au lieu de tabs
  (electric-indent-mode -1)   ;; Désactiver l'indentation automatique
  (local-set-key (kbd "RET") 'newline) ;; Éviter l'auto-indentation gênante
  (setq asm-comment-char ?\;) ;; Assurer que les commentaires commencent par ';'
  (add-hook 'before-save-hook 'delete-trailing-whitespace nil t))

(add-hook 'asm-mode-hook 'my-asm-mode-hook)

(defun assemble-and-run ()
  (interactive)
  (save-buffer)
  (let ((output (file-name-sans-extension buffer-file-name)))
    (compile (format "nasm -f elf64 %s && ld %s.o -o %s && ./%s"
                     buffer-file-name output output output))))

(global-set-key (kbd "C-c C-a") 'assemble-and-run) ;; Raccourci pour assembler et exécuter


(provide 'init-nasm)
