(unless (version< emacs-version "24")
  ;; Charger zig-mode
  (add-to-list 'load-path "/home/martin/.emacs.d/zig-mode/")
  (autoload 'zig-mode "zig-mode" nil t)
  (add-to-list 'auto-mode-alist '("\\.zig\\'" . zig-mode))

  ;; Configuration pour zig-mode
  (with-eval-after-load 'zig-mode
    ;; Configuration de l'auto-complétion (via lsp-mode et compagnie)
    (add-hook 'zig-mode-hook #'lsp)
    (require 'lsp-mode)
    (setq lsp-zig-zls-executable "/path/to/zls") ; Remplace par le chemin vers zls (Zig Language Server)

    ;; Activer company-mode pour l'autocomplétion
    (add-hook 'zig-mode-hook #'company-mode)

    ;; Configuration pour le soulignage des erreurs (flycheck)
    (require 'flycheck)
    (add-hook 'zig-mode-hook #'flycheck-mode)

    ;; Auto-formatage lors de la sauvegarde
    (add-hook 'before-save-hook #'zig-mode-format-buffer)))

(provide 'init-zig)
