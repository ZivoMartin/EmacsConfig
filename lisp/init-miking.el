;;; init-miking.el --- Major mode for Miking language (*.mc) -*- lexical-binding: t; -*-

;;; Commentary:
;; This defines a simple major mode for editing Miking language files (*.mc).
;; It includes syntax highlighting for keywords, types, constants, comments,
;; strings, characters, and common language constructs.

;;; Code:

(define-derived-mode miking-mode prog-mode "Miking"
  "Major mode for editing Miking files (.mc)."

  ;; Table de syntaxe pour les commentaires multilignes
  (modify-syntax-entry ?/ ". 124b" miking-mode-syntax-table)
  (modify-syntax-entry ?- ". 23"   miking-mode-syntax-table)
  (modify-syntax-entry ?\n ">"     miking-mode-syntax-table)

  ;; Détection des commentaires multilignes
  (setq-local syntax-propertize-function
              (syntax-propertize-rules
               ("\\(/-\\)" (1 "<"))
               ("\\(-/\\)" (1 ">"))))

  ;; Font-lock pour la coloration syntaxique
  (let* (
         ;; Définitions
         (keywords '("let" "in" "if" "then" "else" "type" "match" "with" "recursive"
                     "include" "sem" "syn" "lang" "end" "utest" "mexpr" "use" "con" "switch" "case" "lam"))
         (constants '("true" "false"))
         (types '("Int" "Bool" "String" "Float" "Char" "Unit" "List" "Option"))
         (keyword-regexp (regexp-opt keywords 'words))
         (constant-regexp (regexp-opt constants 'words))
         (type-regexp (regexp-opt types 'words)))
    (setq font-lock-defaults
          `((
             ;; Commentaires (ligne + multilignes)
             ("--.*$" . font-lock-comment-face)
             ("\\(/-\\(?:.\\|\n\\)*?-/\\)" . font-lock-comment-face)

             ;; Chaines de caractères
             ("\"\\([^\"\\]\\|\\\\.\\)*\"" . font-lock-string-face)

             ;; Caractères
             ("'\\(\\\\.\\|[^']\\)'" . font-lock-string-face)

             ;; Mots-clés, constantes, types
             (,keyword-regexp . font-lock-keyword-face)
             (,constant-regexp . font-lock-constant-face)
             (,type-regexp . font-lock-type-face)

             ;; Identifiants nommés
             ("\\<\\(type\\|syn\\|lang\\|use\\)\\s-+\\(\\w+\\)" 2 font-lock-type-face)
             ("\\<\\(let\\|lam\\|sem\\|con\\)\\s-+\\(\\w+\\)" 2 font-lock-variable-name-face)
             )))))

(add-to-list 'auto-mode-alist '("\\.mc\\'" . miking-mode))

(provide 'init-miking)
;;; init-miking.el ends here
