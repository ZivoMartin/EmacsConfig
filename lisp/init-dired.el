;;; init-dired.el --- Dired configuration with dired-hacks -*- lexical-binding: t; -*- 
;;; Commentary:
;;; Code:

(require 'dired)
(require 'autorevert)

;; Core Dired settings
(use-package dired
  :defer t
  :commands (dired dired-jump)
  :config
  ;; Always reuse same buffer (don’t spawn a million dired buffers)
  (put 'dired-find-alternate-file 'disabled nil)
  (define-key dired-mode-map (kbd "RET") #'dired-find-alternate-file)
  (define-key dired-mode-map (kbd "^") (lambda () (interactive) (find-alternate-file ".."))))

;; Subtree expansion (inline directory tree)
(use-package dired-subtree
  :after dired
  :bind (:map dired-mode-map
              ("i" . dired-subtree-insert)
              (";" . dired-subtree-remove)
              ("TAB" . dired-subtree-toggle)))

;; Narrow live filtering
(use-package dired-narrow
  :after dired
  :bind (:map dired-mode-map
              ("/" . dired-narrow))) ;; start narrowing

;; Filtering by type / regex
(use-package dired-filter
  :after dired
  :bind (:map dired-mode-map
              ("f" . dired-filter-map))) ;; opens filter command map

;; Color files by extension
(use-package dired-rainbow
  :after dired
  :config
  ;; Example: tweak to your taste
  (dired-rainbow-define html "#4e9a06" ("htm" "html" "xhtml"))
  (dired-rainbow-define media "#ce5c00" ("mp3" "mp4" "avi" "mkv"))
  (dired-rainbow-define image "#75507b" ("jpg" "jpeg" "png" "gif" "svg"))
  (dired-rainbow-define doc "#204a87" ("pdf" "tex" "bib"))
  (dired-rainbow-define compiled "#c4a000" ("o" "so" "elc" "class"))
  (dired-rainbow-define executable "#a40000" ("sh" "zsh" "bash" "exe")))

;; Collapse boring files (like *.o, *~) into one line
(use-package dired-collapse
  :after dired
  :hook (dired-mode . dired-collapse-mode))

;; Open files externally (xdg-open / open)
(use-package dired-open
  :after dired
  :bind (:map dired-mode-map
              ("C-<return>" . dired-open-xdg)))

(provide 'init-dired)
;;; init-dired.el ends here
