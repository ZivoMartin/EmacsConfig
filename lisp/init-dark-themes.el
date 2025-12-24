;;; init-dark-themes.el --- Defines all the dark-themes preferences -*- lexical-binding: t; -*-

;;; Commentary:

;;; Code:

(use-package atom-one-dark-theme
  :defer t)

(use-package nord-theme
  :defer t)

(use-package zenburn-theme
  :defer t)

(use-package gruvbox-theme
  :defer t)

(use-package catppuccin-theme
  :defer t)

(use-package dracula-theme
  :defer t)

(defvar martin-dark-theme-list
  '(atom-one-dark
    nord
    zenburn
    gruvbox-dark-medium
    catppuccin
    dracula)
  "List of my favorite dark-themes.")


(defun martin-load-dark-theme (theme)
  "Disable active dark-themes and load THEME."
  (interactive
   (list (intern
          (completing-read "Theme: "
                           (mapcar #'symbol-name martin-dark-theme-list)
                           nil t))))
  (mapc #'disable-theme custom-enabled-themes)
  (load-theme theme t))

(provide 'init-dark-themes)
;;; init-dark-themes.el ends here
 
