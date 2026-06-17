;;; init-light-themes.el --- Defines all the light-themes preferences -*- lexical-binding: t; -*-

;;; Commentary:

;;; Code:

(use-package doom-themes
  :defer t)

(use-package solarized-theme
  :defer t)

(use-package faff-theme
  :defer t)


(defvar martin-light-theme-list
  '(doom-ayu-light
    doom-acario-light
    doom-nord-light
    solarized-light
    faff)
  "List of my favorite light themes.")

(defun martin-load-light-theme (theme)
  "Disable active light-themes and load THEME."
  (interactive
   (list (intern
          (completing-read "Theme: "
                           (mapcar #'symbol-name martin-light-theme-list)
                           nil t))))
  (mapc #'disable-theme custom-enabled-themes)
  (load-theme theme t))

(provide 'init-light-themes)
;;; init-light-themes.el ends here
