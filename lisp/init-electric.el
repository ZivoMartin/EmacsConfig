;;; init-electric.el --- Defines all the electric preferences -*- lexical-bindings: t; -*-

;;; Commentary:

;;; Code:

(electric-pair-mode 1)
(setq electric-pair-preserve-balance t)
(setq electric-pair-text-pairs '((?< . ?>)))

(provide 'init-electric)
;;; init-electric.el ends here
