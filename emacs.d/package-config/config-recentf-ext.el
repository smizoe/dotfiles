;;; config-recentf-ext.el ---                        -*- lexical-binding: t; -*-
;;; Code:


(use-package recentf-ext
  :ensure t
  :config
  (progn
    (setq recentf-max-saved-items 500)
    (setq recentf-exclude '("/TAGS$" "/var/tmp/"))
    )
  )


(provide 'config-recentf-ext)
;;; config-recentf-ext.el ends here
