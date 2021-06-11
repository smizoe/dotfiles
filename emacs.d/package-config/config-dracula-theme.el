;;; config-dracula-theme.el ---                      -*- lexical-binding: t; -*-

;;; Code:
(use-package dracula-theme
  :ensure t
  :pin melpa
  :init
  (progn
    (load-theme 'dracula t t)
    )
  )


(provide 'config-dracula-theme)
;;; config-dracula-theme.el ends here
