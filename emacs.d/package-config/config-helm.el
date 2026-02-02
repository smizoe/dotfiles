;;; config-helm.el ---                               -*- lexical-binding: t; -*-
;;; Code:
(use-package helm
  :ensure t
  :pin melpa-stable
  :config
  (progn
    (define-key helm-map (kbd "<tab>") 'helm-execute-persistent-action)
    (define-key helm-map (kbd "TAB") 'helm-execute-persistent-action)
    (define-key helm-map (kbd "C-z") 'helm-select-action)
    (define-key isearch-mode-map (kbd "C-o") 'helm-occur-from-isearch)
    (define-key isearch-mode-map (kbd "C-M-o") 'isearch-occur)
    (helm-mode 1)
    )
  )

(use-package helm-ag
  :ensure t
  :after (evil-leader)
  :init
  (custom-set-variables
    '(helm-ag-base-command "rg --vimgrep --color never --no-heading")
    )
  :config
  (progn
    (evil-leader/set-key
      "sr" 'helm-do-ag-project-root
      "sf" 'helm-do-ag-this-file
      "sb" 'helm-do-ag-buffers
      )
    )
  )


(use-package helm-projectile
  :ensure t
  :after (helm projectile evil-leader)
  :config
  (progn
    (helm-projectile-on)
    (evil-leader/set-key
      "ps" 'helm-projectile-switch-project
      "pe" 'helm-projectile-find-file
      "pf" 'helm-projectile-find-file-in-known-projects
      "pxf" 'helm-projectile-recentf
      "pd" 'helm-projectile-find-dir
      )
    )
  :pin melpa-stable
  )

(provide 'config-helm)
;;; config-helm.el ends here
