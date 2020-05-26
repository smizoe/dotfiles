;;; config-company.el ---                            -*- lexical-binding: t; -*-
;;; Code:


(use-package company
  :ensure t
  :config
  (progn
    (global-company-mode t)
    (define-key company-active-map [return] nil)
    (define-key company-active-map (kbd "RET") nil)
    (define-key company-active-map (kbd "SPC") 'company-complete-selection)
    (define-key company-active-map [escape] 'company-abort)
    (define-key company-search-map [escape] 'company-search-abort)
    (custom-set-variables
     '(company-selection-wrap-around t)
     '(company-idle-dellay 0)
     '(company-dabbrev-downcase nil)
     '(company-dabbrev-ignore-case nil)
     )
    (with-eval-after-load 'evil
      (progn
        (define-key evil-insert-state-map (kbd "C-n") 'company-select-next)
        (define-key evil-insert-state-map (kbd "C-p") 'company-select-previous)
        )
      )
    )
  :pin melpa-stable
  )

(provide 'config-company)
;;; config-company.el ends here
