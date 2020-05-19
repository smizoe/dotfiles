;;; config-ess.el ---                                -*- lexical-binding: t; -*-
;;; Code:

(use-package ess
  :ensure t
  :init
  (progn
    (require 'ess-site)
    (define-abbrev-table 'ess-mode-abbrev-table
      '(
        ("pp" "%>%" nil 0)
        )
      )
    (dolist (hook '(ess-mode-hook inferior-ess-mode-hook))
      (add-hook hook
                (lambda ()
                  (make-local-variable 'local-abbrev-table)
                  (setq local-abbrev-table ess-mode-abbrev-table)
                  )
                )
      )
    (with-eval-after-load 'evil
      (progn
            ;;; ess with evil
        (evil-define-key 'normal ess-mode-map
          "\\l" 'ess-eval-line
          "\\aa" 'ess-load-file
          "\\ff" 'ess-eval-function
          "\\pp" 'ess-eval-paragraph
          "\\rh" 'ess-help
          "\\rf" 'run-ess-r-newest
          "\\gr" 'ess-switch-process
          )
        (evil-define-key 'visual ess-mode-map
          "\\ss" 'ess-eval-region
          )
        (evil-define-key 'normal ess-help-mode-map
          "q" 'ess-help-quit
          )
        (evil-leader/set-key-for-mode 'inferior-ess-mode-map
          "b" 'helm-buffers-list
          )
        )
      )
    )
  )

(provide 'config-ess)
;;; config-ess.el ends here
