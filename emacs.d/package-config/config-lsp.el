;;; config-lsp.el ---                                -*- lexical-binding: t; -*-
;;; Code:
(custom-set-variables
 '(lsp-rust-analyzer-proc-macro-enable t)
 )
(use-package lsp-mode
  :hook (
         (typescript-mode . lsp-deferred)
         (rust-mode . lsp-deferred)
         (powershell-mode . lsp-deferred)
         (c++-mode . lsp-deferred)
         (go-mode . (lambda ()
                      (add-hook 'before-save-hook #'lsp-format-buffer t t)
                      (add-hook 'before-save-hook #'lsp-organize-imports t t)
                      (lsp-deferred)
                      )
                  )
         )
  :commands lsp
  :ensure t
  :pin melpa
  )

(use-package lsp-ui
  :commands lsp-ui
  :ensure t
  :pin melpa-stable
  )

(use-package lsp-python-ms
  :ensure t
  :hook (python-mode . (lambda ()
                         (require 'lsp-python-ms)
                         (lsp-deferred)))
  )

(use-package helm-lsp
  :after (helm lsp-mode evil-leader)
  :ensure
  :config
  (progn
    (define-key evil-normal-state-map [remap code-search-symbol] #'helm-lsp-workspace-symbol)
    (evil-define-minor-mode-key 'normal 'lsp-mode
      ;; evil-leader/set-key-for-mode works only for major modes
      ;; so evil-define-minor-mode-key is used
      " ea" (lambda ()
              (interactive)
              (helm-lsp-code-actions)
              )
      " fb" (lambda ()
              (interactive)
              (lsp-format-buffer)
              )
      )
    )
  )

(provide 'config-lsp)
;;; config-lsp.el ends here
