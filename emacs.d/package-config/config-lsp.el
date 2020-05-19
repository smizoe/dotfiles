;;; config-lsp.el ---                                -*- lexical-binding: t; -*-
;;; Code:
(use-package lsp-mode
  :hook (
         (typescript-mode . lsp-deferred)
         (rust-mode . lsp-deferred)
         (powershell-mode . lsp-deferred)
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

(use-package company-lsp
  :commands company-lsp
  :ensure t
  :pin melpa-stable
  )

(use-package lsp-python-ms
  :ensure t
  :hook (python-mode . (lambda ()
                         (require 'lsp-python-ms)
                         (lsp-deferred)))
  )

(provide 'config-lsp)
;;; config-lsp.el ends here
