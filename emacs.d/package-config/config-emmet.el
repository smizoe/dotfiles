;;; config-emmet.el ---                              -*- lexical-binding: t; -*-
;;; Code:

(use-package emmet-mode
  :ensure t
  :init
  (progn
    (add-hook 'web-mode-hook 'emmet-mode)
    (add-hook 'nxml-mode-hook 'emmet-mode)
    (add-hook 'sgml-mode-hook 'emmet-mode) ;; Auto-start on any markup modes
    (add-hook 'css-mode-hook  'emmet-mode) ;; enable Emmet's css abbreviation.
    (add-hook 'js2-mode-hook
              (lambda ()(progn
                 (emmet-mode)
                 (setq-local emmet-expand-jsx-className? t)
                 )))
    (with-eval-after-load 'evil
      (evil-define-key 'insert emmet-mode-keymap
        "\C-e" 'emmet-expand-line
        )
      )
    )
 )


(provide 'config-emmet)
;;; config-emmet.el ends here
