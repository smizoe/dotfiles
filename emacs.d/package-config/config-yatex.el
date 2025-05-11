;;; config-yatex.el ---                              -*- lexical-binding: t; -*-

;; Copyright (C) 2020

(use-package yatex
  :ensure t
  :init
  (progn
    (setq auto-mode-alist (cons '("\\.tex$" . yatex-mode) auto-mode-alist))
    (autoload 'yatex-mode "yatex" "Yet Another LaTeX mode" t)
    (setq tex-command "lualatex")
    )
  :config
  (progn
    (with-eval-after-load 'evil
      (progn
                ;;;; yatex
        (evil-define-key 'normal YaTeX-mode-map
          "\\" (simulate-key-press YaTeX-prefix)
          )
        (evil-define-key 'visual YaTeX-mode-map
          "\\" (simulate-key-press YaTeX-prefix)
          )

        ;; yahtml
        (evil-define-key 'normal yahtml-mode-map
          "\\" (simulate-key-press yahtml-prefix)
          )
        (evil-define-key 'visual yahtml-mode-map
          "\\" (simulate-key-press yahtml-prefix)
          )
        )
      )
    )
  )



(provide 'config-yatex)
;;; config-yatex.el ends here
