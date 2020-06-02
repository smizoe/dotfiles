;;; config-rust.el ---                          -*- lexical-binding: t; -*-

;;; Code:
(use-package rust-mode
  :pin melpa
  :ensure t
  :config
  (with-eval-after-load 'evil-leader
    (progn
      (evil-leader/set-key-for-mode 'rust-mode
        "rb" #'rust-compile
        "rt" #'rust-test
        "rr" #'rust-run
        )
      )
    )
  )


(provide 'config-rust)
;;; config-rust-mode.el ends here
