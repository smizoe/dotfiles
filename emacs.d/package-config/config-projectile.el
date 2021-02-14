;;; config-projectile.el ---                         -*- lexical-binding: t; -*-

;;; Code:
(use-package projectile
  :ensure t
  :config
  (progn
    (projectile-mode)
    (with-eval-after-load 'evil
      (cl-loop for pair in `(
                             ("rb" . ,#'projectile-compile-project)
                             ("rt" . ,#'projectile-test-project)
                             ("rr" . ,#'projectile-run-project)
                             )
               do
               (evil-leader/set-key (car pair) (cdr pair))
               )
      )
    )
  :pin melpa-stable
  )


(provide 'config-projectile)
;;; config-projectile.el ends here
