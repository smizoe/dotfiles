;;; config-yasnippet.el ---                          -*- lexical-binding: t; -*-
;;; Code:


(use-package yasnippet
  :ensure t
  :init
  (add-hook 'yas-minor-mode-hook
            (lambda ()
              (yas-activate-extra-mode 'fundamental-mode)))
  :config
  (progn
    (yas-global-mode 1)
    (cl-loop for hook-name in '(ruby-mode-hook sh-mode-hook python-mode-hook) do
             (add-hook hook-name (lambda ()
                                   (progn
                                     (make-local-variable 'yas-key-syntaxes)
                                     (add-to-list 'yas-key-syntaxes "<_")
                                     (add-to-list 'yas-key-syntaxes "<.")
                                     )
                                   )
                       )
             )
    (define-key yas-minor-mode-map (kbd "<tab>") nil)
    (define-key yas-minor-mode-map (kbd "TAB") nil)
    (define-key yas-minor-mode-map (kbd "C-k") 'yas-expand)

    (define-key yas-keymap [(tab)]       nil)
    (define-key yas-keymap (kbd "TAB")   nil)
    (define-key yas-keymap [(shift tab)] nil)
    (define-key yas-keymap [backtab]     nil)
    (define-key yas-keymap (kbd "C-k") 'yas-next-field-or-maybe-expand)
    (define-key yas-keymap (kbd "C-m") 'yas-prev-field)
    (with-eval-after-load 'evil
      (progn
        (define-key evil-insert-state-map (kbd "C-k") nil) ;; remove keymap; used in yas-minor-mode-map
        (add-hook 'yas-before-expand-snippet-hook (lambda () (evil-insert-state)))
        )
      )
    )
  )

(provide 'config-yasnippet)
;;; config-yasnippet.el ends here
