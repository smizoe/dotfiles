;;;;;;;;;;;;;;;;;;;;
;; yasnippet mode ;;
;;;;;;;;;;;;;;;;;;;;

;; (package-install 'yasnippet)
;; (install-elisp-from-emacswiki "yasnippet-config.el")
(require 'yasnippet)
(yas-global-mode 1)
(require 'yasnippet-config)
(setq ac-use-menu-map t)

(define-key yas-minor-mode-map (kbd "<tab>") nil)
(define-key yas-minor-mode-map (kbd "TAB") nil)
(define-key yas-minor-mode-map (kbd "\C-k") 'yas-expand)

(define-key yas-keymap [(tab)]       nil)
(define-key yas-keymap (kbd "TAB")   nil)
(define-key yas-keymap [(shift tab)] nil)
(define-key yas-keymap [backtab]     nil)
(define-key yas-keymap (kbd "\C-k") 'yas-next-field-or-maybe-expand)
(define-key yas-keymap (kbd "\C-m") 'yas-prev-field)
