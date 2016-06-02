;;;;;;;;;;
;; helm ;;
;;;;;;;;;;

;; git clone https://github.com/emacs-helm/helm.git ~/.emacs.d/elisp/helm
;; (package-install 'helm)
(add-to-list 'load-path "~/.emacs.d/elisp/helm")
(require 'helm-config)
;; (helm-mode 1)
(global-set-key (kbd "C-x b") 'helm-buffers-list)
(global-set-key (kbd "M-x") 'helm-M-x)
(global-set-key (kbd "M-y") 'helm-show-kill-ring)
(global-set-key (kbd "M-s") 'helm-occur)
(define-key isearch-mode-map (kbd "C-o") 'helm-occur-from-isearch)
(define-key isearch-mode-map (kbd "C-M-o") 'isearch-occur)

;;;;;;;;;;;;;;;;;;;
;; auto-complete ;;
;;;;;;;;;;;;;;;;;;;

;; (auto-install-batch "auto-complete")
(require 'auto-complete-config)
(global-auto-complete-mode 1)
(setq ac-dwim t)
(ac-config-default)

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

(provide 'setup-minor)
