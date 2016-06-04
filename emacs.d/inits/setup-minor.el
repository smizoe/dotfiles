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
(global-auto-complete-mode t)
(setq ac-dwim t)
(ac-config-default)
(setq ac-modes (list* 'sql-mode  'yaml-mode  ac-modes))
(setq ac-disable-faces nil)

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

;;;;;;;;;;;;;;;;;;;;;;;
;; other minor modes ;;
;;;;;;;;;;;;;;;;;;;;;;;

;;; colour current line
(global-hl-line-mode 1)

;;; save history and enable it even after reboot of emacs
(savehist-mode 1)

;;; show corresponding parentheses
(show-paren-mode 1)

;;; show line number and column number
(line-number-mode 1)
(column-number-mode 1)

;;; make the selected region visible
(transient-mark-mode 1)


;;; use multiple spaces instead of tabs
(setq-default indent-tabs-mode nil)

;; key-chord
;; (install-elisp-from-emacswiki "key-chord.el")
(require 'key-chord)
(setq key-chord-two-keys-delay 0.04)
(key-chord-mode 1)
;;key-chrod settings
(key-chord-define-global "xf" 'helm-for-files)

;;;;;;;;;;;;;;;;;
;; abbrev mode ;;
;;;;;;;;;;;;;;;;;

(setq-default abbrev-mode t)


;; flymake and flycheck
;; (package-install 'flycheck)
;; (package-install 'flymake-cursor)
(require 'flymake)
(require 'flycheck)
(require 'flymake-cursor)
(add-hook 'flymake-mode-hook (lambda ()
                          (local-set-key (kbd "C-c C-n") 'flymake-goto-next-error)))
(add-hook 'flymake-mode-hook (lambda ()
                          (local-set-key (kbd "C-c C-p") 'flymake-goto-prev-error)))
(add-hook 'flycheck-mode-hook (lambda ()
                          (local-set-key (kbd "C-c C-n") 'flycheck-next-error)))
(add-hook 'flycheck-mode-hook (lambda ()
                          (local-set-key (kbd "C-c C-p") 'flycheck-previous-error)))

(global-flycheck-mode)

(provide 'setup-minor)
