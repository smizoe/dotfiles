;;;;;;;;;;;;;;;;;;;;
;; yasnippet mode ;;
;;;;;;;;;;;;;;;;;;;;

;; (package-install 'yasnippet)
;; (install-elisp-from-emacswiki "yasnippet-config.el")
;; I've made a modification to the yasnippet-config.el
(add-to-list 'load-path "~/.emacs.d/elisp/yasnippet-config")
(require 'yasnippet)
(yas-global-mode 1)

(require 'yasnippet-config)
(define-sequential-command kill-ring-save-x
  kill-ring-save yas/register-oneshot-snippet)
(define-key esc-map "w" 'kill-ring-save-x) ; M-w
(define-key global-map "\C-x\C-y" 'yas/expand-oneshot-snippet)

