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

