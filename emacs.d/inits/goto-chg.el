;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; goto-chg.el (move to recently edited position) ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; (install-elisp-from-emacswiki "goto-chg.el")
(require 'goto-chg)
(define-key global-map (kbd "<f8>") 'goto-last-change)
;;(define-key global-map (kbd "S-<f8>") 'goto-last-change-reverse)
(global-set-key "\M-[29~" 'goto-last-change-reverse)
