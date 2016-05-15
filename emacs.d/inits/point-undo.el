;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; point-undo (move back and forth to the previous or next position that the cursor existed) ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;; (install-elisp-from-emacswiki "point-undo.el")

(require 'point-undo)
(define-key global-map [f7] 'point-undo)
;;(define-key global-map (kbd "S-<f7>") 'point-redo)

;; the following is (shift f7) on mac (at least for now)
(define-key global-map "\M-[28~" 'point-redo)
