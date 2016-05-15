;;;;;;;;;;;
;; bm.el ;;
;;;;;;;;;;;

;; (install-elisp "http://cvs.savannah.gnu.org/viewvc/*checkout*/bm/bm/bm.el")
(setq-default bm-buffer-persistence nil)
(setq bm-restore-repository-on-load t)
(require 'bm)
(add-hook 'find-file-hooks 'bm-buffer-restore)
(add-hook 'kill-buffer-hook 'bm-buffer-save)
(add-hook 'after-save-hook 'bm-buffer-save)
(add-hook 'after-revert-hook 'bm-buffer-restore)
(global-set-key (kbd "M-SPC") 'bm-toggle)
(global-set-key (kbd "<f5>") 'bm-previous)
;;(global-set-key (kbd "S-<f5>") 'bm-next)
(global-set-key "\M-[25~" 'bm-next)
