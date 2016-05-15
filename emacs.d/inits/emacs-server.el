;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; use emacs in server mode ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(server-start)
(defun iconify-emacs-when-server-is-done ()
  (unless server-clients (iconify-frame)))
(global-set-key (kbd "C-x C-c") 'server-edit)
(defalias 'exit 'save-buffers-kill-emacs)

