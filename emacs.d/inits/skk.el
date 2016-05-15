;; skk
(require 'skk)
(global-set-key "\C-x\C-j" 'skk-mode)
(global-set-key "\C-xj" 'skk-auto-fill-mode)
(global-set-key "\C-xt" 'skk-tutorial)

(setq skk-server-portnum 1178)
(setq skk-server-host "localhost")
;;; dired-x occupies C-x C-j and the following takes the key back
(add-hook 'dired-load-hook
          (lambda ()
            (load "dired-x")
            (global-set-key "\C-x\C-j" 'skk-mode)
            ))
