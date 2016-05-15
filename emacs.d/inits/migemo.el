;;;;;;;;;;;;
;; migemo ;;
;;;;;;;;;;;;

;; (package-install 'migemo)
;;; you must install cmigemo independently of this elisp.
(require 'migemo)
(setq migemo-command "cmigemo")
(setq migemo-options '("-q" "--emacs" "-i" "\a"))
(setq migemo-dictionary "/usr/local/Cellar/cmigemo/20110227/share/migemo/utf-8/migemo-dict")
(setq migemo-coding-system 'utf-8-unix)
(setq migemo-user-dictionary nil)
(setq migemo-regex-dictionary nil)
