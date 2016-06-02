;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; elisp installer configuration ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; package manager
(require 'package)
(setq package-archives '(("gnu" . "http://elpa.gnu.org/packages/")
                         ("marmalade" . "http://marmalade-repo.org/packages/")
                         ("melpa-stable" . "http://stable.melpa.org/packages/")
                         ("org" . "http://orgmode.org/elpa/")))
(package-initialize)

;; auto-install.el
;; (package-install 'auto-install)
(add-to-list 'load-path "~/.emacs.d/elisp/auto-install/")
;; add to load-path the directory where elisps installed by auto-install reside
(add-to-list 'load-path "~/.emacs.d/auto-install/")
(require 'auto-install)
(auto-install-update-emacswiki-package-name t)
(auto-install-compatibility-setup)
(setq ediff-window-setup-function 'ediff-setup-windows-plain)

(provide 'setup-package-management)
