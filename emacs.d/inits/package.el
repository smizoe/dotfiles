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


;; targets of package installation by package-install
;; see http://batsov.com/articles/2012/02/19/package-management-in-emacs-the-good-the-bad-and-the-ugly/
(defvar targets-package-install
  '(
    auto-install
    auto-complete
    edit-server
    egg
    ess
    flycheck
    flymake-cursor
    go-mode
    helm
    jabber
    migemo
    org
    org-plus-contrib
    pyvenv
    viewer
    yasnippet
    yatex
  )
  "a list of packages that should be installed using package-install")

(defvar targets-auto-install-batch
  '(sequential-command)
  "a list of elisps that should be installed using auto-install-batch")

(defvar targets-install-elisp
  '(
    http://jblevins.org/projects/markdown-mode/markdown-mode.el
    http://cvs.savannah.gnu.org/viewvc/*checkout*/bm/bm/bm.el
  )
  "a list of elisps that should be installed using install-elisp")

(defvar targets-install-elisp-from-emacswiki
  '(
    goto-chg.el
    key-chord.el
    open-junk-file.el
    point-undo.el
    recentf-ext.el
    yasnippet-config.el
  )
  "a list of elisps that should be installed using install-elisp-from-emacswiki")