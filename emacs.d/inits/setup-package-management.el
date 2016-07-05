;; package manager
(require 'package)
(setq package-archives '(("gnu" . "http://elpa.gnu.org/packages/")
                         ("marmalade" . "http://marmalade-repo.org/packages/")
                         ("melpa" . "https://melpa.org/packages/")
                         ("melpa-stable" . "https://stable.melpa.org/packages/")
                         ("org" . "http://orgmode.org/elpa/")))
(package-initialize)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; elisp installer configuration ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defvar targets-package-install
  '(
    auto-install
    use-package
  )
  "a list of packages that should be installed using package-install")

(defvar targets-auto-install-batch
  '(sequential-command)
  "a list of elisps that should be installed using auto-install-batch")

(defvar targets-install-elisp-from-emacswiki
  '(
    open-junk-file
    point-undo
    recentf-ext
    yasnippet-config
  )
  "a list of elisps that should be installed using install-elisp-from-emacswiki")

(defconst user-auto-install-dir
          (concat (file-name-as-directory user-init-dir) "auto-install"))
(message "user-auto-install-dir: %s" user-auto-install-dir)

(when (not (every 'package-installed-p targets-package-install))
  (message "%s" "refreshing package database...")
  (package-refresh-contents)
  (message "%s" " done.")
  )
;; install yet-to-be-installed packages
(dolist (name targets-package-install)
  (when (not (package-installed-p name))
    (message "%s" (concat "start installing package: " (symbol-name name)))
    (package-install name)
    (message "%s" (concat "finished installing package: " (symbol-name name)))
  )
  )


;; auto-install.el
;; (package-install 'auto-install)
(add-to-list 'load-path "~/.emacs.d/elisp/auto-install/")
;; add to load-path the directory where elisps installed by auto-install reside
(add-to-list 'load-path "~/.emacs.d/auto-install/")
(require 'auto-install)
(auto-install-update-emacswiki-package-name t)
(auto-install-compatibility-setup)
(setq ediff-window-setup-function 'ediff-setup-windows-plain)

(dolist (name targets-auto-install-batch)
  (let ((elisp-name (concat (symbol-name name) ".el")))
    (cond
      ((not (file-exists-p (concat (file-name-as-directory user-auto-install-dir) elisp-name)))
        (message "%s" (concat "start installing elisp: " elisp-name))
        (auto-install-batch (symbol-name name))
        (message "%s" (concat "finished installing elisp: " elisp-name))
      )
      (t (message "skipped installation of %s since it already exists" elisp-name))
    )))

;; install elisps using install-elisp-from-emacswiki if they do not exist
(dolist (name targets-install-elisp-from-emacswiki)
  (let ((elisp-name (concat (symbol-name name) ".el")))
    (cond
      ((not (file-exists-p (concat (file-name-as-directory user-auto-install-dir) elisp-name)))
        (message "%s" (concat "start installing elisp: " elisp-name))
        (install-elisp-from-emacswiki elisp-name)
        (message "%s" (concat "finished installing elisp: " elisp-name))
      )
      (t (message "skipped installation of %s since it already exists" elisp-name))
      )))

(provide 'setup-package-management)
