;; targets of package installation by package-install
;; see http://batsov.com/articles/2012/02/19/package-management-in-emacs-the-good-the-bad-and-the-ugly/
(defvar targets-package-install
  '(
    auto-install
    auto-complete
    bm
    edit-server
    ess
    evil
    evil-magit
    evil-matchit
    flycheck
    flymake-cursor
    go-mode
    goto-chg
    helm
    key-chord
    jabber
    js2-mode
    magit
    markdown-mode
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
    https://raw.githubusercontent.com/timcharper/evil-surround/master/evil-surround.el
    https://raw.githubusercontent.com/yoshiki/yaml-mode/master/yaml-mode.el
    )
  "a list of elisp that should be installed using install-elisp")

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
(message "user-aut-install-dir: %s" user-auto-install-dir)

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

(dolist (name targets-install-elisp)
  (let ((elisp-name (concat (file-name-nondirectory (symbol-name name)))))
    (cond
      ((not (file-exists-p (concat (file-name-as-directory user-auto-install-dir) elisp-name)))
       (message "%s" (concat "start installing elisp: " elisp-name))
       (install-elisp (symbol-name name))
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

(provide 'install-packages)
