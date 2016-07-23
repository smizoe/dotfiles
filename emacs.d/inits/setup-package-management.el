;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; elisp installer configuration ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defvar targets-package-install
  '(
    use-package
  )
  "a list of packages that should be installed using package-install")

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


(setq ediff-window-setup-function 'ediff-setup-windows-plain)
(provide 'setup-package-management)
