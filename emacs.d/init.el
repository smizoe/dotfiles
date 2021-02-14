;;; init.el ---                                      -*- lexical-binding: t; -*-
(require 'cl)

;; settings to modularize settings
;; see http://stackoverflow.com/questions/2079095/how-to-modularize-an-emacs-configuration
(defconst user-init-dir
  (file-name-as-directory
   (cond ((boundp 'user-emacs-directory)
          user-emacs-directory)
         ((boundp 'user-init-directory)
          user-init-directory)
         (t "~/.emacs.d/")))
  )

(defvar user-additional-load-path
  `(
    ,(concat user-init-dir "personal-config")
    ,(concat user-init-dir "package-config")
    )
  )
(cl-loop for dir in user-additional-load-path do
         (add-to-list 'load-path dir)
         )

(require 'personal-preferences)
(require 'personal-functions)

(package-required-setup)

(cond
 ((remove-if (lambda (i)
               (or (string-match-p "\\(vboxnet\\|docker\\).*" i)
                   (member 'loopback (nth 4 (network-interface-info i)))))
                          (mapcar 'car (network-interface-list)))
;; network connection exists. install use-package
  (message "%s" "refreshing package database...")
  (package-refresh-contents)
  (message "%s" " done.")
  (when (not (package-installed-p 'use-package))
    (package-install 'use-package)
    )
  )
 ;; no network connection; setup load-path and proceed without installing packages
  (t
   ()
    )
  )



(cl-loop for name in (mapcar 'file-name-sans-extension (directory-files (concat user-init-dir "package-config") nil "^.+\\.el$")) do
         (require (intern name))
         )

(provide 'init)
