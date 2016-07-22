(require 'cl)
;; settings to modularize settings
;; see http://stackoverflow.com/questions/2079095/how-to-modularize-an-emacs-configuration
(defconst user-init-dir
  (cond ((boundp 'user-emacs-directory)
         user-emacs-directory)
        ((boundp 'user-init-directory)
         user-init-directory)
        (t "~/.emacs.d/")))


(defun load-user-file (file)
  (interactive "f")
  "Load a file in current user's configuration directory"
  (load-file (expand-file-name file (concat user-init-dir "inits"))))

(defun simulate-key-press (key)
    "Return a command that pretends KEY was presssed.
KEY must be given in `kbd' notation."
    `(lambda () (interactive)
       (setq prefix-arg current-prefix-arg)
            (setq unread-command-events (listify-key-sequence (read-kbd-macro ,key)))))

;; we need to loading package.el first so that elpa directory is on load-path
(defvar user-file-load-targets
  '(
    setup-package-management
    setup-installed-elisp
    setup-major
    setup-minor
    setup-personal-configurations
    setup-jabber
    )
  "target file names (basename without el) to be loaded by load-user-file")

(cond
 ((remove-if (lambda (i)
               (or (string-match-p "\\(vboxnet\\|docker\\).*" i)
                   (member 'loopback (nth 4 (network-interface-info i)))))
                          (mapcar 'car (network-interface-list)))
;; network connection exists. do nothing
    ()
    )
 ;; no network connection; setup load-path and proceed without installing packages
  (t (progn
       (message "skipping package install since there seems to be no network connection")
       (setq user-file-load-targets (delete 'setup-package-management user-file-load-targets))
       (require 'package)
       (setq package-archives '(("gnu" . "http://elpa.gnu.org/packages/")
                                ("marmalade" . "http://marmalade-repo.org/packages/")
                                ("melpa-stable" . "https://stable.melpa.org/packages/")
                                ("melpa" . "https://melpa.org/packages/")
                                ("org" . "http://orgmode.org/elpa/")))
       (package-initialize)
       (add-to-list 'load-path "~/.emacs.d/auto-install/")
       )
    )
  )

(let ((inits-dir (concat user-init-dir "inits")))
  (add-to-list 'load-path inits-dir)
  (dolist (name user-file-load-targets 'dummy)
    (require name)
    ))
