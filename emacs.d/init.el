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

;; we need to loading package.el first so that elpa directory is on load-path
(defvar user-file-load-targets
  '(
    package
    auto-install
    install-packages
    ;; package installation and setting up load-path finished
    auto-complete
    bm
    emacs-server
    helm
    goto-chg
    flycheck-and-flymake
    jabber
    open-junk-file
    major
    migemo
    personal
    point-undo
    pyvenv
    skk
    woman
    yasnippet
    )
  "target file names (basename without el) to be loaded by load-user-file")

(dolist (name user-file-load-targets 'dummy)
      (load-user-file (concat (symbol-name name) ".el")))

