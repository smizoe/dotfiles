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
    auto-complete
    auto-install
    bm
    emacs-server
    helm
    goto-chg
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



;; flymake and flycheck
;; (package-install 'flycheck)
;; (package-install 'flymake-cursor)
(require 'flymake)
(require 'flycheck)
(require 'flymake-cursor)
(add-hook 'flymake-mode-hook (lambda ()
                          (local-set-key (kbd "C-c C-n") 'flymake-goto-next-error)))
(add-hook 'flymake-mode-hook (lambda ()
                          (local-set-key (kbd "C-c C-p") 'flymake-goto-prev-error)))
(add-hook 'flycheck-mode-hook (lambda ()
                          (local-set-key (kbd "C-c C-n") 'flycheck-next-error)))
(add-hook 'flycheck-mode-hook (lambda ()
                          (local-set-key (kbd "C-c C-p") 'flycheck-previous-error)))

(add-hook 'c-mode-hook 'flycheck-mode)
(add-hook 'yaml-mode-hook 'flycheck-mode)
(add-hook 'python-mode-hook 'flycheck-mode)




;;;;;;;;;;;;;;;;;;;;
;; yasnippet mode ;;
;;;;;;;;;;;;;;;;;;;;

;; (package-install 'yasnippet)
;; (install-elisp-from-emacswiki "yasnippet-config.el")
;; I've made a modification to the yasnippet-config.el
(add-to-list 'load-path "~/.emacs.d/elisp/yasnippet-config")
(require 'yasnippet)
(yas-global-mode 1)

(require 'yasnippet-config)
(define-sequential-command kill-ring-save-x
  kill-ring-save yas/register-oneshot-snippet)
(define-key esc-map "w" 'kill-ring-save-x) ; M-w
(define-key global-map "\C-x\C-y" 'yas/expand-oneshot-snippet)


