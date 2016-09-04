;;;;;;;;;;;
;; bm.el ;;
;;;;;;;;;;;

;; (install-elisp "http://cvs.savannah.gnu.org/viewvc/*checkout*/bm/bm/bm.el")
(use-package bm
  :ensure t
  :init
  (progn
    (setq-default bm-buffer-persistence nil)
    (setq bm-restore-repository-on-load t)
    )
  :config
  (progn
    (add-hook 'find-file-hooks 'bm-buffer-restore)
    (add-hook 'kill-buffer-hook 'bm-buffer-save)
    (add-hook 'after-save-hook 'bm-buffer-save)
    (add-hook 'after-revert-hook 'bm-buffer-restore)
    )
  )
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; use emacs in server mode ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(server-start)
(defun iconify-emacs-when-server-is-done ()
  (unless server-clients (iconify-frame)))
(global-set-key (kbd "C-x C-c") 'server-edit)
(defalias 'exit 'save-buffers-kill-emacs)

;;;;;;;;;;;;;;;;;;;;
;; open-junk-file ;;
;;;;;;;;;;;;;;;;;;;;
(use-package open-junk-file
 :ensure t
 :init (setq open-junk-file-format "~/junk/%Y-%m/%Y-%m-%d_%H-%M-%S_")
 )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; point-undo (move back and forth to the previous or next position that the cursor existed) ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;; (install-elisp-from-emacswiki "point-undo.el")
(use-package point-undo
  :ensure t
  :init
  (progn
    (define-key global-map [f7] 'point-undo)
    ;; the following is (shift f7) on mac (at least for now)
    (define-key global-map "\M-[28~" 'point-redo)
    )
  )
;;(define-key global-map (kbd "S-<f7>") 'point-redo)

;; skk
(use-package ddskk
  :ensure t
  )

;; enable edit server for chrome extension
(use-package edit-server
  :ensure t
  :config (edit-server-start)
  )
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; goto-chg.el (move to recently edited position) ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; (install-elisp-from-emacswiki "goto-chg.el")
(use-package goto-chg
  :ensure t
  :config
  (progn
    (define-key global-map (kbd "<f8>") 'goto-last-change)
    ;;(define-key global-map (kbd "S-<f8>") 'goto-last-change-reverse)
    (global-set-key "\M-[29~" 'goto-last-change-reverse)
    )
  )
;;;;;;;;;;;;
;; migemo ;;
;;;;;;;;;;;;

;; (package-install 'migemo)
;;; you must install cmigemo independently of this elisp.
(use-package migemo
  :ensure t
  :config
  (progn
    (setq migemo-command "cmigemo")
    (setq migemo-options '("-q" "--emacs" "-i" "\a"))
    (setq migemo-dictionary "/usr/local/Cellar/cmigemo/20110227/share/migemo/utf-8/migemo-dict")
    (setq migemo-coding-system 'utf-8-unix)
    (setq migemo-user-dictionary nil)
    (setq migemo-regex-dictionary nil)
    (migemo-init)
    )
  )

;; python and venv
;; (package-install 'pyvenv)
(use-package pyvenv
  :ensure t
  :config
    (setq
    python-shell-interpreter "ipython"
    python-shell-interpreter-args ""
    python-shell-prompt-regexp "In \\[[0-9]+\\]: "
    python-shell-prompt-output-regexp "Out\\[[0-9]+\\]: "
    python-shell-completion-setup-code
      "from IPython.core.completerlib import module_completion"
    python-shell-completion-module-string-code
      "';'.join(module_completion('''%s'''))\n"
    python-shell-completion-string-code
      "';'.join(get_ipython().Completer.all_completions('''%s'''))\n")
    )


;;;;;;;;;;;;;;;;
;; recentf.el ;;
;;;;;;;;;;;;;;;;

(use-package recentf-ext
  :ensure t
  :init
  (progn
    (setq recentf-max-saved-items 500)
    (setq recentf-exclude '("/TAGS$" "/var/tmp/"))
    )
  )


;;;;;;;;;;;;;;;;;;;;;;;;
;; sequential command ;;
;;;;;;;;;;;;;;;;;;;;;;;;

(use-package sequential-command
  :ensure t
  )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; woman (yet another man command) ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(setq woman-manpath "~/.emacs.d/.wmncache.el")

(provide 'setup-installed-elisp)
