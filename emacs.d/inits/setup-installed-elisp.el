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
    (with-eval-after-load 'evil
      (progn
        (evil-leader/set-key
          "mm" 'bm-toggle
          "mn" 'bm-next
          "mp" 'bm-previous
          )
        )
      )
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


;;;;;;;;;;;;;;;;;;
;; evil-numbers ;;
;;;;;;;;;;;;;;;;;;
(use-package evil-numbers
  :ensure t
  :config
  (with-eval-after-load 'evil
    (progn
      (define-key evil-normal-state-map (kbd "C-a") 'evil-numbers/inc-at-pt)
      (define-key evil-normal-state-map (kbd "C-x") 'evil-numbers/dec-at-pt)
      )
    )
  )

;;;;;;;;;;;;;
;; helm-ag ;;
;;;;;;;;;;;;;

(use-package helm-ag
  :ensure t
  :init
  (custom-set-variables
    '(helm-ag-base-command "rg --vimgrep --no-heading --with-filename")
    )
  :config
  (progn
    (evil-leader/set-key
      "sr" 'helm-do-ag-project-root
      "sf" 'helm-do-ag-this-file
      )
    )
  )

;;;;;;;;;;;;;;;;
;; helm-gtags ;;
;;;;;;;;;;;;;;;;

(use-package helm-gtags
  :ensure t
  :init
  (custom-set-variables
    '(helm-gtags-suggested-key-mapping t)
    )
  :config
  (progn
    (cl-loop for tgt-mode in '(emacs-lisp-mode-hook c++-mode-hook js2-mode-hook java-mode-hook) do
      (add-hook tgt-mode 'helm-gtags-mode)
      )
    )
  )

(defun gtags-root-dir ()
  "Returns GTAGS root directory or nil if doesn't exist."
  (with-temp-buffer
    (if (zerop (call-process "global" nil t nil "-pr"))
        (buffer-substring (point-min) (1- (point-max)))
      nil)))

(defun gtags-update ()
  "Make GTAGS incremental update"
  (interactive)
  (start-process "global" nil nil nil "-u" "--gtagslabel" "pygments"))

(defun gtags-update-single(filename)
  "Update Gtags database for changes in a single file"
  (interactive)
  (start-process "update-gtags" "update-gtags" "bash" "-c" (concat "cd " (gtags-root-dir) " ; gtags --gtagslabel pygments --single-update " filename )))

(defun gtags-update-current-file()
  (interactive)
  (defvar filename)
  (setq filename (replace-regexp-in-string (gtags-root-dir) "." (buffer-file-name (current-buffer))))
  (gtags-update-single filename)
  (message "Gtags updated for %s" filename))

(defun gtags-update-hook()
  "Update GTAGS file incrementally upon saving a file"
  (when (gtags-root-dir)
    (gtags-update-current-file)))

(defun gtags-setup ()
  (interactive)
  (if (gtags-root-dir)
      (message "gtags-root-dir exists: %s" (gtags-root-dir))
    (let ((project-root (helm-ag--project-root)))
      (if (y-or-n-p (format "Would you like to run gtags from %s?" project-root))
          (let ((proc-name (format "setup-gtags: %s" project-root)))
            (start-process proc-name proc-name "bash" "-c" (concat "cd " project-root "; gtags --gtagslabel pygments"))
            )
          )
      )
    )
  )

(add-hook 'after-save-hook #'gtags-update-hook)

;;;;;;;;;;;;;;;;;;;;;
;; helm-projectile ;;
;;;;;;;;;;;;;;;;;;;;;

(use-package helm-projectile
  :ensure t
  :config
  (progn
    (with-eval-after-load 'projectile
      (helm-projectile-on)
      )
    (with-eval-after-load 'evil-leader
      (evil-leader/set-key
        "ps" 'helm-projectile-switch-project
        "pe" 'helm-projectile-find-file
        "pf" 'helm-projectile-find-file-in-known-projects
        "pxf" 'helm-projectile-recentf
        "pd" 'helm-projectile-find-dir
        )
      )
    )
  :pin melpa-stable
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

(setq woman-cache-filename (expand-file-name "~/.emacs.d/.wmncache.el"))
(with-eval-after-load 'evil-leader
  (evil-leader/set-key
    "man" 'woman
    )
  )


(provide 'setup-installed-elisp)
