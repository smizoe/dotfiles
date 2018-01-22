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
    '(helm-ag-base-command "rg --vimgrep --color never --no-heading")
    )
  :config
  (progn
    (evil-leader/set-key
      "sr" 'helm-do-ag-project-root
      "sf" 'helm-do-ag-this-file
      "sb" 'helm-do-ag-buffers
      )
    )
  )

;;;;;;;;;;;;;;;
;; helm-dash ;;
;;;;;;;;;;;;;;;

(use-package helm-dash
  :ensure t

  :config
  (progn
    (with-eval-after-load 'w3m
      (progn
        (let* (
               (dasht-docsets-dir (getenv "DASHT_DOCSETS_DIR"))
               (docsets-path (cond
                              ((or (null dasht-docsets-dir) (= (length dasht-docsets-dir) 0)) "~/.local/share/Zeal/Zeal/docsets")
                              (t dasht-docsets-dir)
                              ))
               )
          (mkdir docsets-path t)
          (custom-set-variables
          '(helm-dash-docsets-path docsets-path)
          '(helm-dash-browser-func 'w3m-browse-url)
          )
        )
        ;; TODO: somehow rename installed directories (e.g., if we install Ruby_2, this makes Ruby.docset directory instead of Ruby_2.docset directory)
        (let* ((installed-docsets
                (mapcar 'intern
                        (append
                          (helm-dash-installed-docsets)
                          (mapcar
                            (lambda (str)
                              (replace-regexp-in-string " " "_" str)
                              )
                            (helm-dash-installed-docsets)
                            )
                          )
                  )
                )
                (install-targets (set-difference
                                  '(R Bootstrap_4 Ruby_2 Python_2 Python_3 React Redis Rust Pandas
                                    SQLAlchemy SQLite SciPy NumPy Java_SE8 Markdown JavaScript Ansible)
                                  installed-docsets
                                  ))
                (install-targets-from-user (set-difference
                                        '(scikit-learn)
                                        installed-docsets
                                        ))
                )
            (cl-loop for target in install-targets do
              (helm-dash-async-install-docset (symbol-name target))
              )
            (cl-loop for target in install-targets-from-user do
              (helm-dash-install-user-docset (symbol-name target))
              )
          )
        (setq helm-dash-common-docsets (helm-dash-installed-docsets))
        (let ((hook-name-docsets-alist
              '(
                (ruby-mode-hook . ("Ruby_2"))
                (ess-mode-hook . ("R"))
                (python-mode-hook . ("Python 3" "Pandas" "SciPy" "NumPy" "scikit-learn" "SQLAlchemy"))
                (ein:notebook-mode-hook . ("R" "Python 3" "Pandas" "SciPy" "NumPy" "scikit-learn" "SQLAlchemy"))
                (js2-mode-hook . ("JavaScript" "React"))
                (yaml-mode-hook . ("Ansible"))
                )))
              (cl-loop for pair in hook-name-docsets-alist do
                      (let (
                            (hook-name (car pair))
                            (docsets (cdr pair))
                            )
                        (add-hook hook-name
                                  `(lambda ()
                                    (setq-local helm-dash-docsets ',docsets)
                                    )
                                  )
                        )
                      )
            )
        )
      )
    )
  (with-eval-after-load 'evil-leader
    (evil-leader/set-key
      "ld" 'helm-dash-at-point ;;lookup doc
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
  (start-process "global" nil "global" "-u" "--gtagslabel" "pygments"))

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
  (progn
    (setq
      python-shell-interpreter "ipython"
      python-shell-interpreter-args "--simple-prompt -i"
      python-shell-prompt-regexp "In \\[[0-9]+\\]: "
      python-shell-prompt-output-regexp "Out\\[[0-9]+\\]: "
      python-shell-completion-setup-code
        "from IPython.core.completerlib import module_completion"
      python-shell-completion-module-string-code
        "';'.join(module_completion('''%s'''))\n"
      python-shell-completion-string-code
      "';'.join(get_ipython().Completer.all_completions('''%s'''))\n"
      )
    (defun find-pipenv-venv-at (directory)
      "Find out a virtual environment that is associated with DIRECTORY.
If pipenv finds one, returns the path to the virtual environment.
If DIRECTORY is nil or it cannot, return nil."
      (when directory
        (let* (
               ;; with the following, we avoid using the current environment variables,
               ;; since pipenv uses VIRTUAL_ENV env. variable if it exists and pyvenv sets it
               (process-environment initial-environment)
               (candidate
               (replace-regexp-in-string "\n\\'" ""
                                         (shell-command-to-string
                                          (concat
                                           "cd " directory
                                           " && pipenv --venv 2>/dev/null"
                                           )
                                          )
                                         )
               ))
          (unless (string-empty-p candidate)
            candidate
            )
          )
        )
      )
    (defun find-pipenv-venv-for (buffer-or-name)
      "Find out a virtual environment that works with BUFFER-OR-NAME created by pipenv."
      (interactive "bThe buffer to find out a virtualenv by pipenv for: ")
      (let* (
             (default-venv (concat (file-name-as-directory (getenv "HOME")) "venv"))
             (buffer (get-buffer buffer-or-name))
             (buf-file-name (buffer-file-name buffer))
             (pipenv-file-dir
              (let ((file-dir (when buf-file-name (file-name-directory buf-file-name))))
                (find-pipenv-venv-at file-dir)
                )
              )
             (pipenv-projectile-root (when (projectile-project-p)
                                         (find-pipenv-venv-at (projectile-project-root))
                                         ))
             (target-venv (or pipenv-file-dir pipenv-projectile-root default-venv))
             )
        (message "using the virtual environment at %s" target-venv)
        target-venv
        )
      )
    (defun advice-pyvenv-pipenv-venv (& rest r)
      (let ((target-venv-dir (find-pipenv-venv-for (current-buffer))))
        (setq-local python-shell-buffer-name (concat "Python@" target-venv-dir))
        (pyvenv-activate target-venv-dir)
        )
      )
    (add-hook 'python-mode-hook
              ;; we assume that each project has bin directory and it contains activate script generated by venv module
              (lambda ()
                (pyvenv-mode t)
                (unless (advice-function-member-p #'advice-pyvenv-pipenv-venv #'run-python)
                  (advice-add #'run-python :before #'advice-pyvenv-pipenv-venv)
                  )
                )
              )
    )
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

;;;;;;;;;;;;;;;
;; emacs-w3m ;;
;;;;;;;;;;;;;;;

(use-package w3m
  :ensure t
  :init
  (custom-set-variables
   '(w3m-use-cookies t)
   )
  :config
  (progn
    (add-hook 'w3m-mode-hook (lambda ()
                              (evil-normal-state)
                              )
              )
    (with-eval-after-load 'evil-leader
      (evil-leader/set-key
        "sw" 'w3m-search
        )
      )
    )
  )

;;;;;;;;;;;;;;;;;;;;;;;;;;
;; ess-smart-underscore ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;

(use-package ess-smart-underscore
  :ensure t
  :pin melpa-stable
  )


;;;;;;;;;;;;;;
;; polymode ;;
;;;;;;;;;;;;;;

(use-package polymode
  :ensure t
  :pin melpa-stable
  :config
  (progn
    (require 'poly-R)
    (require 'poly-markdown)
    (add-to-list 'auto-mode-alist '("\\.Rmd" . poly-markdown+r-mode))
    )
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
