;;;;;;;;;;;;;;;
;; adoc-mode ;;
;;;;;;;;;;;;;;;

(use-package adoc-mode
  :ensure t
  :config
  (add-to-list 'auto-mode-alist (cons "\\.adoc\\'" 'adoc-mode))
  )

;;;;;;;;;;;;;;
;; css-mode ;;
;;;;;;;;;;;;;;

(custom-set-variables
 '(css-indent-offset 2)
 )

;;;;;;;;;;;;;;;;;;;;;
;; dockerfile-mode ;;
;;;;;;;;;;;;;;;;;;;;;
(use-package dockerfile-mode
  :ensure t
  :config
  (progn
    (add-hook 'dockerfile-mode-hook
              (lambda ()
                (progn
                  (add-to-list 'auto-mode-alist '("Dockerfile$" . dockerfile-mode))
                  (define-abbrev-table 'dockerfile-mode-abbrev-table
                    '(
                      ("from" "FROM")
                      ("maintainer" "MAINTAINER")
                      ("run" "RUN")
                      ("cmd" "CMD")
                      ("label" "LABEL")
                      ("expose" "EXPOSE")
                      ("env" "ENV")
                      ("add" "ADD")
                      ("copy" "COPY")
                      ("entrypoint" "ENTRYPOINT")
                      ("volume" "VOLUME")
                      ("user" "USER")
                      ("workdir" "WORKDIR")
                      ("arg" "ARG")
                      ("onbuild" "ONBUILD")
                      ("stopsignal" "STOPSIGNAL")
                      ("arg" "ARG")
                      ("healthcheck" "HEALTHCHECK")
                      ("shell" "SHELL")
                      )
                    "Capitalize instructoins automatically"
                    :regexp "^\\(?:ONBUILD[[:space:]]+\\)?\\([a-zA-Z]+\\)"
                    )
                  )
                )
      )
    )
  )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; ein (emacs ipython notebook) ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(use-package ein
  :ensure t
  :pin melpa-stable
  :config
  (progn
    (add-hook 'ein:notebook-mode-hook (lambda () (setq-local before-save-hook nil)))
    (with-eval-after-load 'evil
      (evil-define-key 'normal ein:notebook-mode-map
        "\\j" 'ein:worksheet-goto-next-input
        "\\k" 'ein:worksheet-goto-prev-input
        "\\w" 'ein:notebook-save-notebook-command
        "\\b" 'ein:worksheet-insert-cell-below
        "\\B" (lambda ()
                (interactive)
                (setq current-prefix-arg t)
                (call-interactively 'ein:worksheet-insert-cell-below)
                )
        "\\a" 'ein:worksheet-insert-cell-above
        "\\A" (lambda ()
                (interactive)
                (setq current-prefix-arg t)
                (call-interactively 'ein:worksheet-insert-cell-above)
                )
        "\\d" 'ein:worksheet-delete-cell
        "\\c" 'ein:worksheet-clear-output
        "\\z" 'ein:notebook-kernel-interrupt-command
        "\\o" 'ein:console-open
        (concat "\\s" (kbd "RET")) (lambda ()
                                     (interactive)
                                     (run-python)
                                     (let* ((cell (ein:worksheet-get-current-cell :cell-p #'ein:codecell-p))
                                            (code (ein:cell-get-text cell))
                                           )
                                       (python-shell-send-string code)
                                       )
                                     )
        (kbd "<f5>") 'ein:worksheet-execute-all-cell
        (kbd "RET" ) 'ein:worksheet-execute-cell
        (kbd "SPC") 'ein:worksheet-execute-cell-and-goto-next
        )
      )
    )
  )

;;;;;;;;;;;;;;
;; org-mode ;;
;;;;;;;;;;;;;;



;; in installing org mode, you must use a 'fresh' emacs session
;; that has no org mode feature turned on.
;; (package-install 'org)
;; (package-install 'org-plus-contrib)
;; or
;; (package-list-packages)
;; and install 'org

;;(require 'org)
;;(require 'ox-md)
;;(require 'ox-odt)
;;(add-to-list 'org-export-backends 'md)
(use-package org
  :ensure org-plus-contrib
  :config
  (progn
    (setq org-use-fast-todo-selection t)
    (setq org-todo-keywords
          '(
            (sequence "TODO(t)" "STARTED(s)" "WAITING(w)" "|" "DONE(x)" "CANCEL(c)")
            (sequence "APPT(a)" "|" "DONE(x)" "CANCEL(c)")
            ))
    (with-eval-after-load 'evil
      (evil-define-key 'normal org-mode-map
        (kbd "TAB") 'org-cycle
        "\\cc" 'org-ctrl-c-ctrl-c
        )
      )
    )
  )


;;;;;;;;;;;;;;;
;; json mode ;;
;;;;;;;;;;;;;;;

(use-package json-mode
  :ensure t
  :config
  (progn
    (setq json-reformat:indent-width 2)
    (with-eval-after-load 'evil
      (evil-define-key 'normal json-mode-map
        "\\rf" 'json-mode-beautify
        "\\p" 'json-mode-show-path
        )
      (evil-define-key 'visual json-mode-map
        "\\rf" 'json-mode-beautify
        )
      )
    )
  )

;;;;;;;;;;;;;;
;; js2 mode ;;
;;;;;;;;;;;;;;

(use-package js2-mode
  :ensure t
  :config
  (progn
      (add-to-list 'auto-mode-alist '("\\.js$" . js2-jsx-mode))
      (custom-set-variables
        '(js2-strict-missing-semi-warning nil)
        '(js2-missing-semi-one-line-override t)
        '(js2-basic-offset 2)
        )
    )
  )

;;;;;;;;;;;;;;;;
;; magit mode ;;
;;;;;;;;;;;;;;;;

(use-package magit
  :ensure t
  )

;;;;;;;;;;;;;;;;;;;
;; markdown mode ;;
;;;;;;;;;;;;;;;;;;;

;; (install-elisp "http://jblevins.org/projects/markdown-mode/markdown-mode.el")

;;;(autoload 'markdown-mode "markdown-mode.el" "Major mode for editing Markdown files" t)

(use-package markdown-mode
  :ensure t
  :commands (markdown-mode gfm-mode)
  :mode (("README\\.md\\'" . gfm-mode)
         ("\\.md\\'" . markdown-mode)
         ("\\.markdown\\'" . markdown-mode))
  :init
    (progn
      (setq markdown-command "multimarkdown")
      (global-set-key "\C-cm" 'markdown-preview-file)
      )
  )

;; from http://support.markedapp.com/kb/how-to-tips-and-tricks/marked-bonus-pack-scripts-commands-and-bundles

(defun markdown-preview-file ()
  "run Marked on the current file and revert the buffer"
  (interactive)
  (shell-command
   (format "open -a /Applications/Marked.app %s"
       (shell-quote-argument (buffer-file-name))))
)


;;;;;;;;;;;;;;;
;; sql mode  ;;
;;;;;;;;;;;;;;;
;; use sql mode in hql
(add-to-list 'auto-mode-alist '("\\.hql$" . sql-mode))

;;;;;;;;;;;;;;;
;; ruby mode ;;
;;;;;;;;;;;;;;;

(add-hook 'ruby-mode-hook 'flycheck-mode)

;;;;;;;;;;;;;
;; go mode ;;
;;;;;;;;;;;;;

;; (package-install 'go-mode)
;; after setting GOPATH and appending its lib directory, do the following:
;; go get -u github.com/dougm/goflymake
;; the following enables goflymake

(use-package go-mode
  :ensure t
  :init
    (add-hook 'go-mode-hook
      (lambda ()
        (progn
          (add-to-list 'load-path "~/go/src/github.com/dougm/goflymake")
          (require 'go-flymake)
          (require 'go-flycheck)
          (local-set-key (kbd "C-c C-r") 'go-remove-unused-imports)
          (local-set-key (kbd "C-c i") 'go-goto-imports)
          )
        )
      )
  )

;;;;;;;;;;;;;;;;;
;; groovy-mode ;;
;;;;;;;;;;;;;;;;;

(use-package groovy-mode
  :ensure t
  :pin melpa-stable
)

;; web-mode

(use-package web-mode
  :init
  (progn
    (cl-loop for pattern in
             '("\\.html?$" "\\.j2$" "\\.erb$" "\\.css$")
             do (add-to-list 'auto-mode-alist `(,(symbol-value 'pattern) . web-mode))
             )
    )
  :pin melpa-stable
  :ensure t
  )

;; yatex

(use-package yatex
  :ensure t
  :init
  (progn
    (setq auto-mode-alist (cons '("\\.tex$" . yatex-mode) auto-mode-alist))
    (autoload 'yatex-mode "yatex" "Yet Another LaTeX mode" t)

    (add-hook 'skk-mode-hook
      (lambda ()
        (if (eq major-mode 'yatex-mode)
      (progn
        (define-key skk-j-mode-map "\\" 'self-insert-command)
        (define-key skk-j-mode-map "$" 'YaTeX-insert-dollar)
        ))
        ))
    (with-eval-after-load 'evil
      (progn
        ;;;; yatex
        (evil-define-key 'normal YaTeX-mode-map
          "\\" (simulate-key-press YaTeX-prefix)
          )
        (evil-define-key 'visual YaTeX-mode-map
          "\\" (simulate-key-press YaTeX-prefix)
          )

        ;; yahtml
        (evil-define-key 'normal yahtml-mode-map
          "\\" (simulate-key-press yahtml-prefix)
          )
        (evil-define-key 'visual yahtml-mode-map
          "\\" (simulate-key-press yahtml-prefix)
          )
        )
      )
    )
  )


;; ess
;; (package-install 'ess)
(use-package ess
  :ensure t
  :init
  (progn
    (require 'ess-site)
    (define-abbrev-table 'ess-mode-abbrev-table
      '(
        ("pp" "%>%" nil 0)
      ))
    (dolist (hook '(ess-mode-hook inferior-ess-mode-hook))
        (add-hook hook
                (lambda ()
                    (make-local-variable 'local-abbrev-table)
                    (setq local-abbrev-table ess-mode-abbrev-table)
                    )
                )
        )
    (with-eval-after-load 'evil
      (progn
        ;;; ess with evil
        (evil-define-key 'normal ess-mode-map
          "\\l" 'ess-eval-line
          "\\aa" 'ess-load-file
          "\\ff" 'ess-eval-function
          "\\pp" 'ess-eval-paragraph
          "\\rh" 'ess-help
          "\\rf" 'R
          "\\gr" 'ess-switch-process
          )
        (evil-define-key 'visual ess-mode-map
          "\\ss" 'ess-eval-region
          )
        (evil-define-key 'normal ess-help-mode-map
          "q" 'ess-help-quit
          )
        (evil-leader/set-key-for-mode 'inferior-ess-mode-map
          "b" 'helm-buffers-list
          )
        )
      )
    )
  )

;;;;;;;;;;;;;;;;;
;; python-mode ;;
;;;;;;;;;;;;;;;;;

(with-eval-after-load 'evil
  (progn
    ;;; python-mode/ with evil
    (evil-define-key 'normal python-mode-map
      "\\l" (lambda (send-main)
              (interactive "P")
              (save-excursion
                (end-of-line)
                (let ((end (point)))
                  (beginning-of-line)
                  (python-shell-send-region (point) end send-main)
                  )
                )
              )
      "\\aa" 'python-shell-send-file
      "\\ff" 'python-shell-send-defun
      "\\pp" (lambda (send-main)
               (interactive "P")
               (save-excursion
                 (forward-paragraph)
                 (let ((end (point)))
                   (backward-paragraph)
                   (python-shell-send-region (point) end send-main)
                   )
                 )
               )
      "\\ph" 'python-eldoc-at-point
      "\\pf" 'run-python
      "\\ss" 'python-shell-switch-to-shell
      )
    (evil-define-key 'visual python-mode-map
      "\\ss" 'python-shell-send-region
      )
    )
  )

;;;;;;;;;;;;;;;
;; rust-mode ;;
;;;;;;;;;;;;;;;

(use-package rust-mode
  :init
  (progn
    (add-to-list 'auto-mode-alist '("\\.rs\\'" . rust-mode))
   )
  :ensure t
  :pin melpa-stable
  )

;;;;;;;;;;;;;;;;;;;
;; synonyms mode ;;
;;;;;;;;;;;;;;;;;;;
(use-package synonyms
  :ensure t
  :pin melpa
  :init
  (progn
    (custom-set-variables
      '(synonyms-file "~/.emacs.d/synonyms/mthesaur.txt")
      '(synonyms-cache-file "~/.emacs.d/synonyms/mthesaur.txt.cache")
      )
    )
  :config
  (progn
    (with-eval-after-load 'evil
      (evil-leader/set-key
        "ss" 'synonyms
        )
      )
    )
 )

;; nxml-mode
(add-to-list 'auto-mode-alist '("\\.pmml" . nxml-mode))

;; yaml-mode
(use-package yaml-mode
  :ensure t
  :commands (yaml-mode)
  :init
  (progn
    (add-to-list 'auto-mode-alist '("\\.yml" . yaml-mode))
    (add-to-list 'auto-mode-alist '("\\.yaml" . yaml-mode))
    )
  )

(provide 'setup-major)
;;; setup-major.el ends here
