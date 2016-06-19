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
(use-package org-plus-contrib
  :ensure t
  :config
  (progn
    (setq org-use-fast-todo-selection t)
    (setq org-todo-keywords
          '(
            (sequence "TODO(t)" "STARTED(s)" "WAITING(w)" "|" "DONE(x)" "CANCEL(c)")
            (sequence "APPT(a)" "|" "DONE(x)" "CANCEL(c)")
            ))
    )
  )


;;;;;;;;;;;;;;
;; js2 mode ;;
;;;;;;;;;;;;;;

(use-package js2-mode
  :ensure t
  :config (add-to-list 'auto-mode-alist '("\\.js$" . js2-mode))
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
    (progn
      (add-to-list 'load-path "~/go/src/github.com/dougm/goflymake")
      (require 'go-flymake)
      (require 'go-flycheck)

      (add-hook 'go-mode-hook (lambda ()
                                (local-set-key (kbd "C-c C-r") 'go-remove-unused-imports)))
      (add-hook 'go-mode-hook (lambda ()
                                (local-set-key (kbd "C-c i") 'go-goto-imports)))
      )
)

;; yatex

(use-package yatex
  :ensure t
  :init
  (progn
    (setq auto-mode-alist (cons '("\\.tex$" . yatex-mode) auto-mode-alist))
    (autoload 'yatex-mode "yatex" "Yet Another LaTeX mode" t)

    (setq auto-mode-alist (cons '("\\.html?$" . yahtml-mode) auto-mode-alist))
    (autoload 'yahtml-mode "yahtml" "Yet Another HTML mode" t)
    ;; (setq yahtml-www-browser "firefox")

    (add-hook 'skk-mode-hook
      (lambda ()
        (if (eq major-mode 'yatex-mode)
      (progn
        (define-key skk-j-mode-map "\\" 'self-insert-command)
        (define-key skk-j-mode-map "$" 'YaTeX-insert-dollar)
        ))
        ))
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
    )
  )

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
