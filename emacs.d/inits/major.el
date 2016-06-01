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

(require 'org)
;;(require 'ox-md)
(require 'ox-odt)
;;(add-to-list 'org-export-backends 'md)
(setq org-use-fast-todo-selection t)
(setq org-todo-keywords
      '(
        (sequence "TODO(t)" "STARTED(s)" "WAITING(w)" "|" "DONE(x)" "CANCEL(c)")
        (sequence "APPT(a)" "|" "DONE(x)" "CANCEL(c)")
        ))

;;;;;;;;;;;;;;;;;;;
;; markdown mode ;;
;;;;;;;;;;;;;;;;;;;

;; (install-elisp "http://jblevins.org/projects/markdown-mode/markdown-mode.el")

(autoload 'markdown-mode "markdown-mode.el" "Major mode for editing Markdown files" t)

;; from http://support.markedapp.com/kb/how-to-tips-and-tricks/marked-bonus-pack-scripts-commands-and-bundles

(defun markdown-preview-file ()
  "run Marked on the current file and revert the buffer"
  (interactive)
  (shell-command
   (format "open -a /Applications/Marked.app %s"
       (shell-quote-argument (buffer-file-name))))
)
(global-set-key "\C-cm" 'markdown-preview-file)



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
(add-to-list 'load-path "~/go/src/github.com/dougm/goflymake")
(require 'go-flymake)
(require 'go-flycheck)

(add-hook 'go-mode-hook (lambda ()
                          (local-set-key (kbd "C-c C-r") 'go-remove-unused-imports)))
(add-hook 'go-mode-hook (lambda ()
                          (local-set-key (kbd "C-c i") 'go-goto-imports)))

;; yatex

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

;; ess
;; (package-install 'ess)
(require 'ess-site)


;; yaml-mode
(require 'yaml-mode)
(add-to-list 'auto-mode-alist '("\\.yml" . yaml-mode))
(add-to-list 'auto-mode-alist '("\\.yaml" . yaml-mode))

;; evil
(require 'evil)
(evil-mode 1)

(require 'evil-surround)
(global-evil-surround-mode 1)

(require 'evil-magit)

(require 'evil-matchit)
(global-evil-matchit-mode 1)


;; esc quits
;; http://stackoverflow.com/questions/8483182/evil-mode-best-practice


(define-key evil-normal-state-map "[b" 'next-buffer)
(define-key evil-normal-state-map "]b" 'previous-buffer)

;; commands that use 'leader' key (= comma)
(define-key evil-visual-state-map ",r." (concat ":normal." (kbd "RET")))
;; edit vimrc = evil related config
(define-key evil-normal-state-map ",ev" (concat ":e ~/.emacs.d/inits/major.el" (kbd "RET")))
(define-key evil-normal-state-map ",sv" (lambda () (interactive) (load-user-file "major.el")))

(define-key evil-normal-state-map ",N" 'linum-mode)
(define-key evil-normal-state-map ",P" 'electric-indent-mode)
(define-key evil-normal-state-map ",nh" 'evil-ex-nohighlight)
(define-key evil-normal-state-map ",b" 'helm-buffers-list)

;;;; magit
(define-key evil-normal-state-map ",git" 'magit-status)

;;;;flycheck
(define-key evil-normal-state-map ",c" (simulate-key-press "C-c !"))

;;;;auto-complete
;; enable auto-complete by CTRL-P and CTRL-N
(define-key evil-insert-state-map "\C-p" 'ac-previous)
(define-key evil-insert-state-map "\C-n" 'ac-next)


;;;;yasnippet
;; see yasnippet for the following 3
(define-key evil-visual-state-map ",os" 'yas-oneshot-snippet) ;; register
(define-key evil-normal-state-map ",oe" 'yas-oneshot-snippet) ;; expand
(define-key evil-insert-state-map "\C-k" nil) ;; remove keymap; used in yas-minor-mode-map
(add-hook 'yas-before-expand-snippet-hook (lambda () (evil-insert-state)))

;; set any custom variables for major modes
(custom-set-variables
    '(evil-search-module 'evil-search)
    '(evil-want-C-u-scroll t)
    )
