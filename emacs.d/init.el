;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; use emacs in server mode ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(server-start)
(defun iconify-emacs-when-server-is-done ()
  (unless server-clients (iconify-frame)))
(global-set-key (kbd "C-x C-c") 'server-edit)
(defalias 'exit 'save-buffers-kill-emacs)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; elisp installer configuration ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; package manager
(require 'package)
;;(add-to-list 'package-archives '("melpa" . "http://melpa.milkbox.net/packages/") t)
;;(add-to-list 'package-archives '("marmalade" . "http://marmalade-repo.org/packages/"))
;;(add-to-list 'package-archives '("org" . "http://orgmode.org/elpa/") t)
(setq package-archives '(;("gnu" . "http://elpa.gnu.org/packages/")
                         ("marmalade" . "http://marmalade-repo.org/packages/")
                         ("melpa" . "http://melpa.milkbox.net/packages/")
                         ("org" . "http://orgmode.org/elpa/")))
(package-initialize)

;; auto-install.el
;; (package-install 'auto-install)
(add-to-list 'load-path "~/.emacs.d/elisp/auto-install/")
;; add to load-path the directory where elisps installed by auto-install reside
(add-to-list 'load-path "~/.emacs.d/auto-install/")
(require 'auto-install)
(auto-install-update-emacswiki-package-name t)
(auto-install-compatibility-setup)
(setq ediff-window-setup-function 'ediff-setup-windows-plain)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; personal configuration ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; delete trailing before saving

 (add-hook 'before-save-hook 'delete-trailing-whitespace)

;; enable view-mode
(setq view-read-only t)
;; (package-install 'viewer)
(require 'viewer)
(viewer-stay-in-setup)
(setq viewer-modeline-color-unwritable "tomato")
(setq viewer-modeline-color-view "orange")
(viewer-change-modeline-color-setup)
(add-hook 'find-file-hook 'view-mode)

;; make unnecessary trailing space visible
(when (boundp 'show-trailing-whitespace)
  (setq-default show-trailing-whitespace t))

;;; colour current line
(global-hl-line-mode 1)

;;; the colour for the current line
;;(set-face-background 'hl-line "darkolivegreen")
(set-face-background 'hl-line "hot pink")

;;; save history and enable it even after reboot of emacs
(savehist-mode 1)

;;; save the cursor position in each file
(setq-default save-place t)
(require 'saveplace)

;;; show corresponding parentheses
(show-paren-mode 1)

;;; use C-h as backspace(delete one char before cursor)

(global-set-key (kbd "C-h") 'delete-backward-char)

;;; display time in the mode line
(display-time)

;;; show line number and column number
(line-number-mode 1)
(column-number-mode 1)

;;; make the selected region visible
(transient-mark-mode 1)

;;; set a higher GC threshold
(setq gc-cons-threshold (* 10 gc-cons-threshold))

;;; increase the number of lines in the message log
(setq message-log-max 10000)

;;; enable recursive creation of mini-buffers
(setq enable-recursive-mini-buffers 1)

;;; avoid using dialog boxes
(setq use-dialog-box nil)
(defalias 'message-box 'message)


;;; increase the number of stored history
(setq history-length 1000)

;;; show keystrokes faster in the echo area
(setq echo-keystrokes 0.1)

;;; set a larger threshold for warning of 'opening a large file'
(setq large-file-warning-threshold (* 25 1024 1024))

;;; store input in a mini-buffer even if the input is aborted
(defadvice abort-recursive-edit (before minibuffer-save activate)
  (when (eq (selected-window) (active-minibuffer-window))
    (add-to-history minibuffer-history-variable (minibuffer-contents))))

;;; y stands for yes and n stands for no
(defalias 'yes-or-no-p 'y-or-n-p)

;;; remove scroll and tool bars
;; (tool-bar-mode -1)
;; (scroll-bar-mode -1)

;;; enable 'ffap'
(ffap-bindings)

;;; make it easy to distinguish files with the same name
;;; but in different directories
(require 'uniquify)
(setq uniquify-buffer-name-style 'post-forward-angle-brackets)
(setq uniquify-ignore-buffers-re "*[^*]+*")

;;; use multiple spaces instead of tabs
(setq-default indent-tabs-mode nil)


;;;;;;;;;;
;; helm ;;
;;;;;;;;;;

;; git clone https://github.com/emacs-helm/helm.git ~/.emacs.d/elisp/helm
(add-to-list 'load-path "~/.emacs.d/elisp/helm")
(require 'helm-config)
;; (helm-mode 1)
(global-set-key (kbd "C-x b") 'helm-buffers-list)
(global-set-key (kbd "M-x") 'helm-M-x)
(global-set-key (kbd "M-y") 'helm-show-kill-ring)
(global-set-key (kbd "M-s") 'helm-occur)
(define-key isearch-mode-map (kbd "C-o") 'helm-occur-from-isearch)
(define-key isearch-mode-map (kbd "C-M-o") 'isearch-occur)

;;;;;;;;;;;;;;;;;
;; anything.el ;;
;;;;;;;;;;;;;;;;;

;; ;; (auto-install-batch "anything")
;; (when (require 'anything-startup nil t)
;;   (setq
;;    anything-idle-delay 0.3
;;    anything-input-idle-delay 0.2
;;    anything-candidate-number-limit 100
;;    anything-quick-update t
;;    anything-enable-shortcuts 'alphabet
;;    anything-su-or-sudo "sudo"
;;    ))
;; (global-set-key (kbd "M-y") 'anything-show-kill-ring)

;; ;; (install-elisp-from-emacswiki "color-moccur.el")
;; ;; (install-elisp "http://svn.coderepos.org/share/lang/elisp/anything-c-moccur/trunk/anything-c-moccur.el")
;; (when (require 'anything-c-moccur nil t)
;;   (setq
;;    anything-c-moccur-anything-idle-delay 0.3
;;    anything-c-moccur-highlight-info-line-flag t
;;    anything-c-moccur-enable-auto-look-flag t
;;    anything-c-moccur-enable-initial-pattern t
;;    )
;;   )
;; (setq moccur-split-word t)
;; (global-set-key (kbd "M-s") 'anything-c-moccur-occur-by-moccur)
;; (define-key isearch-mode-map (kbd "C-o") 'anything-c-moccur-from-isearch)
;; (define-key isearch-mode-map (kbd "C-M-o") 'isearch-occur)


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

;;;;;;;;;;;;;;;;;;;;
;; book mark mode ;;
;;;;;;;;;;;;;;;;;;;;
(setq bookmark-save-flag 1)
(progn
  (setq bookmark-sort-flag nil)
  (defun bookmark-arrange-latest-top ()
    (let ((latest (bookmark-get-bookmark bookmark)))
      (setq bookmark-alist (cons latest (delq latest bookmark-alist))))
    (bookmark-save))
  (add-hook 'bookmark-after-jump-hook 'bookmark-arrange-latest-top))

;;; enable wdired
(require 'wdired)
(define-key dired-mode-map "r" 'wdired-change-to-wdired-mode)

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

;; sequential command
;; (auto-install-batch "sequential-command")
(require 'sequential-command-config)
(sequential-command-setup-keys)



;; key-chord
;; (install-elisp-from-emacswiki "key-chord.el")
(require 'key-chord)
(setq key-chord-two-keys-delay 0.04)
(key-chord-mode 1)

;;key-chrod settings
;;(key-chord-define-global "xf" 'anything-for-files)
(key-chord-define-global "xf" 'helm-for-files)
(key-chord-define-global "kj" 'view-mode)

;; ess
;; (package-install 'ess)
(require 'ess-site)



;; yatex

(setq load-path
      (cons (expand-file-name
             "~/.emacs.d/elisp/yatex1.77") load-path))

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

;;;;;;;;;;;;;;;;;;;;;
;; gauche, or SICP ;;
;;;;;;;;;;;;;;;;;;;;;

(setq scheme-program-name "gosh")
(require 'cmuscheme)

(defun scheme-other-window ()
    "Run scheme on other window"
    (interactive)
    (switch-to-buffer-other-window
        (get-buffer-create "*scheme*"))
    (run-scheme scheme-program-name))

(define-key global-map
    "\C-cS" 'scheme-other-window)

;;;;;;;;;;;;;;;;;;;;;;;;
;; SICP setting done! ;;
;;;;;;;;;;;;;;;;;;;;;;;;



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

;;;;;;;;;;;;;;;;
;; recentf.el ;;
;;;;;;;;;;;;;;;;

;; (install-elisp-from-emacswiki "recentf-ext.el")

(setq recentf-max-saved-items 500)
(setq recentf-exclude '("/TAGS$" "/var/tmp/"))
(require 'recentf-ext)

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

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; point-undo (move back and forth to the previous or next position that the cursor existed) ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;; (install-elisp-from-emacswiki "point-undo.el")

(require 'point-undo)
(define-key global-map [f7] 'point-undo)
;;(define-key global-map (kbd "S-<f7>") 'point-redo)

;; the following is (shift f7) on mac (at least for now)
(define-key global-map "\M-[28~" 'point-redo)

;;;;;;;;;;;
;; bm.el ;;
;;;;;;;;;;;

;; (install-elisp "http://cvs.savannah.gnu.org/viewvc/*checkout*/bm/bm/bm.el")
(setq-default bm-buffer-persistence nil)
(setq bm-restore-repository-on-load t)
(require 'bm)
(add-hook 'find-file-hooks 'bm-buffer-restore)
(add-hook 'kill-buffer-hook 'bm-buffer-save)
(add-hook 'after-save-hook 'bm-buffer-save)
(add-hook 'after-revert-hook 'bm-buffer-restore)
(global-set-key (kbd "M-SPC") 'bm-toggle)
(global-set-key (kbd "<f5>") 'bm-previous)
;;(global-set-key (kbd "S-<f5>") 'bm-next)
(global-set-key "\M-[25~" 'bm-next)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; goto-chg.el (move to recently edited position) ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; (install-elisp-from-emacswiki "goto-chg.el")
(require 'goto-chg)
(define-key global-map (kbd "<f8>") 'goto-last-change)
;;(define-key global-map (kbd "S-<f8>") 'goto-last-change-reverse)
(global-set-key "\M-[29~" 'goto-last-change-reverse)


;;;;;;;;;;;;;;;;;;;
;; auto-complete ;;
;;;;;;;;;;;;;;;;;;;

;; (auto-install-batch "auto-complete")
(require 'auto-complete-config)
(global-auto-complete-mode 1)
(setq ac-dwim t)
(ac-config-default)

;;;;;;;;;;;;
;; migemo ;;
;;;;;;;;;;;;

;; (package-install 'migemo)
;;; you must install cmigemo independently of this elisp.
(require 'migemo)
(setq migemo-command "cmigemo")
(setq migemo-options '("-q" "--emacs" "-i" "\a"))
(setq migemo-dictionary "/usr/local/Cellar/cmigemo/20110227/share/migemo/utf-8/migemo-dict")
(setq migemo-coding-system 'utf-8-unix)
(setq migemo-user-dictionary nil)
(setq migemo-regex-dictionary nil)

;;;;;;;;;;;;;;;;;;;;
;; open-junk-file ;;
;;;;;;;;;;;;;;;;;;;;

;;(install-elisp-from-emacswiki "open-junk-file.el")
(require 'open-junk-file)
(setq open-junk-file-format "~/junk/%Y/%m-%d-%H%M%S.")


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; woman (yet another man command) ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(setq woman-manpath "~/.emacs.d/.wmncache.el")

;;; put the following at the end of init.el,
;;; since several key maps of skk is masked by the 'dired-x.el'
;;; and I haven't found a way to avoid it.
;; skk

(add-to-list 'load-path "/usr/local/Cellar/emacs/24.3/share/emacs/site-lisp/skk")
(require 'skk-autoloads)
(global-set-key "\C-x\C-j" 'skk-mode)
(global-set-key "\C-xj" 'skk-auto-fill-mode)
(global-set-key "\C-xt" 'skk-tutorial)
;;; dired-x occupies C-x C-j and the following takes the key back
(add-hook 'dired-load-hook
          (lambda ()
            (load "dired-x")
            (global-set-key "\C-x\C-j" 'skk-mode)
            ))


;;;;;;;;;
;; egg ;;
;;;;;;;;;
;; (package-install 'egg)
