;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; personal configuration ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; delete trailing before saving

(add-hook 'before-save-hook 'delete-trailing-whitespace)

;;;; enable view-mode
;;(setq view-read-only t)
;;;; (package-install 'viewer)
;;(require 'viewer)
;;(viewer-stay-in-setup)
;;(setq viewer-modeline-color-unwritable "tomato")
;;(setq viewer-modeline-color-view "orange")
;;(viewer-change-modeline-color-setup)
;;(add-hook 'find-file-hook 'view-mode)
;;;;(setq find-file-hook nil)

;; make unnecessary trailing space visible
(when (boundp 'show-trailing-whitespace)
  (setq-default show-trailing-whitespace t))

;;; colour current line
(global-hl-line-mode 1)

;;; the colour for the current line
;;(set-face-background 'hl-line "darkolivegreen")
(set-face-background 'hl-line "green4")

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

;;; add newline at the end of file if there is none
(setq require-final-newline 'ask)

;;;;;;;;;;;;
;; ispell ;;
;;;;;;;;;;;;
;; install aspell beforehand!


(setq ispell-program-name "aspell")
(setq ispell-list-command "list")

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
;;(key-chord-define-global "kj" 'view-mode)

;; autoinsert
(require 'autoinsert)
(add-hook 'find-file-hooks 'auto-insert)
(setq auto-insert-directory "~/.emacs.d/templates")

(setq auto-insert-alist
      (append '(
                ("\\.html" . "html/default.html")
               ) auto-insert-alist))

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

;;;;;;;;;;;;;;;;
;; recentf.el ;;
;;;;;;;;;;;;;;;;

;; (install-elisp-from-emacswiki "recentf-ext.el")

(setq recentf-max-saved-items 500)
(setq recentf-exclude '("/TAGS$" "/var/tmp/"))
(require 'recentf-ext)

;;;;;;;;;;;;;;;;;
;; abbrev mode ;;
;;;;;;;;;;;;;;;;;

(setq-default abbrev-mode t)
