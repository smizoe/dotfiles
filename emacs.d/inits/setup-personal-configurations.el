;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; personal configuration ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; delete trailing before saving

(add-hook 'before-save-hook 'delete-trailing-whitespace)

;; tab size is 2
(setq-default tab-width 2)

;; make unnecessary trailing space visible
(when (boundp 'show-trailing-whitespace)
  (setq-default show-trailing-whitespace t))


;;; the colour for the current line
;;(set-face-background 'hl-line "darkolivegreen")
(set-face-background 'hl-line "green4")


;;; save the cursor position in each file
(setq-default save-place t)
(require 'saveplace)

;;; use C-h as backspace(delete one char before cursor)

(global-set-key (kbd "C-h") 'delete-backward-char)

;;; display time in the mode line
(display-time)

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

(provide 'setup-personal-configurations)