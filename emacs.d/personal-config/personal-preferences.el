;;; personal-preferences.el ---                      -*- lexical-binding: t; -*-
;;; Code:
(defalias 'exit 'save-buffers-kill-emacs)

;; discard the custom defs by saving it in a temp file
(setq custom-file (make-temp-file "emacs-custom"))


(setq tramp-ssh-controlmaster-options
            "-o ControlMaster=auto -o ControlPath='tramp.%%C' -o ControlPersist=no")

(prefer-coding-system 'utf-8)
;; delete trailing before saving

(add-hook 'before-save-hook 'delete-trailing-whitespace)

;; tab size is 2
(setq-default tab-width 2)

;; make unnecessary trailing space visible
(when (boundp 'show-trailing-whitespace)
  (setq-default show-trailing-whitespace t))


;; electric-pair-mode
(electric-pair-mode t)


;;; save history and enable it even after reboot of emacs
(savehist-mode 1)

;;; show corresponding parentheses
(show-paren-mode 1)

;;; show line number and column number
(line-number-mode 1)
(column-number-mode 1)

;;; make the selected region visible
(transient-mark-mode 1)


;;; use multiple spaces instead of tabs
(setq-default indent-tabs-mode nil)

;;;;;;;;;;;;;;;;;
;; abbrev mode ;;
;;;;;;;;;;;;;;;;;

(setq-default abbrev-mode t)

;;;;;;;;;;;;;;;;;;
;; hl-line-mode ;;
;;;;;;;;;;;;;;;;;;
;;; colour current line
(global-hl-line-mode 1)

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


(setq ispell-program-name
      (if (string-equal system-type "windows-nt")
          "hunspell.exe"
        "aspell"
       )
      )
(setq ispell-list-command
      (if (string-equal system-type "windows-nt")
          "-l"
      "list")
      )

;;; enable wdired
(require 'wdired)
(define-key dired-mode-map "r" 'wdired-change-to-wdired-mode)

;; autoinsert
(require 'autoinsert)
(add-hook 'find-file-hooks 'auto-insert)
(setq auto-insert-directory "~/.emacs.d/templates")

(setq auto-insert-alist
      (append '(
                ("\\.html" . "html/default.html")
                ("\\.tex" . "tex/default.tex")
               ) auto-insert-alist))

(provide 'personal-preferences)
;;; personal-preferences.el ends here
