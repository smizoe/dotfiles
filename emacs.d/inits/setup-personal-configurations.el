;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; personal configuration ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; functions used in global snippet
(defun get-shebang-executable ()
  (let ((env-cmd "#!/usr/bin/env"))
    (cond
      ((stringp (buffer-file-name))
        (let ((file-ext (file-name-extension (buffer-file-name))))
          (cond
            ((string= file-ext "py") (concatenate 'string env-cmd " python"))
            ((string= file-ext "rb") (concatenate 'string env-cmd " ruby"))
            ((string= file-ext "sh") (concatenate 'string env-cmd " bash"))
            (t env-cmd)
            )
          )
        )
      (t env-cmd)
      )
    )
  )

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

;; autoinsert
(require 'autoinsert)
(add-hook 'find-file-hooks 'auto-insert)
(setq auto-insert-directory "~/.emacs.d/templates")

(setq auto-insert-alist
      (append '(
                ("\\.html" . "html/default.html")
                ("\\.tex" . "tex/default.tex")
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


;;;;;;;;;;;;;;;;;;;;;;;
;; toggle major mode ;;
;;;;;;;;;;;;;;;;;;;;;;;

(defgroup toggle-major-mode nil
  "group for toggle-major-mode functionality"
  :group 'local
  )

(defcustom toggle-major-mode-alist nil
  "Alist of file name patterns to a list of modes; we toggle major modes based on this association in toggle-major-mode"
  :type 'alist
  :group 'toggle-major-mode
  )

(defun buffer-mode (&optional buffer-or-string)
  "Returns the major mode associated with a buffer."
  (progn
    (if (not buffer-or-string)
        (setq buffer-or-string (current-buffer))
      )
    (with-current-buffer buffer-or-string
          major-mode))
  )

(defun toggle-major-mode ()
  (interactive)
  (let* ((name buffer-file-name)
         (current-mode (buffer-mode))
         (mode-list (assoc-default name toggle-major-mode-alist 'string-match-p))
         mode index
         )
    (if mode-list
        (progn
          (setq index (cl-position current-mode mode-list))
          (setq mode (nth (mod (+ index 1) (cl-list-length mode-list)) mode-list))
          (funcall mode)
          )
      (message "No entry matched for file name %s" name)
      )
    )
  )

(custom-set-variables
 '(toggle-major-mode-alist
   '(
     ("\\.js$" . (web-mode js2-jsx-mode))
     )
   )
 )

(with-eval-after-load 'evil
  (progn
    (define-key evil-normal-state-map ",tm" 'toggle-major-mode)
    )
  )

(provide 'setup-personal-configurations)
