;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; personal configuration ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun get-active-minor-modes (&optional buffer-or-string)
  "Return a list of minor mode names active in BUFFER-OR-STRING"
  (interactive)
  (progn
    (if (not buffer-or-string)
        (setq buffer-or-string (current-buffer))
      )
    ;;; see doc string of 'describe-mode for the following conditions
    (let ((mode-is-active-fn
           (lambda (mode)
             (let ((fmode (or (get mode :minor-mode-function) mode)))
               (and (boundp mode) (symbol-value mode) (fboundp fmode))
              )
             )))
      (with-current-buffer buffer-or-string
        (remove-if-not mode-is-active-fn minor-mode-list))
      )
    )
  )

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

;; jump functions for source code reading
(defcustom mode-to-code-jump-function-name-alist
  '(
    (racer-mode
      (def . racer-find-definition)
      (pop . pop-tag-mark)
      (doc . racer-describe)
      )
    (ensime-mode
      (def . ensime-edit-definition)
      (ref . ensime-show-uses-of-symbol-at-point)
      (pop . ensime-pop-find-definition-stack)
      (doc . ensime-inspector-browse-doc)
      )
    (meghanada-mode
     (def . meghanada-jump-declaration)
     (ref . meghanada-reference)
     (pop . meghanada-back-jump)
     (doc . meghanada-typeinfo)
     )
    )
  "
a nested alist which:
  - 1st layer: maps a major mode name to another alist
  - 2nd layer: maps a type of code jump (jump to definition, reference, pop the stack)
               to a function for that functionality
  "
  :type '(alist
          :value-type (alist :value-type function))
  :group 'local
  )

(defun code-jump-entry-fn (code-jump-type)
  "Perform a code jump of type CODE-JUMP-TYPE."
  (defun code-jump-entry-fn/unimplemented ()
    (interactive)
    (message "doc jump is unimplemented for helm-gtags")
    )
  (defvar default-jump-fn-name
    '(
      (def . helm-gtags-find-tag)
      (ref . helm-gtags-find-rtag)
      (pop . helm-gtags-pop-stack)
      (doc . code-jump-entry-fn/unimplemented)
      )
    "default code jump functions."
    )
  (let* ((target-mode (find-if (lambda (mode) (assq mode mode-to-code-jump-function-name-alist)) (get-active-minor-modes)))
         (jump-fn-alist (cdr (assoc target-mode mode-to-code-jump-function-name-alist)))
         (jump-fn-name (cdr (assoc code-jump-type jump-fn-alist)))
        )
    (if (not jump-fn-name)
        (setq jump-fn-name (cdr (assoc code-jump-type default-jump-fn-name)))
        )
    (call-interactively jump-fn-name)
    )
  )

(defun code-jump-to-def ()
  "Call CODE-JUMP-ENTRY-FN with symbol 'def."
  (interactive)
  (code-jump-entry-fn 'def)
 )

(defun code-jump-to-ref ()
  "Call CODE-JUMP-ENTRY-FN with symbol 'ref."
  (interactive)
  (code-jump-entry-fn 'ref)
 )

(defun code-jump-pop ()
  "Call CODE-JUMP-ENTRY-FN with symbol 'pop."
  (interactive)
  (code-jump-entry-fn 'pop)
 )

(defun code-jump-doc ()
  "Call CODE-JUMP-ENTRY-FN with symbol 'doc."
  (interactive)
  (code-jump-entry-fn 'doc)
  )

(with-eval-after-load 'evil-leader
  (evil-leader/set-key
    "gt" 'code-jump-to-def
    "gr" 'code-jump-to-ref
    "gp" 'code-jump-pop
    "gd" 'code-jump-doc
    )
  )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; camel to snake and snake to camel ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun camel-to-snake (camel)
  (let* (
         (case-fold-search nil)
         (case-replace nil)
         ;; convert FOOBar => fooBar
         (head-downcased
          (replace-regexp-in-string "^[A-Z]*[A-Z]"
                                    (lambda (mtch)
                                      (save-match-data
                                        (if (eq (length mtch) 1)
                                            (downcase mtch)
                                          (concat
                                          (downcase (substring mtch 0 (- (length mtch) 1)))
                                          (substring mtch (- (length mtch) 1) (length mtch))
                                          )
                                          )
                                        )
                                      )
                                    camel
                                    t ;; FIXEDCASE
                                    )
          )
         )
    (replace-regexp-in-string "[a-z0-9][A-Z]"
                              (lambda (mtch)
                                (save-match-data
                                  (let ((mtch-list (mapcar 'char-to-string(string-to-list mtch))))
                                    (concat (car mtch-list) "_" (downcase (cadr mtch-list)))
                                    )
                                  )
                                )
                              head-downcased)
    )
  )

(defun snake-to-camel (snake)
  (let ((words (split-string snake "_")))
    (concat (car words) (mapconcat 'capitalize (cdr words) ""))
    )
  )

(defun toggle-camel-or-snake-case (str)
  (if (string-match-p (regexp-quote "_") str)
      (snake-to-camel str)
    (camel-to-snake str)
      )
  )

(defun toggle-case-word-at-point ()
  "toggle case of the word at point"
  (interactive)
  (let* (
        (start-and-end (bounds-of-thing-at-point 'symbol))
        (start (car start-and-end))
        (end (cdr start-and-end))
        (target (buffer-substring start end))
        )
    (delete-region start end)
    (insert (toggle-camel-or-snake-case target))
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
    (evil-leader/set-key
      "tm" 'toggle-major-mode
      "|" 'align-regexp
      "tc" 'toggle-case-word-at-point
      )
    )
  )

(setq
 org-babel-python-command "ipython --simple-prompt -i --colors=NoColor"
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
    target-venv
    )
  )
(defun advice-pipenv-venv (fun &rest r)
  "set enviroment variables so as to use a virtual environment associated with the current buffer,
then call FUN with R. This function is supposed to be passed to advice-add with :around argument."
  (let* (
         (target-venv-dir (find-pipenv-venv-for (current-buffer)))
         (venv-bin-dir (concat (file-name-as-directory target-venv-dir) "bin"))
         (bufname (concat "Python@" target-venv-dir))
         (old-path (getenv "PATH"))
         (old-virtualenv (getenv "VIRTUAL_ENV"))
         (old-pythonhome (getenv "PYTHONHOME"))
         (exec-path (append `(,venv-bin-dir) exec-path))
         )
    ;; in python-mode, python-shell-buffer-name is used to determine the buffer name of the python inferior process.
    ;; Since function find-pipenv-venv-for (usually) returns the same venv if two files/buffers come from the same project,
    ;; files from the same project share the same python process/environment
    ;; in org-babel, we need to set the session name to python-shell-buffer-name
    ;; e.g., we need to have '#+PROPERTY: header-args:python :session (concat "Python@" (find-pipenv-venv-for (current-buffer)))'
    (unwind-protect
        (progn
          (setq-local python-shell-buffer-name bufname)
          (message "using the virtual environment at %s" target-venv-dir)

          (setenv "VIRTUAL_ENV" target-venv-dir)
          (setenv "PATH" (mapconcat 'identity `(,venv-bin-dir ,old-path) ":"))
          (setenv "PYTHONHOME" nil)

          (apply fun r)
          )
      (progn
        (setenv "PATH" old-path)
        (setenv "VIRTUAL_ENV" old-virtualenv)
        (setenv "PYTHONHOME" old-pythonhome)
        )
      )

    )
  )
(cl-loop for tgt-fun in `(,#'run-python) do
         (unless (advice-function-member-p #'advice-pipenv-venv tgt-fun)
           (advice-add tgt-fun :around #'advice-pipenv-venv)
           )
         )


(provide 'setup-personal-configurations)
