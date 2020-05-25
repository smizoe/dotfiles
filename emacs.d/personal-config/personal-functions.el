;;; personal-functions.el ---                        -*- lexical-binding: t; -*-
;;; Code:

(defun simulate-key-press (key)
    "Return a command that pretends KEY was presssed.
KEY must be given in `kbd' notation."
    `(lambda () (interactive)
       (setq prefix-arg current-prefix-arg)
            (setq unread-command-events (listify-key-sequence (read-kbd-macro ,key)))))

;; function that is required to run emacs when no network is available
(defun package-required-setup ()
  (progn
         (require 'package)
         ;; required by use-package; repository specified by :pin must be present
         (setq package-archives '(("gnu" . "https://elpa.gnu.org/packages/")
                                  ("melpa-stable" . "https://stable.melpa.org/packages/")
                                  ("melpa" . "https://melpa.org/packages/")
                                  ("org" . "https://orgmode.org/elpa/")))
         (package-initialize)
         )
  )


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
    (emacs-lisp-mode
      (pop . pop-tag-mark)
      )
    (lsp-mode
     (def . lsp-find-definition)
     (ref . lsp-find-references)
     (pop . pop-tag-mark)
     (doc . lsp-describe-thing-at-point)
     (impl . lsp-find-implementation)
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
  (defun code-jump-entry-fn/unimplemented (sym)
    (lexical-let ((sym sym))
      #'(lambda ()
          (interactive)
          (message "%s jump is unimplemented for helm-gtags" sym)
          )
      )
    )
  (defvar default-jump-fn-name
    `(
      (def . helm-gtags-find-tag)
      (ref . helm-gtags-find-rtag)
      (pop . helm-gtags-pop-stack)
      (doc . ,(code-jump-entry-fn/unimplemented 'doc))
      (impl . ,(code-jump-entry-fn/unimplemented 'impl))
      (sym . xref-find-apropos)
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

(defun code-jump-impl ()
  "Call CODE-JUMP-ENTRY-FN with symbol 'impl."
  (interactive)
  (code-jump-entry-fn 'impl)
  )
(defun code-search-symbol ()
  "Call CODE-JUMP-ENTRY-FN with symbol 'sym."
  (interactive)
  (code-jump-entry-fn 'sym)
  )

(with-eval-after-load 'evil
  (cl-loop for pair in `(
                         ("gd" . ,#'code-jump-to-def)
                         ("gr" . ,#'code-jump-to-ref)
                         ("gp" . ,#'code-jump-pop)
                         ("gl" . ,#'code-jump-doc)
                         ("gi" . ,#'code-jump-impl)
                         ("gs" . ,#'code-search-symbol)
                         )
           do
           (define-key evil-normal-state-map (car pair) (cdr pair))
           )
  )

(provide 'personal-functions)
;;; personal-functions.el ends here
