;;; config-evil.el ---                                      -*- lexical-binding: t; -*-

(use-package evil
  :pin melpa-stable
  :ensure t
  :init
  (progn
    ;; set any custom variables for major modes
    (custom-set-variables
        '(evil-shift-width 2)
        '(evil-search-module 'evil-search)
        '(evil-want-C-u-scroll t)
        '(evil-ex-visual-char-range t)
        '(evil-want-abbrev-expand-on-insert-exit nil)
        )
    )
  :config
  (progn
    (evil-mode 1)
    ;; esc quits
    ;; http://stackoverflow.com/questions/8483182/evil-mode-best-practice
    (defun minibuffer-keyboard-quit ()
        "Abort recursive edit.
    In Delete Selection mode, if the mark is active, just deactivate it;
    then it takes a second \\[keyboard-quit] to abort the minibuffer."
        (interactive)
        (if (and delete-selection-mode transient-mark-mode mark-active)
            (setq deactivate-mark  t)
          (when (get-buffer "*Completions*") (delete-windows-on "*Completions*"))
          (abort-recursive-edit)))
    (define-key evil-normal-state-map [escape] 'keyboard-quit)
    (define-key evil-visual-state-map [escape] 'keyboard-quit)
    (define-key minibuffer-local-map [escape] 'minibuffer-keyboard-quit)
    (define-key minibuffer-local-ns-map [escape] 'minibuffer-keyboard-quit)
    (define-key minibuffer-local-completion-map [escape] 'minibuffer-keyboard-quit)
    (define-key minibuffer-local-must-match-map [escape] 'minibuffer-keyboard-quit)
    (define-key minibuffer-local-isearch-map [escape] 'minibuffer-keyboard-quit)

    (define-key evil-normal-state-map "[b" 'next-buffer)
    (define-key evil-normal-state-map "]b" 'previous-buffer)

    ;;;; remove RET, SPC, \ from motion state keymap to use them for the other purpose
    (define-key evil-motion-state-map (kbd "RET") nil)
    (define-key evil-motion-state-map (kbd "SPC") nil)
    (define-key evil-motion-state-map "\\" nil)

    ;; add ex commands
    (evil-ex-define-cmd "h[elp]" 'help)
    )
  )

(use-package evil-leader
  :ensure t
  :init
  (with-eval-after-load 'evil
    (progn
      (global-evil-leader-mode)
      (evil-leader/set-leader "<SPC>")
      (evil-leader/set-key
        "mx" 'helm-M-x
        "r." (concat ":normal." (kbd "RET"))
        "r@" (concat ":normal@q" (kbd "RET"))
        "rf" 'helm-for-files
        "ef" 'helm-find-files
        "N" 'linum-mode
        "P" 'electric-indent-mode
        "nh" 'evil-ex-nohighlight
        "b" 'helm-buffers-list
        "y" (concat ":w !pbcopy" (kbd "RET"))
        "ff" 'ffap
        "|" 'align-regexp
        )
      )
    )
  )

(use-package evil-surround
  :ensure t
  :init
  (with-eval-after-load 'evil
    (global-evil-surround-mode 1)
    )
  )

(use-package evil-magit
  :ensure t
  )

(use-package evil-matchit
  :ensure t
  :config
  (with-eval-after-load 'evil
    (global-evil-matchit-mode 1)
    )
  )

(use-package evil-numbers
  :ensure t
  :init
  (with-eval-after-load 'evil
    (progn
      (define-key evil-normal-state-map (kbd "C-a") 'evil-numbers/inc-at-pt)
      (define-key evil-normal-state-map (kbd "C-x") 'evil-numbers/dec-at-pt)
      )
    )
  )

(provide 'config-evil)
