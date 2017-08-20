;;;;;;;;;;
;; helm ;;
;;;;;;;;;;

;; git clone https://github.com/emacs-helm/helm.git ~/.emacs.d/elisp/helm
;; (package-install 'helm)
;;(add-to-list 'load-path "~/.emacs.d/elisp/helm")
(use-package helm
  :ensure t
  :init (progn
    (require 'helm-config)
    (define-key helm-map (kbd "<tab>") 'helm-execute-persistent-action)
    (define-key helm-map (kbd "TAB") 'helm-execute-persistent-action)
    (define-key helm-map (kbd "C-z") 'helm-select-action)
    (define-key isearch-mode-map (kbd "C-o") 'helm-occur-from-isearch)
    (define-key isearch-mode-map (kbd "C-M-o") 'isearch-occur)
    )
)

;; emmet
(use-package emmet-mode
  :ensure t
  :init
  (progn
    (add-hook 'web-mode-hook 'emmet-mode)
    (add-hook 'nxml-mode-hook 'emmet-mode)
    (add-hook 'sgml-mode-hook 'emmet-mode) ;; Auto-start on any markup modes
    (add-hook 'css-mode-hook  'emmet-mode) ;; enable Emmet's css abbreviation.
    (add-hook 'js2-mode-hook
              (lambda ()(progn
                 (emmet-mode)
                 (setq-local emmet-expand-jsx-className? t)
                 )))
    (with-eval-after-load 'evil
      (evil-define-key 'insert emmet-mode-keymap
        "\C-e" 'emmet-expand-line
        )
      )
    )
 )


;; evil
(use-package evil
  :ensure  evil-leader
  :config  (progn
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

    ;;;; remove RET and SPC from motion state keymap to use them for the other purpose
    (define-key evil-motion-state-map (kbd "RET") nil)
    (define-key evil-motion-state-map (kbd "SPC") nil)

    ;;;; skk
    (define-key evil-insert-state-map "\C-j" 'skk-mode)

    ;; set any custom variables for major modes
    (custom-set-variables
        '(evil-shift-width 2)
        '(evil-search-module 'evil-search)
        '(evil-want-C-u-scroll t)
        )

    ;; add ex commands
    (evil-ex-define-cmd "h[elp]" 'help)
    )
  )

(use-package evil-leader
  :ensure t
  :config
  (progn
    (global-evil-leader-mode)
    (evil-leader/set-leader ",")
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
     )
    )
  )

(use-package evil-surround
  :ensure t
  :init (global-evil-surround-mode 1)
  )

(use-package evil-magit
  :ensure t
  )

(use-package evil-matchit
  :ensure t
  :init (global-evil-matchit-mode 1)
  )



;;;;;;;;;;;;;;;;;
;; artist-mode ;;
;;;;;;;;;;;;;;;;;
(require 'artist)

(add-to-list 'auto-mode-alist '("\\.art$" . artist-mode))
(define-key artist-mode-map [return] nil)
(with-eval-after-load 'evil
  (progn
    (evil-define-key 'motion artist-mode-map
      "h" 'artist-backward-char
      "j" 'artist-next-line
      "k" 'artist-previous-line
      "l" 'artist-forward-char
      "\\\r" (lambda () (interactive) (artist-key-set-point t)) ;; draw
      (kbd "RET") 'artist-key-set-point
      "\\r" 'artist-select-op-rectangle
      "\\c" 'artist-select-op-circle
      "\\e" 'artist-select-op-ellipse
      "\\s" 'artist-select-op-square
      "\\p" 'artist-select-op-poly-line
      "\\l" 'artist-select-op-line
      )
    (evil-define-key 'insert artist-mode-map
      (kbd "DEL") (lambda () (interactive) (picture-backward-clear-column 1))
      )
    (evil-define-key 'normal artist-mode-map
      "x" 'picture-clear-column
      )
    )
  )
(add-hook 'artist-mode-hook
  (lambda ()
    (setq-local evil-move-cursor-back nil)
    )
  )


;;;;;;;;;;;;;;;;;
;; company-mode;;
;;;;;;;;;;;;;;;;;

(use-package company
  :ensure t
  :init (progn
    (add-hook 'after-init-hook
      (lambda()
        (global-company-mode t)
        (define-key company-active-map [return] nil)
        (define-key company-active-map (kbd "RET") nil)
        (define-key company-active-map (kbd "SPC") 'company-complete-selection)
        (define-key company-active-map [escape] 'company-abort)
        (define-key company-search-map [escape] 'company-search-abort)
        ))
    (custom-set-variables
    '(company-selection-wrap-around t)
    '(company-idle-dellay 0)
    '(company-dabbrev-downcase nil)
    '(company-dabbrev-ignore-case nil)
    )
    (with-eval-after-load 'evil
      (progn
        (define-key evil-insert-state-map "\C-p" 'company-select-previous)
        (define-key evil-insert-state-map "\C-n" 'company-select-next)
        )
      )
    )
  :pin melpa-stable
  )

;;;;;;;;;;;;;;;;;;
;; company jedi ;;
;;;;;;;;;;;;;;;;;;

(use-package company-jedi
  :ensure t
  :config
  (progn
    (custom-set-variables
     '(python-environment-virtualenv
       (list
        (shell-command-to-string "{ which virtualenv 2>/dev/null || which virtualenv2 ; } | tr -d '\n'")
        "--system-site-packages" "--quiet")))
    (cl-loop for hook-name in '(python-mode-hook ein:notebook-mode-hook) do
      (add-hook hook-name
                (lambda ()
                  (set (make-local-variable 'company-backends)
                      (cons '(company-jedi :with company-dabbrev) company-backends)
                    )
                  )
        )
      )
    )
  )

;;;;;;;;;;;;;;;;;;;
;; company-shell ;;
;;;;;;;;;;;;;;;;;;;

(use-package company-shell
  :ensure t
  :config
  (add-hook 'shell-mode-hook
            (lambda ()
              (set (make-local-variable 'company-backends) (cons '(company-shell company-capf :with company-dabbrev) company-backends))
              )
    )
  )

;;;;;;;;;;;;;;;;
;; irony mode ;;
;;;;;;;;;;;;;;;;

(use-package irony
  :init
    (progn
      (add-hook 'c++-mode-hook 'irony-mode)
      (add-hook 'c-mode-hook 'irony-mode)
      (add-hook 'objc-mode-hook 'irony-mode)

      (defun my-irony-mode-hook ()
        (define-key irony-mode-map [remap completion-at-point]
          'irony-completion-at-point-async)
        (define-key irony-mode-map [remap complete-symbol]
          'irony-completion-at-point-async))
      (add-hook 'irony-mode-hook 'my-irony-mode-hook)
      (add-hook 'irony-mode-hook 'irony-cdb-autosetup-compile-options)

      (if (string= system-type "darwin")
        (custom-set-variables
        '(irony-additional-clang-options
          '(
            "-I/Library/Developer/CommandLineTools/usr/include/c++/v1"
            "-std=c++11"
            )))
        ()
        )
      )
  )

(use-package company-irony
  :ensure t
  )
(use-package company-irony-c-headers
  :ensure t
  :init
    (with-eval-after-load 'company
      (cl-loop for hook-name in '(c-mode-hook c++-mode-hook objc-mode-hook) do
        (add-hook hook-name
                  (lambda ()
                    (set (make-local-variable 'company-backends) (cons '(company-irony-c-headers company-irony :with company-dabbrev) company-backends))
                    )
                  )
        )
      )
  )

(use-package flycheck-irony
  :ensure t
  :init
    (with-eval-after-load 'flycheck
      (add-hook 'flycheck-mode-hook #'flycheck-irony-setup))
  )

;;;;;;;;;;;;;;;;;;;
;; flyspell mode ;;
;;;;;;;;;;;;;;;;;;;
(cl-loop for hook-name in '(org-mode-hook adoc-mode-hook markdown-mode-hook yatex-mode-hook) do
         (add-hook hook-name (lambda () (flyspell-mode 1)))
         )

;;;;;;;;;;;;;;;;;;;;
;; yasnippet mode ;;
;;;;;;;;;;;;;;;;;;;;

;; (package-install 'yasnippet)
(use-package yasnippet
  :ensure t
  :init
    (add-hook 'yas-minor-mode-hook
      (lambda ()
        (yas-activate-extra-mode 'fundamental-mode)))
  :config
    (progn
      (yas-global-mode 1)
      (cl-loop for hook-name in '(ruby-mode-hook sh-mode-hook python-mode-hook) do
        (add-hook hook-name (lambda ()
                              (progn
                                (make-local-variable 'yas-key-sytaxes)
                                (add-to-list 'yas-key-syntaxes "<_")
                                (add-to-list 'yas-key-syntaxes "<.")
                                )
                              )
          )
        )
      (define-key yas-minor-mode-map (kbd "<tab>") nil)
      (define-key yas-minor-mode-map (kbd "TAB") nil)
      ;; yas-expand doesn't expand '#!' snippet somehow. use yas/expand
      (define-key yas-minor-mode-map (kbd "\C-k") 'yas/expand)

      (define-key yas-keymap [(tab)]       nil)
      (define-key yas-keymap (kbd "TAB")   nil)
      (define-key yas-keymap [(shift tab)] nil)
      (define-key yas-keymap [backtab]     nil)
      (define-key yas-keymap (kbd "\C-k") 'yas-next-field-or-maybe-expand)
      (define-key yas-keymap (kbd "\C-m") 'yas-prev-field)
      (with-eval-after-load 'evil
        (progn
          (evil-leader/set-key
            "os" 'yas-oneshot-snippet ;; register
            "oe" 'yas-oneshot-snippet ;; expand
            )
          (define-key evil-insert-state-map "\C-k" nil) ;; remove keymap; used in yas-minor-mode-map
          (add-hook 'yas-before-expand-snippet-hook (lambda () (evil-insert-state)))
          )
        )
      )
  )


;;;;;;;;;;;;;;;;;;;;;;;
;; other minor modes ;;
;;;;;;;;;;;;;;;;;;;;;;;

;; electric-pair-mode
(electric-pair-mode t)

;;; colour current line
(global-hl-line-mode 1)

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


;;;;;;;;;;;;;;
;; flycheck ;;
;;;;;;;;;;;;;;

(use-package flycheck
  :ensure t
  :config
  (progn
    (global-flycheck-mode)
    (flycheck-define-checker textlint
      "A linter for prose"
      :command ("textlint" "--format" "unix" "--rule" "alex" "--rule" "write-good" source-inplace)
      :error-patterns
      ((warning line-start (file-name) ":" line ":" column ": "
                (id (one-or-more (not (any " "))))
                (message (one-or-more not-newline)
                         (zero-or-more "\n" (any " ") (one-or-more not-newline))
                         )
                line-end))
      :modes (text-mode adoc-mode markdown-mode)
     )
    (flycheck-define-checker pmml-lint
      "A linter for pmml v4.3"
      :command ("xmllint" "--schema" "http://dmg.org/pmml/v4-3/pmml-4-3.xsd" "--noout" "-")
      :standard-input t
      :error-patterns
      ((error line-start "-:" line ": " (message) line-end))
      :modes (xml-mode nxml-mode)
      )
    (add-to-list 'flycheck-checkers 'textlint)
    (add-to-list 'flycheck-checkers 'pmml-lint)
    (add-hook 'nxml-mode-hook (lambda ()
                                (if (string-suffix-p ".pmml" (buffer-file-name))
                                    (setq flycheck-checker 'pmml-lint)
                                    )
                                )
              )
    (with-eval-after-load 'evil
      (evil-leader/set-key
       "c" (simulate-key-press flycheck-keymap-prefix)
       )
      )
    )
  :pin melpa-stable
  )

;;;;;;;;;;;;
;; ensime ;;
;;;;;;;;;;;;

(use-package ensime
  :ensure t
  :pin melpa-stable
  :config
  (progn
    (cl-loop for hook-name in '(scala-mode-hook) do
             (add-hook hook-name
                       (lambda ()
                         (progn
                           (if (ensime-config-find-file buffer-file-name)
                               (ensime)
                             (message "ensime did not start since we can't find .ensime file for this file.")
                             )
                           )
                         )
                       )
      )
    )
  )

;;;;;;;;;;;;;;;
;; meghanada ;;
;;;;;;;;;;;;;;;

(use-package meghanada
  :ensure t
  :defer t
  :config
  (add-hook 'java-mode-hook
            (lambda ()
              ;; meghanada-mode on
              (meghanada-mode t)
              ;; use code format
              (add-hook 'before-save-hook 'meghanada-code-beautify-before-save)
              )
            )
 )

;;;;;;;;;;;;;;;;
;; projectile ;;
;;;;;;;;;;;;;;;;

(use-package projectile
  :ensure t
  :config (projectile-mode)
  :pin melpa-stable
  )

;;;;;;;;;;;;;;;;;;
;; racer (rust) ;;
;;;;;;;;;;;;;;;;;;

(use-package racer
  :ensure t
  :init
  (progn
    (setq racer-rust-src-path "~/.emacs.d/rustc/src/")
    (add-hook 'racer-mode-hook #'company-mode)
    (add-hook 'rust-mode-hook #'racer-mode)
    (add-hook 'racer-mode-hook #'eldoc-mode)
    (add-hook 'racer-mode-hook
              (lambda ()
                (set (make-local-variable 'company-backends) (cons '(company-capf company-dabbrev) company-backends))
                )
              )
   )
  :pin melpa-stable
  )

(provide 'setup-minor)
