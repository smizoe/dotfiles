;;;;;;;;;;
;; helm ;;
;;;;;;;;;;

;; git clone https://github.com/emacs-helm/helm.git ~/.emacs.d/elisp/helm
;; (package-install 'helm)
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
;; artist-mode ;;
;;;;;;;;;;;;;;;;;
(require 'artist)

(add-to-list 'auto-mode-alist '("\\.art$" . artist-mode))
(define-key artist-mode-map [return] nil)
(eval-after-load 'evil
  '(progn
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

;;;;;;;;;;;;;;;;;;
;; company jedi ;;
;;;;;;;;;;;;;;;;;;

(defun my-python-mode-hook ()
  (add-to-list 'company-backends '(company-jedi :with company-dabbrev)))

(add-hook 'python-mode-hook 'my-python-mode-hook)

;;;;;;;;;;;;;;;;
;; irony mode ;;
;;;;;;;;;;;;;;;;

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
      "--std=c++11"
      )))
  ()
  )
(eval-after-load 'company
    '(add-to-list 'company-backends '(company-irony-c-headers company-irony :with company-dabbrev)))
(eval-after-load 'flycheck
  '(add-hook 'flycheck-mode-hook #'flycheck-irony-setup))

;;;;;;;;;;;;;;;;;;;;
;; yasnippet mode ;;
;;;;;;;;;;;;;;;;;;;;

;; (package-install 'yasnippet)
;; (install-elisp-from-emacswiki "yasnippet-config.el")
(require 'yasnippet)
(yas-global-mode 1)
(require 'yasnippet-config)

(define-key yas-minor-mode-map (kbd "<tab>") nil)
(define-key yas-minor-mode-map (kbd "TAB") nil)
(define-key yas-minor-mode-map (kbd "\C-k") 'yas-expand)

(define-key yas-keymap [(tab)]       nil)
(define-key yas-keymap (kbd "TAB")   nil)
(define-key yas-keymap [(shift tab)] nil)
(define-key yas-keymap [backtab]     nil)
(define-key yas-keymap (kbd "\C-k") 'yas-next-field-or-maybe-expand)
(define-key yas-keymap (kbd "\C-m") 'yas-prev-field)

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

;; key-chord
;; (install-elisp-from-emacswiki "key-chord.el")
(require 'key-chord)
(setq key-chord-two-keys-delay 0.04)
(key-chord-mode 1)
;;key-chrod settings
(key-chord-define-global "xf" 'helm-for-files)

;;;;;;;;;;;;;;;;;
;; abbrev mode ;;
;;;;;;;;;;;;;;;;;

(setq-default abbrev-mode t)


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

(global-flycheck-mode)

(provide 'setup-minor)
