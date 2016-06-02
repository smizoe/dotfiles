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
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; use emacs in server mode ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(server-start)
(defun iconify-emacs-when-server-is-done ()
  (unless server-clients (iconify-frame)))
(global-set-key (kbd "C-x C-c") 'server-edit)
(defalias 'exit 'save-buffers-kill-emacs)

;;;;;;;;;;;;;;;;;;;;
;; open-junk-file ;;
;;;;;;;;;;;;;;;;;;;;

;;(install-elisp-from-emacswiki "open-junk-file.el")
(require 'open-junk-file)
(setq open-junk-file-format "~/junk/%Y-%m/%Y-%m-%d_%H-%M-%S_")
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; point-undo (move back and forth to the previous or next position that the cursor existed) ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;; (install-elisp-from-emacswiki "point-undo.el")

(require 'point-undo)
(define-key global-map [f7] 'point-undo)
;;(define-key global-map (kbd "S-<f7>") 'point-redo)

;; the following is (shift f7) on mac (at least for now)
(define-key global-map "\M-[28~" 'point-redo)
;; skk
(require 'skk)
(global-set-key "\C-x\C-j" 'skk-mode)
(global-set-key "\C-xj" 'skk-auto-fill-mode)
(global-set-key "\C-xt" 'skk-tutorial)

(setq skk-server-portnum 1178)
(setq skk-server-host "localhost")
;;; dired-x occupies C-x C-j and the following takes the key back
(add-hook 'dired-load-hook
          (lambda ()
            (load "dired-x")
            (global-set-key "\C-x\C-j" 'skk-mode)
            ))
;; enable edit server for chrome extension
(require 'edit-server)
(edit-server-start)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; goto-chg.el (move to recently edited position) ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; (install-elisp-from-emacswiki "goto-chg.el")
(require 'goto-chg)
(define-key global-map (kbd "<f8>") 'goto-last-change)
;;(define-key global-map (kbd "S-<f8>") 'goto-last-change-reverse)
(global-set-key "\M-[29~" 'goto-last-change-reverse)
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
;; python and venv
;; (package-install 'pyvenv)
(setq
 python-shell-interpreter "ipython"
 python-shell-interpreter-args ""
 python-shell-prompt-regexp "In \\[[0-9]+\\]: "
 python-shell-prompt-output-regexp "Out\\[[0-9]+\\]: "
 python-shell-completion-setup-code
   "from IPython.core.completerlib import module_completion"
 python-shell-completion-module-string-code
   "';'.join(module_completion('''%s'''))\n"
 python-shell-completion-string-code
   "';'.join(get_ipython().Completer.all_completions('''%s'''))\n")
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; woman (yet another man command) ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(setq woman-manpath "~/.emacs.d/.wmncache.el")

(provide 'setup-installed-elisp)
