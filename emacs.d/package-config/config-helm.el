;;; config-helm.el ---                               -*- lexical-binding: t; -*-
;;; Code:
(use-package helm
  :ensure t
  :pin melpa-stable
  :hook (emacs-startup . (lambda() (require 'helm-config)))
  :config
  (progn
    (define-key helm-map (kbd "<tab>") 'helm-execute-persistent-action)
    (define-key helm-map (kbd "TAB") 'helm-execute-persistent-action)
    (define-key helm-map (kbd "C-z") 'helm-select-action)
    (define-key isearch-mode-map (kbd "C-o") 'helm-occur-from-isearch)
    (define-key isearch-mode-map (kbd "C-M-o") 'isearch-occur)
    )
  )

(use-package helm-ag
  :ensure t
  :after (evil-leader)
  :init
  (custom-set-variables
    '(helm-ag-base-command "rg --vimgrep --color never --no-heading")
    )
  :config
  (progn
    (evil-leader/set-key
      "sr" 'helm-do-ag-project-root
      "sf" 'helm-do-ag-this-file
      "sb" 'helm-do-ag-buffers
      )
    )
  )

(use-package helm-gtags
  :ensure t
  :init
  (custom-set-variables
    '(helm-gtags-suggested-key-mapping t)
    )
  )

(defun gtags-root-dir ()
  "Returns GTAGS root directory or nil if doesn't exist."
  (with-temp-buffer
    (if (zerop (call-process "global" nil t nil "-pr"))
        (buffer-substring (point-min) (1- (point-max)))
      nil)))

(defun gtags-update ()
  "Make GTAGS incremental update"
  (interactive)
  (start-process "global" nil "global" "-u" "--gtagslabel" "pygments"))

(defun gtags-update-single(filename)
  "Update Gtags database for changes in a single file"
  (interactive)
  (start-process "update-gtags" "update-gtags" "bash" "-c" (concat "cd " (gtags-root-dir) " ; gtags --gtagslabel pygments --single-update " filename )))

(defun gtags-update-current-file()
  (interactive)
  (defvar filename)
  (setq filename (replace-regexp-in-string (gtags-root-dir) "." (buffer-file-name (current-buffer))))
  (gtags-update-single filename)
  (message "Gtags updated for %s" filename))

(defun gtags-update-hook()
  "Update GTAGS file incrementally upon saving a file"
  (when (gtags-root-dir)
    (gtags-update-current-file)))

(defun gtags-setup ()
  (interactive)
  (if (gtags-root-dir)
      (message "gtags-root-dir exists: %s" (gtags-root-dir))
    (let ((project-root (helm-ag--project-root)))
      (if (y-or-n-p (format "Would you like to run gtags from %s?" project-root))
          (let ((proc-name (format "setup-gtags: %s" project-root)))
            (start-process proc-name proc-name "bash" "-c" (concat "cd " project-root "; gtags --gtagslabel pygments"))
            )
          )
      )
    )
  )
(add-hook 'after-save-hook #'gtags-update-hook)

(use-package helm-projectile
  :ensure t
  :after (helm projectile evil-leader)
  :config
  (progn
    (helm-projectile-on)
    (evil-leader/set-key
      "ps" 'helm-projectile-switch-project
      "pe" 'helm-projectile-find-file
      "pf" 'helm-projectile-find-file-in-known-projects
      "pxf" 'helm-projectile-recentf
      "pd" 'helm-projectile-find-dir
      )
    )
  :pin melpa-stable
  )

(provide 'config-helm)
;;; config-helm.el ends here
