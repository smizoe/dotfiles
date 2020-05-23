;;; config-org.el ---                                -*- lexical-binding: t; -*-

;;; Code:

(use-package org
  :ensure org-plus-contrib
  :ensure htmlize
  :config
  (progn
    (setq org-use-fast-todo-selection t)
    (setq org-todo-keywords
          '(
            (sequence "TODO(t)" "STARTED(s)" "WAITING(w)" "|" "DONE(x)" "CANCEL(c)")
            (sequence "APPT(a)" "|" "DONE(x)" "CANCEL(c)")
            ))
    (cl-loop for type in '(md confluence) do
             (add-to-list 'org-export-backends type)
             )
    (require 'ox-md)
    (org-babel-do-load-languages
     'org-babel-load-languages
     '(
       (C . t)
       (R . t)
       (python . t)
       (ruby . t)
       (shell . t)
       (sql . t)
       )
     )
    (with-eval-after-load 'evil
      (evil-define-key 'normal org-mode-map
        (kbd "TAB") 'org-cycle
        "\\cb" #'org-babel-execute-buffer
        "\\cB" (lambda ()
                 (interactive)
                 (let ((org-confirm-babel-evaluate nil))
                   (org-babel-execute-buffer)
                   )
                 )
        "\\ce" #'org-export-dispatch
        "\\cE" (lambda ()
                 (interactive)
                 (let ((org-confirm-babel-evaluate nil))
                   (org-export-dispatch)
                   )
                 )
        "\\cc" #'org-ctrl-c-ctrl-c
        "\\c'" #'org-edit-special
        "\\j"  #'org-next-block
        "\\k"  #'org-previous-block
        )
      )
    )
  )




(provide 'config-org)
;;; config-org.el ends here
