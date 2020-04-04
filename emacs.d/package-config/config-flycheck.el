;;; config-flycheck.el ---                           -*- lexical-binding: t; -*-
;;; Code:

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
                                (cond (
                                       (string-suffix-p ".pmml" (buffer-file-name))
                                       (setq flycheck-checker 'pmml-lint)
                                       )
                                      (t
                                       (setq flycheck-checker 'xml-xmlstarlet)
                                       )
                                      )
                                )
              )
    (with-eval-after-load 'evil
      (evil-leader/set-key
        "c" (simulate-key-press flycheck-keymap-prefix)
        )
      )
    )
  )


(provide 'config-flycheck)
;;; config-flycheck.el ends here
