;;; config-dap.el ---                                -*- lexical-binding: t; -*-

(defun my--get-test-binaries()
  (let (
        (raw-output (shell-command-to-string "cargo test --no-run --message-format=json 2>/dev/null | jq -r 'select(.profile.test == true) | .filenames[]'"))
        )
    (split-string raw-output)
    )
  )

(defun my--lldb-debug-select-program()
  (helm :sources (helm-build-sync-source "test binaries"
                   :candidates #'my--get-test-binaries
                   :fuzzy-match t
                   )
        )
  )

(use-package dap-mode
  :config
  (progn
    (custom-set-variables
     '(dap-lldb-debug-program '("/usr/bin/lldb-vscode"))
     '(dap-lldb-debugged-program-function #'my--lldb-debug-select-program)
     )
    (require 'dap-lldb)
    )
  :ensure t
  :pin melpa
  )


(provide 'config-dap)
;;; config-dap.el ends here
