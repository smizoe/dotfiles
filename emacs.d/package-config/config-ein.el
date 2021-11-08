;;; config-ein.el --- config for emacs ipython notebook (ein)  -*- lexical-binding: t; -*-

;; Copyright (C) 2021

;; Author:  <smizoe@dell-ws>
;; Keywords:

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.
;;; Code:

(use-package ein
  :ensure t
  :init
  (progn
    (add-hook 'ein:notebook-mode-hook (lambda () (setq-local before-save-hook nil)))
    (with-eval-after-load 'evil
      (evil-define-key 'normal ein:notebook-mode-map
        "\\j" 'ein:worksheet-goto-next-input-km
        "\\k" 'ein:worksheet-goto-prev-input-km
        "\\w" 'ein:notebook-save-notebook-command-km
        "\\b" 'ein:worksheet-insert-cell-below-km
        "\\B" (lambda ()
                (interactive)
                (setq current-prefix-arg t)
                (call-interactively 'ein:worksheet-insert-cell-below-km)
                )
        "\\a" 'ein:worksheet-insert-cell-above
        "\\A" (lambda ()
                (interactive)
                (setq current-prefix-arg t)
                (call-interactively 'ein:worksheet-insert-cell-above-km)
                )
        "\\d" 'ein:worksheet-delete-cell
        "\\c" 'ein:worksheet-clear-output-km
        "\\z" 'ein:notebook-kernel-interrupt-command-km
        (kbd "RET" ) 'ein:worksheet-execute-cell-km
        (kbd "<M-return>") 'ein:worksheet-execute-cell-and-goto-next-km
        )
      )
    )
  )

(provide 'config-ein)
;;; config-ein.el ends here
