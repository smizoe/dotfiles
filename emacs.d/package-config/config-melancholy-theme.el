;;; config-melancholy-theme.el ---                   -*- lexical-binding: t; -*-
;; Copyright (C) 2021

;; Author:  <smizoe@dell-ws>
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
(use-package melancholy-theme
  :ensure t
  :pin melpa
  :init
  (progn
    (load-theme 'melancholy t t)
    )
  )



(provide 'config-melancholy-theme)
;;; config-melancholy-theme.el ends here
