;;; init-temp.el  -*- lexical-binding: t; -*-

;; Copyright (C) 2023  yangxue

;; Author: yangxue <yangxue.cs@foxmail.com>
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

;;; Commentary:

;;

;;; Code:

;; %% skeleton latex
(define-skeleton yx/latex-graphics
  "Insert centered picture."
  nil
  > "\\begin{center}" \n
  > "\\includegraphics[width=" @ (skeleton-read "Width: ") "]{" @ _ "}" \n
  > "\\begin{center}" > \n @)

;; %% el-file-header skeleton for autoinsert
(define-skeleton yx/auto-insert-el-header  "Insert el-file-header" nil
  ";;; "
  (file-name-nondirectory
   (buffer-file-name))
  " --- "
  (skeleton-read "Descriptions: ")
  "  "
  "-*- lexical-binding: t; -*-"
  '(setq lexical-binding t)
  "\n\n;; Author: "
  (user-full-name)
  " <"
  (progn user-mail-address)
  ">"
  "\n;; Copyright (C) "
  (format-time-string "%Y")
  ", "
  (user-full-name)
  ", all right reserved."
  "\n;; Created: "
  (format-time-string "%F %T")
  "\n;; Last Modified: <>"
  "\n\n;;; This file is NOT part of GNU Emacs"
  "\n\n;;; Licence"
  "
;;
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

;;; Commentary:

;; " _ "

;;; Code:


(provide '"
  (file-name-base
   (buffer-file-name))
  ")
;;; "
  (file-name-nondirectory
   (buffer-file-name))
  " ends here
")

(provide 'init-temp)
;;; init-temp.el ends here
