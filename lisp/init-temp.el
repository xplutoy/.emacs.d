;;; init-temp.el  -*- lexical-binding: t; -*-

;; Author: yangxue <yangxue.cs@foxmail.com>
;; Copyright (C) 2023, yangxue, all right reserved.
;; Created: 2023-08-22 22:24:06
;; Last Modified: <2023-08-23 01:14:08 yx>

;;; Licence

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

;; %% skeleton latex-graphics
(define-skeleton yx/latex-graphics-skl
  "Insert centered picture."
  nil
  > "\\begin{center}" \n
  > "\\includegraphics[width=" @ (skeleton-read "Width: ") "]{" @ _ "}" \n
  > "\\begin{center}" > \n @)

;; %% skeleton latex-book-note
(define-skeleton yx/latex-book-note-skl "" nil
  "\\documentclass[12pt, a4paper, oneside]{ctexbook}
\\usepackage{amsmath}
\\usepackage{amssymb}
\\usepackage{bm}
\\usepackage{graphicx}
\\usepackage{hyperref}
\\usepackage{mathrsfs}

\\usepackage{amsthm}

\\usepackage{geometry}

\\linespread{1.5}
\\geometry{left=2.54cm, right=2.54cm, top=3.18cm, bottom=3.18cm}

\\newtheorem{theorem}{定理}[section]
\\newtheorem{definition}[theorem]{定义}
\\newtheorem{lemma}[theorem]{引理}
\\newtheorem{corollary}[theorem]{推论}
\\newtheorem{example}[theorem]{例}
\\newtheorem{proposition}[theorem]{命题}

\\title{\\Huge{\\textbf{"
  (skeleton-read "Title: ")
  "}}}
\\author{杨雪}
\\date{\\today}

\\begin{document}

\\maketitle

% 前言
\\pagenumbering{roman}
\\setcounter{page}{1}

\\begin{center}
\\Huge\\textbf{前言}
\\end{center}~\\

这里是前言部分
~\\\\
\\begin{flushright}
\\begin{tabular}{c}
xplutoyz\\
\\today
\\end{tabular}
\\end{flushright}

% 目录
\\newpage
\\pagenumbering{Roman}
\\setcounter{page}{1}
\\tableofcontents

% 正文
\\newpage
\\setcounter{page}{1}
\\pagenumbering{arabic}

\\chapter{第一章}

\\section{第一小结}

\\chapter{第二章}

\\end{document}"
  )

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
