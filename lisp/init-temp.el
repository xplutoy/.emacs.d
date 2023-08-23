;;; init-temp.el  -*- lexical-binding: t; -*-

;; Author: yangxue <yangxue.cs@foxmail.com>
;; Copyright (C) 2023, yangxue, all right reserved.
;; Created: 2023-08-22 22:24:06
;; Last Modified: <2023-08-23 15:49:07 yx>

;;; Licence GPLV3

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

;; %% .h file header sketeton for autoinsert
(define-skeleton yx/auto-insert-h-header ""
  (replace-regexp-in-string
   "[^A-Z0-9]" "_"
   (string-replace "+" "P"
                   (upcase
                    (file-name-nondirectory buffer-file-name))))
  "/**\n***************************************************"
  "\n* @author: "
  (user-full-name)
  "\n* @date: "
  (format-time-string "%F %T")
  "\n* @brief: "
  (skeleton-read "brief: ")
  "\n* @modified: <>"
  "\n**************************************************\n*/"
  "\n\n#ifndef " str \n "#define " str
  "\n\n" @ _
  "\n\n#endif"
  )

;; %% c header sketeton for autoinsert
(define-skeleton yx/auto-insert-c-header ""
  nil
  "/**\n***************************************************"
  "\n* @author: "
  (user-full-name)
  "\n* @date: "
  (format-time-string "%F %T")
  "\n* @modified: <>"
  "\n**************************************************\n*/"
  "\n\n" @ _ "\n"
  )

;; %% common (#) header sketeton for autoinsert
(define-skeleton yx/auto-insert-common-header ""
  nil
  "# --------------------------------------------------"
  "\n# Author: "
  (user-full-name)
  "\n# Date: "
  (format-time-string "%F %T")
  "\n# Modified: <>\n#"
  "\n# Description: "
  (skeleton-read "Description: ")
  "\n#\n#\n"
  "# --------------------------------------------------"
  "\n\n\n" @ _
  )

;; %% el-file-header skeleton for autoinsert
(define-skeleton yx/auto-insert-el-header  "" nil
  ";;; "
  (file-name-nondirectory
   (buffer-file-name))
  " --- "
  (skeleton-read "Descriptions: ")
  "  -*- lexical-binding: t; -*-"
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
  "\n;; Modified: <>"
  "\n;; Licence: GPLv3"
  "\n\n;;; Commentary:\n\n;; " @ _
  "\n\n;;; Code:\n\n(provide '"
  (file-name-base
   (buffer-file-name))
  ")\n;;; "
  (file-name-nondirectory
   (buffer-file-name))
  " ends here
")

(provide 'init-temp)
;;; init-temp.el ends here
