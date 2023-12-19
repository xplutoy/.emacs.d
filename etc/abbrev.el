;;-*-coding: utf-8;-*-
(define-abbrev-table 'global-abbrev-table
  '(;; Misc
    ("1td" "" yx/insert-date :count 0)
    ;; regexp
    ("2bl" "^\s-*$" nil :count 1)
    ;; math/unicode symbols
    ("8in"  "∈" nil :count 1)
    ("8nin" "∉" nil :count 1)
    ("8inf" "∞" nil :count 1)
    ("8lov" "♥" nil :count 1)
    ("8sml" "☺" nil :count 1)
    ("8yx"  "yangxue" nil :count 1)
    ("8em"  "yangxue.cs@foxmail.com" nil :count 1)
    ))

(define-abbrev-table 'latex-mode-abbrev-table
  '(
    ("7gh" "" yx/latex-graphics-skl :count 0)
    ("7bn" "" yx/tex-note-tmpl :count 0)
    ))
