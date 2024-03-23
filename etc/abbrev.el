;;-*-coding: utf-8;-*-
(define-abbrev-table 'global-abbrev-table
  '(;; Misc
    ("1td" "" yx/insert-date)
    ;; regexp
    ("2bl" "^\s-*$")
    ;; math/unicode symbols
    ("8in"  "∈")
    ("8nin" "∉")
    ("8inf" "∞")
    ("8lov" "♥")
    ("8sml" "☺")
    ("8yx"  "yangxue")
    ("8em"  "yangxue.cs@foxmail.com")
    ;; emoji
    (";up"    "🙃")
    (";uni"   "🦄")
    (";laugh" "🤣")
    (";smile" "😀")))

(define-abbrev-table 'latex-mode-abbrev-table
  '(("7bn" "" yx/tex-note-tmpl)
    ("7gh" "" yx/latex-graphics-skl)))
