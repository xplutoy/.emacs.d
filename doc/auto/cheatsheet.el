(TeX-add-style-hook
 "cheatsheet"
 (lambda ()
   (TeX-add-to-alist 'LaTeX-provided-class-options
                     '(("article" "10pt")))
   (TeX-add-to-alist 'LaTeX-provided-package-options
                     '(("geometry" "margin=0.5cm" "landscape" "a3paper")))
   (TeX-run-style-hooks
    "latex2e"
    "article"
    "art10"
    "geometry"
    "textcomp"
    "multicol"
    "menukeys"
    "enumitem"
    "xcolor"
    "xpatch"
    "xparse"
    "calc"
    "tcolorbox"
    "ctex")
   (TeX-add-symbols
    '("keyify" 1)
    '("meta" 1)
    '("humanreadable" 1)
    '("subsection" 1)
    '("section" 1))
   (LaTeX-add-xcolor-definecolors
    "default"
    "heading"
    "command"
    "key")
   (LaTeX-add-enumitem-newlists
    '("keylist" "description")))
 :latex)

