fundamental-mode

(today (format-time-string "%Y-%m-%d"))
(now (format-time-string "<%Y-%0m-%0d %a %H:%0M>"))

python-base-mode

(for "for " p " in " p ":" n> q)
(main "if __name__=='__main__':" n> q)
(header "#!/usr/bin/env python" n "# -*- coding: utf-8 -*-" n n)

emacs-lisp-mode

(lambda "(lambda (" p ")" n> r> ")")
(autoload ";;;###autoload")
(let "(let (" p ")" n> r> ")")
(cond "(cond" n "(" q "))" >)
(pcase "(pcase " (p "scrutinee") n "(" q "))" >)
(dolist "(dolist (" p ")" n> r> ")")
(loop "(cl-loop for " p " in " p " do" n> r> ")")

latex-mode


org-mode

($  "\\(" p "\\)")
($$ "\\[" n> r> n "\\]")
(dot "#+begin_src dot :file " p " :cmdline -Kdot -Tpng" n> "digraph " p " {" n> r> n "}" n "#+end_src")
