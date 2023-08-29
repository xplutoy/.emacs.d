fundamental-mode

(today (format-time-string "%Y-%m-%d"))
(now (format-time-string "<%Y-%0m-%0d %a %H:%0M>"))

prog-mode

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

(_src "src_" q "{" q "}")
(-src "src_" q "[" q "]" "{" q "}")
(+src "#+begin_src " q n> r> n "#+end_src")
(+qt  "#+begin_quote" n> r> n "#+end_quote")
(+sep "#+begin_src emacs-lisp" n> r> n "#+end_src" :post (org-edit-src-code))
