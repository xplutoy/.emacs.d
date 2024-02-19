;;;
fundamental-mode

(today (format-time-string "%Y-%m-%d"))
(now (format-time-string "<%Y-%0m-%0d %a %H:%0M>"))
(yesterday (format-time-string "<%Y-%0m-%0d %a>" (time-subtract nil (* 24 60 60))))
(tomorrow (format-time-string "<%Y-%0m-%0d %a>" (time-add nil (* 24 60 60))))

;;;
prog-mode

(todo (if (derived-mode-p 'emacs-lisp-mode) ";; " comment-start) "TODO: ")
(fail (if (derived-mode-p 'emacs-lisp-mode) ";; " comment-start) "FAIL: ")
(fixme (if (derived-mode-p 'emacs-lisp-mode) ";; " comment-start) "FIXME: ")

;;;
python-base-mode

(for "for " p " in " p ":" n> q)
(from "from " p " import " q)
(if "if " p ":" n> q)
(ife "if " p ":" n> p n> "else:" n> q)
(main "def main():" n> q)
(ifmain "if __name__ == '__main__':" n> q)
(init "def __init__(self" p "):" n> q)
(static "@staticmethod" n> "def " p "(" p "):" n> q)
(classmethod "@classmethod" n> "def " p "(cls, " p "):" n> q)
(lambda "lambda " p ": " q)
(list "[" p " for " p " in " p "]")
(not_impl "raise NotImplementedError")
(try "try:" n> p n> "except " p ":" n> q)
(tryelse "try:" n> p n> "except " p ":" n> p n> "else:" n> q)

;;;
emacs-lisp-mode

(autoload ";;;###autoload")
(var "(defvar " p "\n  \"" p "\")")
(local "(defvar-local " p "\n  \"" p "\")")
(const "(defconst " p "\n  \"" p "\")")
(custom "(defcustom " p "\n  \"" p "\"" n> ":type '" p ")")
(face "(defface " p " '((t :inherit " p "))\n  \"" p "\")")
(group "(defgroup " p " nil\n  \"" p "\"" n> ":group '" p n> ":prefix \"" p "-\")")
(macro "(defmacro " p " (" p ")\n  \"" p "\"" n> r> ")")
(alias "(defalias '" p " '" p ")")
(fun "(defun " p " (" p ")\n  \"" p "\"" n> r> ")")
(iflet "(if-let (" p ")" n> r> ")")
(whenlet "(when-let (" p ")" n> r> ")")
(iflet* "(if-let* (" p ")" n> r> ")")
(whenlet* "(when-let* (" p ")" n> r> ")")
(andlet* "(and-let* (" p ")" n> r> ")")
(cond "(cond" n "(" q "))" >)
(pcase "(pcase " (p "scrutinee") n "(" q "))" >)
(let "(let (" p ")" n> r> ")")
(let* "(let* (" p ")" n> r> ")")
(rec "(letrec (" p ")" n> r> ")")
(dotimes "(dotimes (" p ")" n> r> ")")
(dolist "(dolist (" p ")" n> r> ")")
(loop "(cl-loop for " p " in " p " do" n> r> ")")
(command "(defun " p " (" p ")\n  \"" p "\"" n> "(interactive" p ")" n> r> ")")
(advice "(defun " (p "adv" name) " (&rest app)" n> p n> "(apply app))" n>
        "(advice-add #'" (p "fun") " " (p ":around") " #'" (s name) ")")
(provide "(provide '" (file-name-base (or (buffer-file-name) (buffer-name))) ")" n
         ";;; " (file-name-nondirectory (or (buffer-file-name) (buffer-name))) " ends here" n)
(package-header ";;; " (p (file-name-base (or (buffer-file-name) (buffer-name))) file) ".el --- " (p "summary" summary) " -*- lexical-binding: t -*-" n n

                ";; Author: " (p (user-full-name)) n
                ";; Version: " (p "version") n
                ";; Package-Requires: " (p "dependencies") n
                ";; Homepage: " (p "homepage") n
                ";; Keywords: " (p "keywords") n n

                ";; This file is not part of GNU Emacs" n n

                (p (concat
                    ";; This program is free software: you can redistribute it and/or modify\n"
                    ";; it under the terms of the GNU General Public License as published by\n"
                    ";; the Free Software Foundation, either version 3 of the License, or\n"
                    ";; (at your option) any later version.\n\n"

                    ";; This program is distributed in the hope that it will be useful,\n"
                    ";; but WITHOUT ANY WARRANTY; without even the implied warranty of\n"
                    ";; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the\n"
                    ";; GNU General Public License for more details.\n\n"

                    ";; You should have received a copy of the GNU General Public License\n"
                    ";; along with this program.  If not, see <https://www.gnu.org/licenses/>."))

                n n

                ";;; Commentary:" n n

                ";; " (p "commentary") n n

                ";;; Code:" n n

                (p "(message \"Hello, World\")") n n

                "(provide '" (s file) ")" n
                ";;; " (s file) ".el ends here")

;;;
org-mode

($  "\\(" p "\\)")
($$ "\\[" n> r> n "\\]")
(caption "#+caption: ")
(begin "#+begin_" (s name) n> r> n "#+end_" name)
(src "#+begin_src " q n r n "#+end_src")
(quote "#+begin_quote" n> r> n "#+end_quote")
(example "#+begin_example" n> r> n "#+end_example")
(center "#+begin_center" n> r> n "#+end_center")
(comment "#+begin_comment" n> r> n "#+end_comment")
(verse "#+begin_verse" n> r> n "#+end_verse")
(dot "#+begin_src dot :file " p " :cmdline -Kdot -Tpng" n> "digraph " p " {" n> r> n "}" n "#+end_src")
(gnuplot "#+begin_src gnuplot :var data=" (p "table") " :file " (p "plot.png") n r n "#+end_src" :post (org-edit-src-code))
(elisp "#+begin_src elisp" n r n "#+end_src" :post (org-edit-src-code))
(inlsrc "src_" p "{" q "}")
