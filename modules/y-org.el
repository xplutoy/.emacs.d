;;; -*- lexical-binding: t -*-

;; Author: yangxue <yangxue.cs@foxmail.com>
;; Copyright (C) 2024, yangxue, all right reserved.
;; Created: 2024-06-07 13:24:59
;; Modified: <2024-06-21 09:48:06 yangx>
;; Licence: GPLv3

;;; Commentary:

;;

;;; Code:

(use-package org
  :ensure nil
  :defer 5
  :bind (:map org-mode-map
              ([remap org-return] . x-org-return-dwim)
              ("s-/"     . transform-previous-char)
              ("M-g h"   . consult-org-heading)
              ("M-l i"   . org-clock-in)
              ("M-l o"   . org-clock-out)
              ("M-l h"   . org-toggle-heading)
              ("M-l M-r" . denote-refs-mode)
              ("M-l y"   . x-org-link-copy)
              ("M-l p"   . org-download-clipboard)
              ("M-l M-p" . org-download-screenshot)
              ("M-l f"   . org-ql-find)
              ("M-l r"   . org-ql-refile)
              ("M-l t"   . org-transclusion-add)
              ("M-l C-t" . org-transclusion-add-all)
              ("M-l M-t" . org-transclusion-remove)
              ("M-l TAB" . x-org-show-current-heading-tidily)
              ("M-l l"   . x-org-insert-fixed-link)
              ("M-l M-l" . org-latex-preview)
              ("M-l M-v" . x-org-toggle-inline-images-in-subtree)
              :repeat-map org-heading-navigate-repeat-map
              ("u" . outline-up-heading)
              ("p" . org-previous-visible-heading)
              ("n" . org-next-visible-heading)
              ("f" . org-forward-heading-same-level)
              ("b" . org-backward-heading-same-level))
  :autoload (org-calendar-holiday)
  :hook
  (org-trigger . save-buffer)
  (org-agenda-finalize . x-org-agenda-to-appt)
  (org-babel-after-execute . x-org-babel-display-image)
  :custom
  (org-directory yx/org-dir)
  (org-ellipsis nil)
  (org-num-max-level 2)
  (org-reverse-note-order t)
  (org-list-allow-alphabetical t)
  (org-return-follows-link t)
  (org-mouse-1-follows-link nil)
  (org-hide-leading-stars t)
  (org-hide-emphasis-markers t)
  (org-cycle-separator-lines 0)
  (org-use-sub-superscripts '{})
  (org-use-speed-commands t)
  (org-special-ctrl-o t)
  (org-special-ctrl-k t)
  (org-special-ctrl-a/e t)
  (org-support-shift-select t)
  (org-ctrl-k-protect-subtree nil)
  (org-M-RET-may-split-line nil)
  (org-link-file-path-type 'relative)
  (org-link-use-indirect-buffer-for-internals t)
  (org-ascii-headline-spacing '(0 . 1))
  (org-yank-adjusted-subtrees t)
  (org-insert-heading-respect-content t)
  (org-fold-catch-invisible-edits 'show-and-error)
  (org-image-actual-width '(600))
  (org-id-link-to-org-use-id 'create-if-interactive-and-no-custom-id)

  (org-lowest-priority ?D)
  (org-priority-default ?C)

  (org-element-use-cache t)
  (org-element-cache-persistent nil)

  (org-blank-before-new-entry '((heading . nil)
                                (plain-list-item . nil)))


  (org-startup-folded t)
  (org-startup-indented nil)
  (org-adapt-indentation nil)
  (org-hide-block-startup t)
  (org-hide-drawer-startup t)
  (org-startup-align-all-tables t)
  (org-startup-with-inline-images nil)

  (org-log-done 'time)
  (org-log-into-drawer t)
  (org-log-redeadline 'time)
  (org-log-reschedule 'time)
  (org-log-note-clock-out nil)
  (org-read-date-prefer-future 'time)

  (org-pretty-entities nil)
  (org-pretty-entities-include-sub-superscripts nil)

  (org-fontify-done-headline t)
  (org-fontify-todo-headline t)
  (org-fontify-whole-heading-line t)
  (org-fontify-quote-and-verse-blocks t)
  (org-fontify-whole-block-delimiter-line t)

  (org-refile-targets '((nil :maxlevel . 3)
                        (org-agenda-files :maxlevel . 3)))
  (org-refile-use-outline-path 'file)
  (org-refile-allow-creating-parent-nodes 'confirm)

  (org-goto-max-level 3)
  (org-goto-interface 'outline-path-completion)
  (org-outline-path-complete-in-steps nil)


  (org-preview-latex-process-alist '((dvisvgm :programs ("xelatex" "dvisvgm")
                                              :description "xdv > svg"
                                              :message "you need to install the programs: xelatex and dvisvgm."
                                              :image-input-type "xdv" :image-output-type "svg" :image-size-adjust (1.7 . 1.5)
                                              :latex-compiler ("xelatex -no-pdf -interaction nonstopmode -output-directory %o %f")
                                              :image-converter ("dvisvgm %o/%b.xdv --no-fonts --exact-bbox --scale=%S --output=%O"))))

  (org-footnote-auto-adjust t)
  (org-footnote-define-inline nil)

  (org-src-tab-acts-natively t)
  (org-src-fontify-natively t)
  (org-confirm-babel-evaluate nil)
  (org-src-window-setup 'split-window-right)
  (org-src-ask-before-returning-to-edit-buffer nil)

  (org-babel-default-header-args '((:evel    . "never-export")
                                   (:session . "none")
                                   (:results . "replace")
                                   (:exports . "both")
                                   (:cache   . "no")
                                   (:noweb   . "no")
                                   (:hlines  . "no")
                                   (:wrap    . "results")
                                   (:tangle  . "no")))

  (org-modules '(ol-info ol-eww))

  ;; tag
  (org-tags-column 0)
  (org-auto-align-tags t)
  (org-tags-exclude-from-inheritance '(project crypt))
  (org-tag-persistent-alist '((:startgroup . nil)
                              ("@me" . ?m) ("@work" . ?t) ("@life" . ?l)
                              (:endgroup . nil)
                              ("crypt" . ?c) ("project" . ?p)))
  (org-fast-tag-selection-single-key t)

  ;; todo
  (org-todo-keywords
   '((sequence "TODO(t!)" "SOMEDAY(s!)" "NEXT(n!)" "HOLD(h@/!)" "WAITING(w@/!)" "|" "CANCELED(c@/!)" "DONE(d!)")))
  (org-todo-repeat-to-state "NEXT")
  (org-enforce-todo-dependencies t)
  (org-enforce-todo-checkbox-dependencies t)
  (org-treat-S-cursor-todo-selection-as-state-change nil)

  (org-default-notes-file (expand-file-name "inbox.org" org-directory))

  (org-capture-bookmark nil)
  (org-capture-templates
   '(("d" "工作完成" entry (file+olp+datetree "work.org" "工作完成") "* %?")
     ("w" "工作待办" entry (file+headline "work.org" "工作待办") "* TODO %?" :prepend t)
     ("t" "个人事务" entry (file+headline org-default-notes-file "个人事务") "* TODO [#B] %?" :prepend t)
     ("s" "未来想做" entry (file+headline org-default-notes-file "未来想做") "* SOMEDAY %?"   :prepend t)
     ("h" "习惯养成" entry (file+headline org-default-notes-file "习惯养成") "* NEXT %?"      :prepend t)))

  (org-stuck-projects '("+project/-DONE-CANCELED"
                        ("NEXT")
                        nil ""))

  (org-scheduled-past-days 365)
  (org-deadline-warning-days 365)

  (org-habit-show-all-today nil)

  (org-agenda-span 'day)
  (org-agenda-files (nol-expand-etc "agenda.txt"))
  (org-agenda-inhibit-startup t)
  (org-agenda-sticky nil)
  (org-agenda-compact-blocks t)
  (org-agenda-remove-tags t)
  (org-agenda-show-all-dates t)
  (org-agenda-mouse-1-follows-link nil)
  (org-agenda-time-leading-zero t)
  (org-agenda-include-deadlines t)
  (org-agenda-skip-deadline-if-done t)
  (org-agenda-skip-scheduled-if-done t)
  (org-agenda-skip-scheduled-delay-if-deadline 't)
  (org-agenda-skip-timestamp-if-deadline-is-shown nil)
  (org-agenda-skip-scheduled-if-deadline-is-shown nil)
  (org-agenda-skip-deadline-prewarning-if-scheduled 't)
  (org-agenda-todo-ignore-deadlines 'near)
  (org-agenda-todo-ignore-scheduled 'future)
  (org-agenda-tags-todo-honor-ignore-options t)
  (org-agenda-window-setup 'only-window)
  (org-agenda-restore-windows-after-quit t)
  (org-agenda-use-tag-inheritance nil)
  (org-agenda-use-time-grid t)
  (org-agenda-time-grid (quote ((daily today require-timed)
                                (300 600 900 1200 1500 1800 2100 2400)
                                "......"
                                "---------------------------------")))
  (org-agenda-current-time-string "Now - - - - - - - - - - - - - - - - - - - - -")
  (org-agenda-diary-file
   (expand-file-name "diary.org" yx/org-dir))
  (org-agenda-include-diary t)
  (org-agenda-show-future-repeats 'next)
  (org-agenda-format-date 'x-org-agenda-format-date-aligned)
  (org-agenda-scheduled-leaders '("计划@  " "拖%03d  "))
  (org-agenda-deadline-leaders  '("截止@  " "剩%03d  " "逾%03d  "))

  (org-clock-idle-time 60)
  (org-clock-into-drawer t)
  (org-clock-persist t)
  (org-clock-in-resume t)
  (org-clock-out-when-done t)
  (org-clock-in-switch-to-state "NEXT")
  (org-clock-persist-query-resume nil)
  (org-clock-out-remove-zero-time-clocks t)
  (org-clock-report-include-clocking-task t)
  (org-use-last-clock-out-time-as-effective-time t)

  (org-global-properties '(("STYLE_ALL"  . "habit")
                           ("Score_ALL"  . "1 2 3 5 8")
                           ("Effort_ALL" . "0:15 0:30 0:45 1:00 2:00 3:00 4:00")))
  (org-columns-default-format "%50ITEM(Task) %2PRIORITY %10Effort(Effort){:} %10CLOCKSUM")

  (org-cite-csl-styles-dir (expand-file-name "styles/" yx/zotero-root))
  (org-cite-global-bibliography (list (expand-file-name "bibliography.bib" yx/org-dir)))
  (org-cite-export-processors '((latex biblatex)
                                (t . (csl "ieee.csl"))))

  (org-attach-id-dir (expand-file-name "data/" yx/org-dir))
  (org-attach-dir-relative t)
  (org-attach-store-link-p 'attach)
  (org-attach-sync-delete-empty-dir t)

  (org-export-with-broken-links t)
  (org-export-with-smart-quotes t)
  (org-export-with-section-numbers nil)
  (org-export-with-sub-superscripts '{})

  :custom-face
  (org-level-1 ((t (:height 1.3))))
  (org-level-2 ((t (:height 1.2))))
  (org-level-3 ((t (:height 1.1))))
  (org-document-title ((t (:height 1.5))))
  (org-drawer ((t (:height 0.85))))
  (org-special-keyword ((t (:height 0.85))))

  :config
  (require 'org-habit)
  (require 'org-tempo)

  (defun yx/org-mode-setup ()
    (auto-fill-mode -1)
    (variable-pitch-mode 1)
    (push 'cape-tex completion-at-point-functions)
    (modify-syntax-entry ?< "." org-mode-syntax-table)
    (modify-syntax-entry ?> "." org-mode-syntax-table))
  (add-hook 'org-mode-hook #'yx/org-mode-setup)

  (cond
   (IS-MAC (plist-put org-format-latex-options :scale 1.3))
   (IS-LINUX (plist-put org-format-latex-options :scale 0.8))
   (t (plist-put org-format-latex-options :scale 1.15)))
  (plist-put org-format-latex-options :background "Transparent")

  (org-babel-do-load-languages 'org-babel-load-languages
                               '((python . t)
                                 ;; (C . t)
                                 ;; (R . t)
                                 ;; (julia . t)
                                 ;; (org . t)
                                 (shell . t)
                                 ;; (latex . t)
                                 ;; (dot . t)
                                 ;; (gnuplot . t)
                                 ;; (lisp . t)
                                 ;; (scheme . t)
                                 ;; (jupyter . t)
                                 (emacs-lisp . t)))

  (setopt org-crypt-key nil)            ; use symmetric encryption unconditionally
  (org-crypt-use-before-save-magic)

  (org-clock-persistence-insinuate)
  (org-clock-auto-clockout-insinuate)
  (run-at-time t 7200 'org-agenda-to-appt)

  (font-lock-add-keywords
   'org-mode
   '(("\\(\\(?:\\\\\\(?:label\\|ref\\|eqref\\)\\)\\){\\(.+?\\)}"
      (1 font-lock-keyword-face)
      (2 font-lock-constant-face))))

  (add-hook 'org-ctrl-c-ctrl-c-hook 'x-org-check-latex-fragment)
  (add-hook 'org-cycle-hook (lambda (state)
                              (when (eq state 'subtree)
                                (x-org-toggle-inline-images-in-subtree t)))))

(use-package ox-latex
  :ensure nil
  :custom
  (org-latex-compiler "xelatex")
  (org-latex-prefer-user-labels t)
  (org-latex-default-class "ctexart")
  (org-latex-packages-alist '(("" "bm")
                              ("" "amsthm")
                              ("" "amsfonts")
                              ("" "xcolor" t)
                              ("cache=false" "minted" t)))
  (org-latex-src-block-backend 'minted)
  (org-latex-minted-options '(("breaklines")
                              ("bgcolor" "bg")))
  (org-latex-pdf-process '("latexmk -f -xelatex -shell-escape -output-directory=%o %f" ))

  (org-startup-with-latex-preview nil)
  (org-highlight-latex-and-related '(native entities))
  (org-preview-latex-image-directory (nol-expand-var "ltximg/"))
  (org-preview-latex-default-process 'dvisvgm)
  (org-preview-latex-process-alist'((dvisvgm :programs
                                             ("xelatex" "dvisvgm")
                                             :description "xdv > svg"
                                             :message "you need to install the programs: xelatex and dvisvgm."
                                             :use-xcolor t
                                             :image-input-type "xdv"
                                             :image-output-type "svg"
                                             :image-size-adjust (1.7 . 1.5)
                                             :latex-compiler
                                             ("xelatex -no-pdf -interaction nonstopmode -shell-escape -output-directory %o %f")
                                             :image-converter
                                             ("dvisvgm %f -e -n -b min -c %S -o %O"))))
  :config
  (add-to-list 'org-latex-classes
               '("ctexart"
                 "\\documentclass[11pt]{ctexart}"
                 ("\\section{%s}" . "\\section*{%s}")
                 ("\\subsection{%s}" . "\\subsection*{%s}")
                 ("\\subsubsection{%s}" . "\\subsubsection*{%s}")
                 ("\\paragraph{%s}" . "\\paragraph*{%s}")
                 ("\\subparagraph{%s}" . "\\subparagraph*{%s}")))
  (appendq! org-latex-logfiles-extensions '("toc" "dvi" "tex" "bbl" "aux" "fls" "fdb_latexmk" "log")))

(use-package org-super-agenda
  :hook (org-agenda-mode . org-super-agenda-mode)
  :init
  (setq org-super-agenda-groups
        '((:name "今天要做"
                 :time-grid t
                 :deadline today
                 :scheduled today)
          (:name "重要紧急"
                 :tag "urgent"
                 :priority>= "A")
          (:name "过期事务"
                 :deadline past)
          (:name "阻塞事务"
                 :todo "HOLD"
                 :todo "WAITING")
          (:name "其他待办"
                 :anything))))

(use-package org-ql
  :after org
  :bind (:map org-mode-map
              ("C-c q f" . org-ql-find)
              ("C-c q r" . org-ql-refile)))

(use-package mixed-pitch
  :hook ((org-mode
          eww-mode
          elfeed-show-mode) . mixed-pitch-mode))

(use-package org-modern
  :after org
  :demand t
  :custom-face
  (org-modern-block-name ((t (:height 0.85))))
  :config
  (setq org-modern-block-fringe 0)
  (global-org-modern-mode 1))

(use-package valign
  :hook (org-mode . valign-mode)
  :custom
  (valign-fancy-bar 1))

(use-package org-download
  :after org
  :demand t
  :hook (dired-mode . org-download-enable)
  :custom
  (org-download-heading-lvl nil)
  (org-download-image-org-width 900)
  (org-download-image-dir (expand-file-name "images/" org-attach-directory))
  (org-download-screenshot-method (cond
                                   (IS-MAC "screencapture -i %s")
                                   (IS-WIN "powershell.exe -command \"(Get-Clipboard -Format Image).Save('%s')\"")
                                   (IS-WSL "powershell.exe -command \"(Get-Clipboard -Format image).Save('$(wslpath -w %s)')\"")
                                   (t "scrot -s %s")))
  :config
  (setq org-download-annotate-function (lambda (_link) "")))

(use-package org-transclusion)

(provide 'y-org)
;;; y-org.el ends here
