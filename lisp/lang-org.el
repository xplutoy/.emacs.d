;;; lang-org.el --- org  -*- lexical-binding: t; -*-

;; Author: yangxue <yangxue.cs@foxmail.com>
;; Copyright (C) 2023, yangxue, all right reserved.
;; Created: 2023-09-15 21:58:58
;; Modified: <2023-09-20 09:11:03 yx>
;; Licence: GPLv3

;;; Commentary:

;;

;;; Code:
(use-package org
  :ensure nil
  :defer 3
  :bind (:map org-mode-map
              ("M-<f10>" . yx/transient-org)
              ("RET"     . crux-yx/org-return-dwim)
              ("M-g o"   . consult-org-heading)
              ("C-c I"   . org-clock-in)
              ("C-c O"   . org-clock-out)
              ("C-x n h" . crux-yx/org-show-current-heading-tidily))
  :autoload (org-calendar-holiday)
  :custom
  (org-directory yx/org-dir)
  (org-ellipsis "...")
  (org-num-max-level 2)
  (org-reverse-note-order t)
  (org-return-follows-link t)
  (org-crypt-key yx/gpg-encrypt-key)
  (org-hide-emphasis-markers t)
  (org-cycle-separator-lines 0)
  (org-use-sub-superscripts '{})
  (org-special-ctrl-k t)
  (org-special-ctrl-a/e t)
  (org-support-shift-select t)
  (org-use-speed-commands t)
  (org-M-RET-may-split-line nil)
  (org-link-file-path-type 'relative)
  (org-ascii-headline-spacing '(0 . 1))
  (org-yank-adjusted-subtrees t)
  (org-insert-heading-respect-content t)
  (org-fold-catch-invisible-edits 'show-and-error)
  (org-image-actual-width `(,(* (window-font-width)
                                (- fill-column 8))))
  (org-id-link-to-org-use-id 'create-if-interactive-and-no-custom-id)

  (org-startup-folded t)
  (org-startup-indented nil)
  (org-hide-block-startup t)
  (org-hide-drawer-startup t)
  (org-startup-align-all-tables t)
  (org-startup-with-inline-images nil)

  (org-log-redeadline t)
  (org-log-reschedule t)
  (org-log-into-drawer t)

  (org-pretty-entities nil)
  (org-pretty-entities-include-sub-superscripts nil)

  (org-fontify-done-headline t)
  (org-fontify-whole-heading-line t)
  (org-fontify-quote-and-verse-blocks t)

  (org-refile-targets
   '((nil :maxlevel . 3)
     (org-agenda-files :maxlevel . 3)))

  (org-latex-compiler "xelatex")
  (org-latex-packages-alist '(("" "ctex" t)
                              ("" "bm")
                              ("" "amsfonts")
                              ("" "xcolor" t)
                              ("" "minted" t)))
  (org-latex-minted-options '(("breaklines")
                              ("bgcolor" "bg")))
  (org-latex-src-block-backend 'minted)
  (org-preview-latex-default-process 'dvisvgm)
  (org-startup-with-latex-preview nil)
  (org-latex-preview-ltxpng-directory
   (no-littering-expand-var-file-name "ltxinmg/"))

  (org-footnote-auto-adjust t)

  (org-refile-use-outline-path 'file)
  (org-goto-max-level 3)
  (org-goto-interface 'outline-path-completion)
  (org-outline-path-complete-in-steps nil)

  (org-src-fontify-natively t)
  (org-src-tab-acts-natively t)
  (org-confirm-babel-evaluate nil)
  (org-src-preserve-indentation nil)
  (org-src-window-setup 'split-window-right)
  (org-src-ask-before-returning-to-edit-buffer nil)

  (org-modules '(ol-info ol-eww ol-gnus ol-docview org-habit org-tempo))

  ;; tag
  (org-tags-column 0)
  (org-auto-align-tags t)
  (org-tags-exclude-from-inheritance '(project crypt))
  (org-tag-persistent-alist (quote
                             ((:startgroup . nil)
                              ("@work" . ?w) ("@home" . ?h) ("@society" . ?s)
                              (:endgroup . nil)
                              (:startgroup . nil)
                              ("#math" . ?m) ("#ai" . ?a) ("#technology" . ?t)
                              (:endgroup . nil)
                              ("crypt" . ?c) ("project" . ?p)
                              ("%bugfix%" . ?b) ("%urgent%" . ?u)
                              )))
  (org-fast-tag-selection-single-key t)

  ;; todo
  (org-todo-keywords
   '((sequence "TODO(t!)" "SOMEDAY(s!)" "NEXT(n!)" "HOLD(h@/!)" "WAITING(w@/!)" "|" "CANCELED(c@/!)" "DONE(d!)")))
  (org-todo-repeat-to-state "NEXT")
  (org-enforce-todo-dependencies t)
  (org-treat-S-cursor-todo-selection-as-state-change nil)

  (org-default-notes-file
   (expand-file-name "inbox.org" org-directory))

  (org-capture-templates
   '(("t" "Task"  entry (file+headline org-default-notes-file "Task")
      "* TODO [#B] %?\nDEADLINE: %(org-insert-time-stamp (org-read-date nil t \"+7d\"))\n" :prepend t)
     ("s" "Someday"  entry (file+headline org-default-notes-file "Someday/Maybe")
      "* SOMEDAY [#C] %?\nDEADLINE: %(org-insert-time-stamp (org-read-date nil t \"12/30\"))" :prepend t)
     ("h" "Habit" entry (file+headline org-default-notes-file "Habit")
      "* NEXT [#B] %?\nSCHEDULED: \<%(format-time-string (string ?% ?F ?  ?% ?a) (current-time)) .+1d/7d\>\n:PROPERTIES:\n:STYLE: habit\n:REPEAT_TO_STATE: NEXT\n:END:\n" :prepend t))
   )


  (org-stuck-projects '("+project/-DONE-CANCELED"
                        ("NEXT")
                        nil ""))

  (org-scheduled-past-days 36500)
  (org-deadline-warning-days 365)

  (org-habit-show-all-today nil)

  (org-agenda-span 'day)
  (org-agenda-files `(,org-default-notes-file))
  (org-agenda-compact-blocks t)
  (org-agenda-remove-tags t)
  (org-agenda-include-deadlines t)
  (org-agenda-skip-deadline-if-done t)
  (org-agenda-skip-scheduled-if-done t)
  (org-agenda-skip-scheduled-if-deadline-is-shown 't)
  (org-agenda-skip-scheduled-delay-if-deadline 'post-deadline)
  (org-agenda-skip-deadline-prewarning-if-scheduled 'pre-scheduled)
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
  (org-agenda-format-date 'yx/org-agenda-format-date-aligned)
  (org-agenda-scheduled-leaders '("&计划&  " "&拖%03d  "))
  (org-agenda-deadline-leaders  '("&截止&  " "&剩%03d  " "&逾%03d  "))

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

  (org-global-properties
   '(("STYLE_ALL"  . "habit")
     ("Score_ALL"  . "1 2 3 5 8")
     ("Effort_ALL" . "0:15 0:30 0:45 1:00 2:00 3:00 4:00")))
  (org-columns-default-format "%50ITEM(Task) %2PRIORITY %10Effort(Effort){:} %10CLOCKSUM")

  (org-cite-csl-styles-dir
   (expand-file-name "styles/" yx/zotero-dir))
  (org-cite-export-processors
   '((latex biblatex)
     (t . (csl "ieee.csl"))))
  (org-cite-global-bibliography
   (list (expand-file-name "bibliography.bib" yx/org-dir)))

  (org-attach-id-dir
   (expand-file-name "data/" yx/org-dir))
  (org-attach-store-link-p 'attach)
  (org-attach-sync-delete-empty-dir t)

  (org-export-with-sub-superscripts '{})

  :config
  (plist-put org-format-latex-options :scale 1.50)
  (with-eval-after-load 'ox-latex
    (setq org-latex-logfiles-extensions
          (append org-latex-logfiles-extensions '("toc" "dvi" "tex" "bbl"))))
  (add-hook
   'org-mode-hook
   (lambda ()
     (setq-local
      evil-auto-indent nil)
     (auto-fill-mode 0)
     (variable-pitch-mode 1)))
  (mapc
   (lambda (face)
     (set-face-attribute face nil :inherit 'fixed-pitch-serif))
   (list
    'org-date
    'org-block
    'org-table
    'org-verbatim
    'org-block-begin-line
    'org-block-end-line
    'org-meta-line
    'org-drawer
    'org-property-value
    'org-special-keyword
    'org-latex-and-related
    'org-document-info-keyword))
  (org-babel-do-load-languages
   'org-babel-load-languages
   '((emacs-lisp . t)
     (python . t)
     (R . t)
     (julia . t)
     (org . t)
     (latex . t)
     (dot . t)
     (gnuplot . t)
     (lisp . t)
     (scheme . t)
     (jupyter . t)))
  (org-crypt-use-before-save-magic)
  (org-clock-persistence-insinuate)
  (org-clock-auto-clockout-insinuate)
  (unpackaged/def-org-maybe-surround "~" "=" "*" "/" "+")
  (add-hook 'org-babel-after-execute-hook
            (lambda ()
              (when org-inline-image-overlays
                (org-redisplay-inline-images))))
  (add-hook 'org-agenda-finalize-hook
            (lambda ()
              (setq appt-time-msg-list nil)
              (org-agenda-to-appt)))

  :preface
  (defun yx/org-clock-in ()
    (interactive)
    (org-clock-in '(4)))
  (transient-define-prefix yx/transient-org ()
    "Org commands."
    [["Misc"
      ("a" "Archive Subtree" org-archive-subtree)
      ("g" "org-goto" org-goto)
      ("i" "Clock In" org-clock-in)
      ("o" "Clock Out" org-clock-out)
      ("n" "org-narrow-to-subtree" org-narrow-to-subtree)]
     ["Toggle"
      ("tl" "org-toggle-link-display" org-toggle-link-display)
      ("cv" "org-toggle-inline-images" org-toggle-inline-images)
      ("cl" "org-preview-latex-fragment" org-preview-latex-fragment)]
     ["Cite"
      ("bi" "org-cite-insert" org-cite-insert)
      ("bo" "citar-open" citar-open)]
     ]
    )
  )

;; %% org+
(use-package org-ql
  :after org
  )

(use-package org-super-agenda
  :init
  (setq
   org-super-agenda-groups
   '((:name "Today"
            :time-grid t
            :deadline today
            :scheduled today)
     (:name "Important"
            :tag "urgent"
            :priority>= "A")
     (:name "Overdue"
            :deadline past)
     (:name "Next"
            :todo "NEXT")
     (:name "Someday/Hold"
            :todo "HOLD"
            :todo "WAITING"
            :todo "SOMEDAY")
     (:name "Other"
            :anything))
   )
  :hook (org-agenda-mode . org-super-agenda-mode)
  )

(use-package org-modern
  :after org
  :demand t
  :config
  (global-org-modern-mode)
  )

(use-package valign
  :hook (org-mode . valign-mode)
  :custom
  (valign-fancy-bar 1)
  )

(use-package org-appear
  :hook (org-mode . org-appear-mode)
  :custom
  (org-appear-delay 1)
  (org-appear-autolinks t)
  (org-appear-inside-latex t)
  (org-appear-autoentities t)
  (org-appear-autoemphasis t)
  (org-appear-autosubmarkers t)
  )

(use-package org-download
  :after org
  :commands
  (org-download-screenshot)
  :custom
  (org-download-heading-lvl nil)
  (org-download-screenshot-method "screencapture -i %s")
  (org-download-image-dir
   (expand-file-name (concat org-attach-directory "images/") yx/org-dir))
  :hook (dired-mode . org-download-enable)
  :bind (:map org-mode-map
              ("C-S-y" . org-download-screenshot))
  )

(use-package org-project
  :after org
  :demand t
  :load-path "site-lisp/org-project-yx"
  :custom
  (org-project-prompt-for-project t)
  (org-project-todos-per-project nil)
  (org-project-todos-file org-default-notes-file)
  (org-project-capture-template "* TODO [#B] %?\nDEADLINE: %(org-insert-time-stamp (org-read-date nil t \"+7d\"))\n")
  :bind (:map project-prefix-map
              ("t" . org-project-capture)
              ("o" . org-project-open-todos))
  )

(provide 'lang-org)
;;; lang-org.el ends here
