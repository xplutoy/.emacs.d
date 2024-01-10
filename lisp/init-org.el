;;; -*- lexical-binding: t -*-

;; Author: yangxue <yangxue.cs@foxmail.com>
;; Copyright (C) 2023, yangxue, all right reserved.
;; Created: 2023-09-15 21:58:58
;; Modified: <2024-01-10 17:12:30 yx>
;; Licence: GPLv3

;;; Commentary:

;;

;;; Code:
(use-package org
  :ensure nil
  :defer 2
  :bind
  (:map org-mode-map
        ("M-<f10>" . yx/transient-org)
        ("RET"     . crux-yx/org-return-dwim)
        ("M-g h"   . consult-org-heading)
        ("C-c y"   . crux-yx/org-link-fast-copy)
        ("C-c t h" . org-toggle-heading)
        ("C-c t l" . org-toggle-link-display)
        ("C-c t v" . crux-yx/org-display-subtree-inline-images)
        ("C-c I"   . org-clock-in)
        ("C-c O"   . org-clock-out)
        ("C-c L"   . org-web-tools-insert-link-for-url)
        ("C-x n h" . crux-yx/org-show-current-heading-tidily))
  :autoload (org-calendar-holiday)
  :hook
  (org-mode . yx/org-mode-setup)
  (org-agenda-finalize . yx/org-agenda-finalize-setup)
  (org-babel-after-execute . yx/org-babel-display-image)
  :custom
  (org-directory yx/org-dir)
  (org-ellipsis nil)
  (org-num-max-level 2)
  (org-reverse-note-order t)
  (org-list-allow-alphabetical t)
  (org-return-follows-link nil)
  (org-mouse-1-follows-link nil)
  (org-crypt-key yx/gpg-encrypt-key)
  (org-hide-leading-stars t)
  (org-hide-emphasis-markers t)
  (org-cycle-separator-lines 0)
  (org-use-sub-superscripts '{})
  (org-use-speed-commands nil)
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
  (org-image-actual-width '(300))
  (org-id-link-to-org-use-id 'create-if-interactive-and-no-custom-id)

  (org-element-use-cache nil)
  (org-element-cache-persistent nil)

  (org-blank-before-new-entry '((heading . nil)
                                (plain-list-item . nil)))

  (org-startup-indented nil)
  (org-adapt-indentation nil)

  (org-startup-folded t)
  (org-hide-block-startup t)
  (org-hide-drawer-startup t)
  (org-startup-align-all-tables nil)
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

  (org-refile-targets
   '((nil :maxlevel . 3)
     (org-agenda-files :maxlevel . 3)))
  (org-refile-use-outline-path 'file)
  (org-refile-allow-creating-parent-nodes 'confirm)

  (org-goto-max-level 3)
  (org-goto-interface 'outline-path-completion)
  (org-outline-path-complete-in-steps nil)

  (org-latex-compiler "xelatex")
  (org-latex-packages-alist '(("" "ctex" t)
                              ("" "bm")
                              ("" "amsfonts")
                              ("" "xcolor" t)
                              ("" "minted" t)))
  (org-latex-minted-options '(("breaklines")
                              ("bgcolor" "bg")))
  (org-latex-src-block-backend 'minted)
  ;; (org-latex-preview-numbered t)
  (org-startup-with-latex-preview nil)
  (org-highlight-latex-and-related '(latex))
  (org-preview-latex-default-process 'dvisvgm)
  (org-latex-preview-ltxpng-directory
   (no-littering-expand-var-file-name "ltxinmg/"))

  (org-footnote-auto-adjust nil)

  (org-src-fontify-natively t)
  (org-src-tab-acts-natively t)
  (org-confirm-babel-evaluate nil)
  (org-src-preserve-indentation nil)
  (org-src-window-setup 'split-window-right)
  (org-src-ask-before-returning-to-edit-buffer nil)

  (org-babel-default-header-args
   '(
     (:evel    . "never-export")
     (:session . "none")
     (:results . "replace")
     (:exports . "both")
     (:cache   . "no")
     (:noweb   . "no")
     (:hlines  . "no")
     (:wrap    . "results")
     (:tangle  . "no")
     ))

  (org-modules '(ol-info ol-eww ol-docview org-habit org-tempo))

  ;; tag
  (org-tags-column 0)
  (org-auto-align-tags t)
  (org-tags-exclude-from-inheritance '(project crypt))
  (org-tag-persistent-alist '((:startgroup . nil)
                              ("#math" . ?m) ("#ai" . ?a) ("#tech" . ?t) ("#life")
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

  (org-default-notes-file
   (expand-file-name "inbox.org" org-directory))

  (org-capture-bookmark nil)
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
  (org-agenda-format-date 'yx/org-agenda-format-date-aligned)
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

  (org-export-with-toc t)
  (org-export-with-drawers nil)
  (oeg-export-with-footnotes t)
  (org-export-with-section-numbers nil)
  (org-export-with-sub-superscripts '{})

  :config
  (plist-put org-format-latex-options :scale 1.8)
  ;; (plist-put org-latex-preview-options :zoom 1.15)
  ;; (plist-put org-latex-preview-options :scale 2.20)
  (with-eval-after-load 'ox-latex
    (setq org-latex-logfiles-extensions
          (append org-latex-logfiles-extensions '("toc" "dvi" "tex" "bbl"))))
  (org-babel-do-load-languages
   'org-babel-load-languages
   '((emacs-lisp . t)
     (python . t)
     (R . t)
     (julia . t)
     (org . t)
     (shell . t)
     (latex . t)
     (dot . t)
     (gnuplot . t)
     (lisp . t)
     (scheme . t)
     (jupyter . t)))
  (org-crypt-use-before-save-magic)
  (org-clock-persistence-insinuate)
  (org-clock-auto-clockout-insinuate)
  (run-at-time t 7200 'org-agenda-to-appt)
  (crux-yx/def-org-maybe-surround "~" "=" "*" "/" "+")
  (add-hook 'org-ctrl-c-ctrl-c-hook 'yx/check-latex-fragment)

  :custom-face
  (org-level-1 ((t (:height 1.3))))
  (org-level-2 ((t (:height 1.2))))
  (org-level-3 ((t (:height 1.1))))
  (org-document-title ((t (:height 1.5))))

  :preface
  (defun yx/org-mode-setup ()
    (auto-fill-mode -1)
    (variable-pitch-mode 1)
    (yas-minor-mode -1) ; confict with C-c &
    (push 'cape-tex completion-at-point-functions)
    (modify-syntax-entry ?< "." org-mode-syntax-table)
    (modify-syntax-entry ?> "." org-mode-syntax-table)
    )

  (defun yx/check-latex-fragment ()
    (let ((datum (org-element-context)))
      (when (memq (org-element-type datum) '(latex-environment latex-fragment))
        (org-latex-preview)
        t))
    )

  (defun yx/org-agenda-finalize-setup ()
    (setq appt-time-msg-list nil)
    (org-agenda-to-appt)
    )

  (defun yx/org-babel-display-image ()
    (when org-inline-image-overlays
      (org-redisplay-inline-images))
    )

  (defun yx/org-clock-in ()
    (interactive)
    (org-clock-in '(4))
    )

  (transient-define-prefix yx/transient-org ()
    "Org commands."
    [["Misc"
      ("a" "org-archive-subtree" org-archive-subtree)
      ("g" "org-goto" org-goto)
      ("i" "org-clock-in" org-clock-in)
      ("o" "org-clock-out" org-clock-out)
      ("n" "org-narrow-to-subtree" org-narrow-to-subtree)]
     ["Toggle"
      ("L" "org-toggle-link-display" org-toggle-link-display)
      ("I" "org-toggle-inline-images" org-toggle-inline-images)
      ("F" "org-preview-latex-fragment" org-preview-latex-fragment)]
     ["Cite"
      ("ci" "org-cite-insert" org-cite-insert)
      ("co" "citar-open" citar-open)]
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

(use-package mixed-pitch
  :hook (org-mode . mixed-pitch-mode))

(use-package org-appear
  :hook (org-mode . org-appear-mode)
  :custom
  (org-appear-delay 1)
  (org-appear-autolinks nil)
  (org-appear-inside-latex t)
  (org-appear-autoentities t)
  (org-appear-autoemphasis t)
  (org-appear-autosubmarkers t)
  )

(use-package org-download
  :after org
  :hook (dired-mode . org-download-enable)
  :commands (org-download-screenshot org-download-clipboard)
  :custom
  (org-download-heading-lvl nil)
  (org-download-screenshot-method "screencapture -i %s")
  (org-download-image-dir
   (expand-file-name (concat org-attach-directory "images/") yx/org-dir))
  :bind
  (:map org-mode-map
        ("C-c C-y" . org-download-screenshot)
        ("C-c M-y" . org-download-clipboard)
        )
  )

(use-package org-web-tools)

(provide 'init-org)
;;; init-org.el ends here
