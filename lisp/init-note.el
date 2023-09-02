;;; init-note.el --- org note  -*- lexical-binding: t; -*-

;; Author: yangxue <yangxue.cs@foxmail.com>
;; Copyright (C) 2023, yangxue, all right reserved.
;; Created: 2023-08-24 23:00:59
;; Modified: <2023-09-02 23:41:29 yx>
;; Licence: GPLv3

;;; Commentary:

;; note

;;; Code:

;; %% org
(use-package org
  :ensure nil
  :defer 3
  :bind (:map org-mode-map
              ("RET" . crux-yx/org-return-dwim)
              ("C-M-," . org-mark-ring-goto)
              ("M-g o" . consult-org-heading)
              ("C-x n h" . crux-yx/org-show-current-heading-tidily))
  :autoload (org-calendar-holiday)
  :custom
  (org-directory yx/org-dir)
  (org-ellipsis "...")
  (org-num-max-level 2)
  (org-log-into-drawer t)
  (org-reverse-note-order t)
  (org-return-follows-link nil)
  (org-crypt-key yx/gpg-encrypt-key)
  (org-hide-emphasis-markers t)
  (org-cycle-separator-lines 0)
  (org-use-sub-superscripts '{})
  (org-image-actual-width '(600))
  (org-special-ctrl-k t)
  (org-special-ctrl-a/e t)
  (org-use-speed-commands t)
  (org-M-RET-may-split-line nil)
  (org-insert-heading-respect-content nil)
  (org-id-link-to-org-use-id 'create-if-interactive-and-no-custom-id)

  (org-startup-folded t)
  (org-hide-block-startup t)
  (org-hide-drawer-startup t)
  (org-startup-with-inline-images t)
  (org-startup-with-latex-preview t)

  (org-pretty-entities t)
  (org-pretty-entities-include-sub-superscripts nil)

  (org-fontify-done-headline t)
  (org-fontify-whole-heading-line t)
  (org-fontify-quote-and-verse-blocks t)

  (org-refile-targets
   '((nil :maxlevel . 3)
     (org-agenda-files :maxlevel . 3)))

  (org-latex-compiler "xelatex")
  (org-latex-packages-alist '(("" "ctex" t)))
  (org-preview-latex-default-process 'dvisvgm)
  (plist-put org-format-latex-options :scale 1.5)
  (org-latex-preview-ltxpng-directory
   (expand-file-name "ltximg/" no-littering-var-directory))

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
                              ("math" . ?m) ("AI" . ?a) ("technology" . ?t)
                              (:endgroup . nil)
                              ("crypt" . ?c) ("project" . ?p)
                              ("bugfix" . ?b) ("urgent" . ?u)
                              )))
  (org-fast-tag-selection-single-key t)

  ;; todo
  (org-todo-keywords
   '((sequence "TODO(t!)" "SOMEDAY(s!)" "NEXT(n!)" "HOLD(h@/!)" "|" "CANCELED(c@/!)" "DONE(d!)")))
  (org-todo-repeat-to-state "NEXT")
  (org-enforce-todo-dependencies t)

  (org-default-notes-file
   (expand-file-name "inbox.org" org-directory))

  (org-capture-templates
   '(("t" "Task"  entry (file+headline org-default-notes-file "Task")
      "* TODO [#B] %?\nSCHEDULED: %(org-insert-time-stamp (current-time) t)" :prepend t)
     ("s" "Someday"  entry (file+headline org-default-notes-file "Someday/Maybe")
      "* SOMEDAY [#C] %?\nDEADLINE: %(org-insert-time-stamp (org-read-date nil t \"12/30\"))" :prepend t)
     ("h" "Habit" entry (file+headline org-default-notes-file "Habit")
      "* NEXT [#B] %?\nSCHEDULED: \<%(format-time-string (string ?% ?F ?  ?% ?a) (current-time)) .+1d/7d\>\n:PROPERTIES:\n:STYLE: habit\n:REPEAT_TO_STATE: NEXT\n:END:\n" :prepend t))
   )


  (org-stuck-projects '("+project/-DONE-CANCELED"
                        ("TODO" "NEXT" "SOMEDAY")
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
  (org-agenda-skip-scheduled-delay-if-deadline t)
  (org-agenda-window-setup 'current-window)
  (org-agenda-use-tag-inheritance nil)
  (org-agenda-use-time-grid t)
  (org-agenda-time-grid (quote ((daily today require-timed)
                                (300 600 900 1200 1500 1800 2100 2400)
                                "......"
                                "---------------------------------")))
  (org-agenda-current-time-string "Now - - - - - - - - - - - - - - - - - - - - -")
  (org-agenda-include-diary t)
  (org-agenda-format-date 'yx/org-agenda-format-date-aligned)
  (org-agenda-scheduled-leaders '("&计划  " "&拖%02d  "))
  (org-agenda-deadline-leaders '("&截止  " "&剩%02d  " "&逾%02d  "))

  (org-clock-persist t)
  (org-clock-in-resume t)
  (org-clock-out-when-done t)
  (org-clock-out-remove-zero-time-clocks t)

  (org-cite-csl-styles-dir
   (expand-file-name "styles/" yx/zotero-dir))
  (org-cite-export-processors
   '((latex biblatex)
     (t . (csl "ieee.csl"))))
  (org-cite-global-bibliography
   (list (expand-file-name "bibliography.bib" yx/org-dir)))

  (org-attach-store-link-p 'attach)
  (org-attach-sync-delete-empty-dir t)

  :config
  (key-chord-define org-mode-map "jh" 'avy-org-goto-heading-timer)
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
  (add-hook 'org-babel-after-execute-hook
            (lambda ()
              (when org-inline-image-overlays
                (org-redisplay-inline-images))))
  (org-crypt-use-before-save-magic)
  (org-clock-persistence-insinuate)
  (unpackaged/def-org-maybe-surround "~" "=" "*" "/" "+")
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
            :deadline today)
     (:name "Important"
            :tag "urgent"
            :priority>= "A")
     (:name "Overdue"
            :deadline past)
     (:todo "HOLD")
     (:auto-planning t))
   )
  :hook (org-agenda-mode . org-super-agenda-mode)
  )

(use-package org-modern
  :hook
  (org-mode . org-modern-mode)
  (org-agenda-finalize . org-modern-agenda)
  )

(use-package valign
  :hook (org-mode . valign-mode)
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
  :defer 3
  :load-path "site-lisp/org-project-yx"
  :custom
  (org-project-prompt-for-project t)
  (org-project-todos-per-project nil)
  (org-project-todos-file org-default-notes-file)
  (org-project-capture-template "* TODO [#B] %?\nDEADLINE: %(org-insert-time-stamp (org-read-date nil t \"+1d\"))\n")
  :bind (:map project-prefix-map
              ("t" . org-project-capture)
              ("o" . org-project-open-todos))
  )

(use-package org-web-tools)

;; %% org-roam notes
(use-package org-roam
  :after org
  :demand t
  :init
  (setq
   org-roam-directory yx/org-dir
   org-roam-db-update-on-save t
   org-roam-database-connector 'sqlite-builtin
   org-roam-completion-everywhere t
   org-roam-dailies-directory "journal/"
   org-roam-capture-templates
   '(("d" "default" plain "%?"
      :target (file+head "%<%Y%m%d>-${slug}.org" "#+title: ${title}\n#+created: %U\n#+modified: <>\n\n%i%?"))
     ("p" "post" plain "%?"
      :target (file+head "blog/%<%Y%m%d>-${slug}.org" "#+title: ${title}\n#created: %U\n#+modified: <>\n\n%i%?")))
   org-roam-dailies-capture-templates
   '(("d" "default" entry "* %<%m-%d %p>: ${title}\n\n %i%?"
      :target (file+head "%<%Y>.org" "#+title: %<%Y>年琐记\n" :prepend t)))
   )
  :config
  (org-roam-db-autosync-mode 1)
  )

(use-package org-roam-ui
  :init
  (when (featurep 'xwidget-internal)
    (setq org-roam-ui-browser-function 'xwidget-webkit-browse-url)
    )
  )

(use-package org-transclusion)

;; %% latex
(use-package tex
  :ensure auctex
  :mode (("\\.tex\\'" . LaTeX-mode))
  :config
  (setq-default
   Tex-master nil
   TeX-engine 'xetex)
  (setq
   TeX-auto-save t
   TeX-parse-self t
   TeX-view-program-selection
   '((output-pdf "pdf-tools"))
   TeX-view-program-list
   '(("pdf-tools" TeX-pdf-tools-sync-view)))
  (setq reftex-plug-into-AUCTeX t)
  (mapc (lambda (mode)
          (add-hook 'LaTeX-mode-hook mode))
        (list
         'flyspell-mode
         'TeX-PDF-mode
         'turn-on-reftex
         'LaTeX-math-mode
         'TeX-source-correlate-mode))
  (add-hook 'TeX-after-comilation-finished-functions
            'TeX-revert-document-buffer)
  (with-eval-after-load 'eglot
    (add-to-list 'eglot-server-programs '(latex-mode "texlab")))
  )

(use-package cdlatex
  :hook
  ((LaTeX-mode . turn-on-cdlatex)
   (org-mode   . turn-on-org-cdlatex))
  )

;; %% citar
(use-package citar
  :after org
  :demand t
  :custom
  (org-cite-insert-processor 'citar)
  (org-cite-follow-processor 'citar)
  (org-cite-activate-processor 'citar)
  (citar-notes-paths `(,yx/org-dir))
  (citar-library-paths `(,yx/zotero-dir))
  (citar-at-point-function 'embark-act)
  (citar-bibliography org-cite-global-bibliography)
  :hook
  (org-mode . citar-capf-setup)
  (LaTeX-mode . citar-capf-setup)
  )

(use-package citar-org-roam
  :after (citar org-roam)
  :demand t
  :custom
  (citar-org-roam-subdir "literature_notes")
  (citar-org-roam-note-title-template "${title}")
  (citar-org-roam-capture-template-key "l")
  :config
  (add-to-list
   'org-roam-capture-templates
   '("l" "literature" plain "%?"
     :target
     (file+head
      "%(expand-file-name (or citar-org-roam-subdir \"\") org-roam-directory)/%<%Y%m%d>-${citar-citekey}.org"
      "#+title: 文献笔记: ${note-title}\n#+created: %U\n#+modified: <>\n\n")
     :unnarrowed t))
  (citar-org-roam-mode 1)
  )

;; %% end
(provide 'init-note)
;;; init-note.el ends here
