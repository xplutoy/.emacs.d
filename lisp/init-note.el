;;; init-note.el --- org note  -*- lexical-binding: t; -*-

;; Author: yangxue <yangxue.cs@foxmail.com>
;; Copyright (C) 2023, yangxue, all right reserved.
;; Created: 2023-08-24 23:00:59
;; Modified: <2023-08-27 00:17:16 yx>
;; Licence: GPLv3

;;; Commentary:

;; note

;;; Code:

;; %% org
(use-package org
  :ensure nil
  :defer 2
  :custom
  (org-directory yx/org-dir)
  (org-ellipsis "...")
  (org-num-max-level 2)
  (org-log-into-drawer t)
  (org-reverse-note-order t)
  (org-return-follows-link nil)
  (org-crypt-key yx/gpg-encrypt-key)
  (org-hide-emphasis-markers t)
  (org-use-sub-superscripts '{})
  (org-image-actual-width '(600))
  (org-special-ctrl-k t)
  (org-special-ctrl-a/e t)
  (org-use-speed-commands t)
  (org-fontify-quote-and-verse-blocks t)
  (org-id-link-to-org-use-id 'create-if-interactive-and-no-custom-id)

  (org-hide-block-startup t)
  (org-hide-drawer-startup t)
  (org-startup-folded 'content)
  (org-startup-with-inline-images t)
  (org-startup-with-latex-preview t)

  (org-pretty-entities t)
  (org-pretty-entities-include-sub-superscripts nil)

  (org-latex-compiler "xelatex")
  (org-latex-packages-alist '(("" "ctex" t)))
  (plist-put org-format-latex-options :scale 2.20)
  (org-preview-latex-default-process 'dvisvgm)
  (org-latex-preview-ltxpng-directory
   (expand-file-name "ltximg/" no-littering-var-directory))

  (org-footnote-auto-adjust t)

  (org-tags-column 0)
  (org-auto-align-tags nil)
  (org-tag-alist '(("crypt" . ?c) ("project" . ?p)))
  (org-tags-exclude-from-inheritance '(project crypt))

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

  (org-modules '(org-habit org-tempo))

  (org-default-notes-file
   (expand-file-name "inbox.org" org-directory))
  (org-capture-templates
   '(("t" "task"  entry (file+headline org-default-notes-file "Tasks")
      "* TODO [#B] %?\n" :prepend t :kill-buffer t)
     ("i" "idea"  entry (file+headline org-default-notes-file "Someday/Maybe")
      "* WAIT [#C] %?\n" :prepend t :kill-buffer t)
     ("h" "habit" entry (file+headline org-default-notes-file "Habits")
      "* NEXT [#B] %?\n:PROPERTIES:\n:STYLE: habit\n:REPEAT_TO_STATE: NEXT\n:END:\n"
      :prepend t :kill-buffer t))
   )

  (org-tag-alist
   '((:startgroup . nil)
     ("@work" . ?w) ("@home" . ?h) ("@society" . ?s) ("bugfix" . ?b)
     (:endgroup)  ;用作任务管理的tag
     (:startgroup . nil)
     ("programing" . ?p) ("math" . ?m) ("academe" . ?a) ("technology" . ?t)
     (:endgroup)  ;用作笔记的内容tag
     ))
  (org-fast-tag-selection-single-key t)

  (org-todo-keywords
   '((sequence "TODO(t!)" "NEXT(n!)" "HOLD(h@/!)" "|" "DONE(d@/!)")))
  (org-enforce-todo-dependencies t)

  (org-agenda-files `(,org-default-notes-file))
  (org-agenda-span 'day)
  (org-agenda-tags-column 0)
  (org-agenda-compact-blocks t)
  (org-agenda-include-diary t)
  (org-agenda-include-deadlines t)
  (org-agenda-skip-deadline-if-done t)
  (org-agenda-skip-scheduled-if-done t)
  (org-agenda-skip-scheduled-delay-if-deadline t)
  (org-agenda-window-setup 'current-window)
  (org-agenda-use-time-grid t)
  (org-agenda-time-grid
   '((daily today require-timed)
     (300 600 900 1200 1500 1800 2100 2400)
     "......" "----------------------------------------"))

  ;; org-cite
  (org-cite-csl-styles-dir
   (expand-file-name "styles/" yx/zotero-dir))
  (org-cite-export-processors
   '((latex biblatex)
     (t . (csl "ieee.csl"))))
  (org-cite-global-bibliography
   (list (expand-file-name "bibliography.bib" yx/org-dir)))

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
     (jupyter . t)))
  (org-crypt-use-before-save-magic)
  )

;; %% org+
(use-package org-ql
  :after org
  )

(use-package org-super-agenda
  :init
  (setq
   org-super-agenda-groups
   '((:name "今日待办"
            :time-grid t
            :deadline today)
     (:name "重要待办"
            :priority "A")
     (:name "逾期事项"
            :deadline past)
     (:name "受阻事项"
            :todo ("HOLD"))
     (:auto-category t))
   )
  :hook (org-mode . org-super-agenda-mode)
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
  (org-download-clipboard org-download-screenshot)
  :custom
  (org-download-method 'attach)
  (org-download-screenshot-method "screencapture -i %s")
  (org-download-image-dir (expand-file-name "attachs" yx/org-dir))
  :bind (:map org-mode-map
              ("C-c y" . org-download-clipboard)
              ("C-c Y" . org-download-screenshot))
  )

;; %% org-roam notes
(use-package org-roam
  :after org
  :demand t
  :init
  (setq
   org-roam-directory yx/org-dir
   org-roam-database-connector 'sqlite-builtin
   org-roam-completion-everywhere t
   org-roam-dailies-directory "daily/"
   org-roam-capture-templates
   '(("d" "default" plain "%?"
      :target (file+head "%<%Y%m%d>-${slug}.org" "#+title: ${title}")
      :unnarrowed t)
     ("p" "post" plain "%?"
      :target (file+head "blog/%<%Y%m%d>-${slug}.org" "#+title: ${title}")
      :unnarrowed t :immediate-finish t))
   org-roam-dailies-capture-templates
   '(("d" "default" entry "* %<%m-%d %p>: %?"
      :target (file+head "%<%Y>.org" "#+title: %<%Y>年琐记\n")
      :unnarrowed t))
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
      "literature_notes/%<%Y%m%d>-${citar-citekey}.org"
      "#+title: ${citar-citekey}: ${note-title}\n#+created: %U\n#+modified: <>\n\n")
     :unnarrowed t))
  (citar-org-roam-mode 1)
  )

;; %% end
(provide 'init-note)
;;; init-note.el ends here
