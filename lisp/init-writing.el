;;; -*- lexical-binding: t -*-

;; Author: yangxue <yangxue.cs@foxmail.com>
;; Copyright (C) 2023, yangxue, all right reserved.
;; Created: 2023-08-24 23:00:59
;; Modified: <2023-12-17 18:43:05 yx>
;; Licence: GPLv3

;;; Commentary:

;; note

;;; Code:
(use-package denote
  :after org
  :demand t
  :custom
  (denote-directory yx/org-dir)
  (denote-infer-keywords t)
  (denote-known-keywords nil)
  (denote-date-prompt-use-org-read-date t)
  (denote-excluded-directories-regexp "data\\|scaffold")
  (denote-prompts '(subdirectory title keywords))
  (denote-templates nil)
  :config
  (require 'denote-org-dblock)
  (denote-rename-buffer-mode 1)
  :preface
  (defun yx/denote-template ()
    "Create note while prompting for a template.
This is equivalent to calling `denote' when `denote-prompts' is
set to \\='(template title keywords subdirectory)."
    (declare (interactive-only t))
    (interactive)
    (let ((denote-prompts '(template subdirectory title keywords)))
      (call-interactively #'denote)))
  )

(use-package citar-denote
  :after (citar denote)
  :demand t
  :config
  (setq citar-denote-subdir t)
  (citar-denote-mode)
  )

(use-package org-transclusion)

(use-package edit-indirect)
(use-package writegood-mode)

(use-package markdown-mode
  :ensure t
  :mode (("README\\.md\\'" . gfm-mode))
  :init
  (setq
   markdown-command "pandoc"
   markdown-enable-math t
   markdown-header-scaling t)
  )

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
   reftex-plug-into-AUCTeX t
   TeX-source-correlate-start-server t)

  (setq TeX-view-program-list
        '(("pdf-tools" TeX-pdf-tools-sync-view)
          ("Skim" "displayline -b -g %n %o %b"))
        TeX-view-program-selection
        `((output-pdf ,(cond
                        (IS-MAC "Skim")
                        (t "pdf-tools")))
          (output-dvi  ,yx/default-open-program)
          (output-html ,yx/default-open-program)))

  (mapc (lambda (mode)
          (add-hook 'LaTeX-mode-hook mode))
        (list
         'flyspell-mode
         'TeX-PDF-mode
         'TeX-fold-mode
         'turn-on-reftex
         'LaTeX-math-mode
         'prettify-symbols-mode
         'TeX-source-correlate-mode))
  (add-hook 'TeX-after-comilation-finished-functions 'TeX-revert-document-buffer)
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


(provide 'init-writing)
;;; init-writing.el ends here
