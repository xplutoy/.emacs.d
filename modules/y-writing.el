;;; -*- lexical-binding: t -*-

;; Author: yangxue <yangxue.cs@foxmail.com>
;; Copyright (C) 2024, yangxue, all right reserved.
;; Created: 2024-06-07 12:02:01
;; Licence: GPLv3

;;; Commentary:

;;

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
  (denote-prompts '(subdirectory title keywords signature))
  (denote-templates nil)
  :config
  (require 'denote-org-extras)
  (denote-rename-buffer-mode 1)
  :preface
  (defun yx/denote-template ()
    "Create note while prompting for a template.
This is equivalent to calling `denote' when `denote-prompts' is set to \\='(template title keywords subdirectory)."
    (declare (interactive-only t))
    (interactive)
    (let ((denote-prompts '(template subdirectory title keywords)))
      (call-interactively #'denote))))

(use-package consult-denote
  :after denote
  :demand t
  :config (consult-denote-mode +1))

(use-package denote-refs)

(use-package denote-menu
  :bind ( :map denote-menu-mode-map
          ("c"   . denote-menu-clear-filters)
          ("e"   . denote-menu-export-to-dired)
          ("/ r" . denote-menu-filter)
          ("/ k" . denote-menu-filter-by-keyword)
          ("/ o" . denote-menu-filter-out-keyword)))

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
  :hook ((org-mode . citar-capf-setup)
         (LaTeX-mode . citar-capf-setup)))

(use-package citar-embark
  :after citar embark
  :demand t
  :no-require t
  :config (citar-embark-mode +1))

(use-package citar-denote
  :after (citar denote)
  :demand t
  :config
  (setq citar-denote-subdir t)
  (citar-denote-mode))

(use-package bibtex
  :ensure nil
  :custom
  (bibtex-dialect 'biblatex)
  (bibtex-align-at-equal-sign t))

(use-package tex
  :ensure auctex
  :mode ("\\.tex\\'" . latex-mode)
  :hook ((LaTeX-mode . prettify-symbols-mode)
         (LaTeX-mode . turn-on-reftex)
         (LaTeX-mode . LaTeX-math-mode)
         (LaTeX-mode . TeX-fold-mode)
         (LaTeX-mode . TeX-source-correlate-mode)
         (LaTeX-mode . eglot-ensure))
  :bind (:map LaTeX-mode-map
              ("s-/" . transform-previous-char))
  :config
  (setq-default Tex-master nil)
  (setq-default TeX-engine 'xetex)
  (setq-default LaTeX-electric-left-right-brace t)
  (setq TeX-auto-save t
        TeX-save-query nil
        TeX-parse-self t
        TeX-source-correlate-start-server t
        TeX-view-program-selection '((output-pdf "PDF Tools")))
  (setq reftex-plug-into-AUCTeX t)
  (add-hook 'TeX-after-comilation-finished-functions #'TeX-revert-document-buffer))

(use-package cdlatex
  :hook ((LaTeX-mode . turn-on-cdlatex)
         (org-mode   . turn-on-org-cdlatex)))

(use-package transform
  :defer 5
  :ensure nil)

(use-package edit-indirect)
(use-package markdown-mode
  :ensure t
  :mode (("README\\.md\\'" . gfm-mode))
  :hook (markdown-mode . olivetti-mode)
  :init
  (setq markdown-command (cond
                          (IS-WIN "markdown-py")
                          (t "pandoc"))
        markdown-enable-math t
        markdown-header-scaling t
        markdown-fontify-code-blocks-natively t))

(provide 'y-writing)
;;; y-writing.el ends here
