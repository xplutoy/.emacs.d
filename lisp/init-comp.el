;;; init-comp.el --- minibuffer  -*- lexical-binding: t; -*-

;; Author: yangxue <yangxue.cs@foxmail.com>
;; Copyright (C) 2023, yangxue, all right reserved.
;; Created: 2023-08-24 22:58:30
;; Modified: <2023-09-13 22:15:19 yx>
;; Licence: GPLv3

;;; Commentary:

;; minibuffer

;;; Code:

;; %%
(use-package vertico
  :init
  (setq vertico-resize nil)
  (vertico-mode 1)
  (vertico-mouse-mode 1)
  (vertico-indexed-mode 1)
  :bind (:map vertico-map
              ("M-q" . vertico-quick-insert)
              ("RET" . vertico-directory-enter)
              ("DEL" . vertico-directory-delete-char))
  :hook (rfn-eshadow-update-overlay . vertico-directory-tidy)
  )

(use-package orderless
  :custom
  (completion-styles '(orderless basic flex))
  (completion-category-overrides
   '((file (styles basic partial-completion))
     (eglot (styles orderless basic))))
  )

;; %% embark
(use-package embark
  :custom
  (embark-selection-indicator nil)
  (embark-prompter 'embark-completing-read-prompter)
  (embark-indicators '(embark-minimal-indicator
                       embark-highlight-indicator
                       embark-isearch-highlight-indicator))
  )
(use-package embark-consult
  :after embark
  :hook (embark-collect-mode . consult-preview-at-point-mode))

;; %% consult
(use-package consult
  :config
  (setq
   consult-narrow-key "<"
   xref-show-xrefs-function #'consult-xref
   xref-show-definitions-function #'consult-xref
   consult-ripgrep-args (concat consult-ripgrep-args " --hidden"))
  (consult-customize
   consult-theme
   :preview-key '(:debounce 0.2 any)
   consult-ripgrep
   consult-git-grep
   consult-recent-file
   consult-xref
   :preview-key '(:debounce 0.4 any))
  :bind (:map minibuffer-local-map
              ("M-s" . consult-history)
              ("M-r" . consult-history))
  :preface
  (defun yx/consult-emacs-info ()
    "Search through Emacs info pages."
    (interactive)
    (consult-info "emacs" "elisp" "cl" "compat"))
  )

(use-package consult-dir
  :after consult
  :bind (:map vertico-map
              ("C-x C-d" . consult-dir)
              ("C-x C-j" . consult-dir-jump-file))
  )

;; %% corfu
(use-package corfu
  :hook ((text-mode prog-mode) . corfu-mode)
  :custom
  (corfu-auto t)
  (corfu-preselect 'valid)
  (corfu-quit-no-match t)
  (corfu-echo-documentation nil)
  :config
  (corfu-echo-mode 1)
  (corfu-history-mode 1)
  (corfu-indexed-mode 1)
  (corfu-popupinfo-mode 1)
  :bind (:map corfu-map
              ("TAB"   . corfu-next)
              ("S-TAB" . corfu-previous)
              ("M-q"   . corfu-quick-insert)
              ("M-SPC"   . corfu-insert-separator))
  )

(use-package cape
  :hook
  (org-mode    . yx/cape-capf-setup-org)
  (prog-mode   . yx/cape-capf-setup-prog)
  (LaTeX-mode  . yx/cape-capf-setup-latex)
  (eshell-mode . yx/cape-capf-setup-eshell)
  :init
  (setq cape-dabbrev-min-length 3)
  (dolist (ele '(cape-dict
                 cape-dabbrev
                 cape-symbol
                 cape-abbrev
                 cape-file))
    (add-to-list 'completion-at-point-functions ele))
  :preface
  (defun yx/cape-capf-setup-org ()
    (dolist (ele `(,(cape-super-capf 'cape-dabbrev 'cape-dict)
                   cape-tex))
      (add-to-list 'completion-at-point-functions ele)))
  (defun yx/cape-capf-setup-latex ()
    (dolist (ele '(cape-dict
                   cape-tex))
      (add-to-list 'completion-at-point-functions ele)))
  (defun yx/cape-capf-setup-eshell ()
    (dolist (ele '(cape-file
                   cape-history))
      (add-to-list 'completion-at-point-functions ele)))
  (defun yx/cape-capf-setup-prog ()
    (dolist (ele `(,(cape-super-capf 'cape-dabbrev 'cape-symbol 'cape-keyword)))
      (add-to-list 'completion-at-point-functions ele)))
  )

;; %% end
(provide 'init-comp)
;;; init-comp ends here
