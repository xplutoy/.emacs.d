;;; -*- lexical-binding: t -*-

;; Author: yangxue <yangxue.cs@foxmail.com>
;; Copyright (C) 2023, yangxue, all right reserved.
;; Created: 2023-08-24 22:58:30
;; Modified: <2024-02-01 10:56:37 yx>
;; Licence: GPLv3

;;; Commentary:

;; minibuffer

;;; Code:

;; %%
(use-package vertico
  :init
  (setq vertico-resize nil
        vertico-preselect 'directory)
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
  (completion-styles '(basic substring initials flex orderless))
  (completion-category-defaults nil)
  (completion-category-overrides
   '((file (styles . (basic partial-completion orderless)))
     (bookmark (styles . (basic substring)))
     (library (styles . (basic substring)))
     (embark-keybinding (styles . (basic substring)))
     (imenu (styles . (basic substring orderless)))
     (consult-location (styles . (basic substring orderless)))
     (kill-ring (styles . (emacs22 orderless)))
     (eglot (styles . (emacs22 substring orderless)))))
  )

;; %% embark
(use-package embark
  :init
  (setq prefix-help-command 'embark-prefix-help-command)
  :custom
  (embark-confirm-act-all nil)
  (embark-selection-indicator nil)
  (embark-prompter 'embark-completing-read-prompter)
  (embark-indicators '(embark-minimal-indicator
                       embark-highlight-indicator
                       embark-isearch-highlight-indicator))
  :bind
  ( :map embark-general-map
    (", b" . engine/search-bing)
    (", z" . engine/search-zhihu)
    :map
    embark-file-map
    (", s" . crux-sudo-edit)
    :map
    embark-identifier-map
    (", h" . symbol-overlay-put)
    :map
    minibuffer-local-map
    ("C-SPC" . (lambda () (interactive) (embark-select) (vertico-next)))
    ))

(use-package embark-consult
  :after embark
  :hook (embark-collect-mode . consult-preview-at-point-mode))

;; %% consult
(use-package consult
  :config
  (setq
   consult-narrow-key "?"
   xref-show-xrefs-function #'consult-xref
   xref-show-definitions-function #'consult-xref
   consult-ripgrep-args (concat consult-ripgrep-args " --hidden"))
  (consult-customize
   consult-theme :preview-key '(:debounce 0.2 any)
   consult-bookmark consult-recent-file :preview-key "M-.")
  :bind (:map minibuffer-local-map
              ("M-s" . consult-history)
              ("M-r" . consult-history))
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
  (corfu-cycle t)
  (corfu-preselect 'prompt)
  (corfu-quit-no-match t)
  (corfu-echo-documentation nil)
  :config
  (corfu-echo-mode 1)
  (corfu-history-mode 1)
  (corfu-indexed-mode 1)
  (corfu-popupinfo-mode 1)
  :bind
  (:map corfu-map
        ("M-q"     . corfu-quick-insert)
        ("M-SPC"   . corfu-insert-separator))
  )

(use-package cape
  :init
  (setq cape-dabbrev-min-length 3)
  (dolist (ele '(cape-abbrev cape-file))
    (add-to-list 'completion-at-point-functions ele))
  )

(use-package marginalia
  :hook (after-init . marginalia-mode)
  :bind (:map minibuffer-local-map
              ("M-A" . marginalia-cycle))
  )

;; %% end
(provide 'init-completion)
;;; init-completion ends here
