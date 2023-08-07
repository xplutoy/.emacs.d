;;; -*- coding: utf-8; lexical-binding: t; -*-
(use-package vertico
  :demand
  :config
  (setq vertico-resize nil)

  (vertico-mode 1)
  (vertico-mouse-mode 1)
  (vertico-indexed-mode 1)
  :bind (:map vertico-map
              ("C-q" . vertico-quick-insert)
              ("RET" . vertico-directory-enter)
              ("DEL" . vertico-directory-delete-char))
  :hook (rfn-eshadow-update-overlay . vertico-directory-tidy)
  )

(use-package orderless
  :custom
  (completion-styles '(orderless basic))
  )

(use-package embark
  :custom
  (embark-selection-indicator nil)
  (embark-prompter 'embark-completing-read-prompter)
  (embark-indicators '(embark-minimal-indicator
                       embark-highlight-indicator
                       embark-isearch-highlight-indicator))
  )
(use-package embark-consult
  :hook (embark-collect-mode . consult-preview-at-point-mode))

(use-package consult
  :init
  (setq xref-show-xrefs-function #'consult-xref
        xref-show-definitions-function #'consult-xref)

  :config
  (setq consult-ripgrep-args
        (concat consult-ripgrep-args " --hidden"))
  (consult-customize
   consult-theme
   :preview-key '(:debounce 0.2 any)
   consult-ripgrep consult-git-grep consult-recent-file consult-xref
   :preview-key '(:debounce 0.4 any))

  :bind (([remap goto-line]                     . consult-goto-line)
         ([remap switch-to-buffer]              . consult-buffer)
         ([remap switch-to-buffer-other-window] . consult-buffer-other-window)
         ([remap switch-to-buffer-other-frame]  . consult-buffer-other-frame)
         ([remap yank-pop]                      . consult-yank-pop)
         ([remap apropos]                       . consult-apropos)
         ([remap bookmark-jump]                 . consult-bookmark)
         ([remap imenu]                         . consult-imenu)
         ("C-x p b" . consult-project-buffer)
         ("M-#"     . consult-register-load)
         ("M-'"     . consult-register-store)
         ("C-M-#"   . consult-register)
         ("M-g m"   . consult-mark)
         ("M-g M"   . consult-global-mark)
         :map minibuffer-local-map
         ("M-s" . consult-history)
         ("M-r" . consult-history))
  )

(use-package consult-dir
  :bind (("C-x C-d" . consult-dir)
         :map vertico-map
         ("C-x C-d" . consult-dir)
         ("C-x C-j" . consult-dir-jump-file))
  )

(use-package consult-eglot
  :bind (("M-s s" . consult-eglot-symbols))
  )

;; %% corfu
(use-package corfu
  :custom
  (corfu-auto t)
  (corfu-preselect 'valid)
  (corfu-echo-documentation nil)
  :config
  (corfu-echo-mode 1)
  (corfu-history-mode 1)
  (corfu-indexed-mode 1)
  (corfu-popupinfo-mode 1)
  :hook ((text-mode prog-mode) . corfu-mode)
  :bind (:map corfu-map
              ("TAB"   . corfu-next)
              ("S-TAB" . corfu-previous)
              ("C-q"   . corfu-quick-insert)
              ("SPC"   . corfu-insert-separator))
  )

(use-package cape
  :init
  (setq cape-dabbrev-min-length 2)
  (add-to-list 'completion-at-point-functions #'cape-file)
  (add-to-list 'completion-at-point-functions #'cape-symbol)
  (add-to-list 'completion-at-point-functions #'cape-dabbrev)
  )

;; %% end
(provide 'init-comp)
