;;; -*- lexical-binding: t -*-

;; Author: yangxue <yangxue.cs@foxmail.com>
;; Copyright (C) 2024, yangxue, all right reserved.
;; Created: 2024-06-07 11:51:58
;; Modified: <2024-06-10 11:01:18 yangx>
;; Licence: GPLv3

;;; Commentary:

;;

;;; Code:

(use-package vertico
  :hook (after-init . vertico-mode)
  :bind (:map vertico-map
              ("M-q" . vertico-quick-insert)
              ("M-r" . vertico-repeat-select)
              ("RET" . vertico-directory-enter)
              ("DEL" . vertico-directory-delete-char))
  :custom
  (vertico-resize nil)
  (vertico-preselect 'directory)
  :config
  (vertico-mouse-mode +1)
  (vertico-indexed-mode +1)

  (with-eval-after-load 'savehist
    (add-to-list 'savehist-additional-variables 'vertico-repeat-history))

  (add-hook 'minibuffer-setup-hook #'vertico-repeat-save)
  (add-hook 'rfn-eshadow-update-overlay #'vertico-directory-tidy))

(use-package orderless
  :demand t
  :custom
  (orderless-component-separator  #'orderless-escapable-split-on-space)
  (orderless-matching-styles '(orderless-literal orderless-regexp orderless-flex))
  :config
  (orderless-define-completion-style yx/orderless-with-initialism
    (orderless-matching-styles '(orderless-initialism orderless-literal orderless-regexp)))
  (setq completion-styles '(basic yx/orderless-with-initialism)
        completion-category-defaults nil
        completion-category-overrides '((file (styles basic partial-completion))
                                        (eglot (styles basic yx/orderless-with-initialism))
                                        (eglot-capf (styles basic yx/orderless-with-initialism)))))

(use-package embark
  :commands embark-open-externally
  :bind (("C-." . embark-act)
         :map minibuffer-local-map
         ("C-c C-e" . embark-export)
         ("C-c C-c" . embark-collect)
         ("C-SPC" . (lambda () (interactive) (embark-select) (vertico-next)))
         :map  embark-general-map
         ("h" . yx/consult-outline-insert-heading))
  :custom
  (embark-help-key "?")
  (embark-cycle-key ".")
  (embark-confirm-act-all nil)
  (embark-selection-indicator nil)
  (prefix-help-command 'embark-prefix-help-command)
  (embark-indicators '(embark-minimal-indicator
                       embark-highlight-indicator
                       embark-isearch-highlight-indicator)))

(use-package consult
  :bind (:map minibuffer-local-map
              ("M-h" . consult-history))
  :custom
  (consult-narrow-key "<")
  (consult-line-start-from-top t)
  :config
  (setq xref-show-xrefs-function #'consult-xref
        xref-show-definitions-function #'consult-xref)
  (setq register-preview-function #'consult-register-format)
  (advice-add #'register-preview :override #'consult-register-window)
  (setq consult-ripgrep-args (concat consult-ripgrep-args " --hidden"))
  (consult-customize
   consult-theme :preview-key '(:debounce 0.2 any)
   consult-bookmark
   consult-recent-file
   consult--source-buffer
   consult--source-recent-file :preview-key "M-.")
  (use-package embark-consult
    :demand t
    :hook (embark-collect-mode . consult-preview-at-point-mode)))

(use-package consult-dir
  :after consult
  :bind (:map vertico-map
              ("C-x C-d" . consult-dir)
              ("C-x C-j" . consult-dir-jump-file)))

(use-package cape
  :init
  (setq cape-dabbrev-min-length 3)
  (add-to-list 'completion-at-point-functions #'cape-dabbrev)
  (add-to-list 'completion-at-point-functions #'cape-file)
  :config
  (cape-wrap-prefix-length #'cape-dict 4)
  (cape-wrap-prefix-length #'cape-line 4))

(use-package corfu
  :hook ((text-mode prog-mode) . corfu-mode)
  :custom
  (corfu-auto t)
  (corfu-auto-delay 0.5)
  (corfu-cycle t)
  (corfu-popupinfo-delay nil)
  :config
  (corfu-echo-mode 1)
  (corfu-history-mode 1)
  (corfu-indexed-mode 1)
  (corfu-popupinfo-mode 1)
  (keymap-set corfu-map "M-q" #'corfu-quick-insert)
  (use-package corfu-terminal
    :unless (display-graphic-p)
    :init
    (corfu-terminal-mode +1)))

(use-package marginalia
  :defer 3
  :custom
  (marginalia-align 'left)
  (marginalia-align-offset 10)
  :config
  (marginalia-mode +1))

(provide 'y-completion)
;;; y-completion.el ends here
