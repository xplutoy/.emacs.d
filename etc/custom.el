;;; custom.el -*- lexical-binding: t; -*-

;; Author: yangxue <yangxue.cs@foxmail.com>
;; Copyright (C) 2023, yangxue, all right reserved.
;; Created: 2023-08-27 00:33:17
;; Modified: <2024-01-08 20:27:34 yx>
;; Licence: GPLv3

;;; Commentary:

;;

;;; Code:

(provide 'custom)
;;; custom.el ends here
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(magit-todos-insert-after '(bottom) nil nil "Changed by setter of obsolete option `magit-todos-insert-at'")
 '(org-preview-latex-image-directory (no-littering-expand-var-file-name "ltxinmg/") nil nil "Customized with use-package org")
 '(package-selected-packages
   '(snap-indent surround goto-chg org-web-tools graphviz-dot-mode org-download lua-ts-mode symbol-overlay citar-embark lin haskell-mode quickrun editorconfig eshell-syntax-highlighting dwim-shell-command dape sideline-eldoc sideline-flymake sideline yasnippet-snippets yasnippet edit-indirect markdown-mode ef-themes mixed-pitch citre puni hungry-delete marginalia dirvish tldr git-modes breadcrumb breadcrumb-mode devdocs ws-butler tabspaces bug-hunter command-log-mode apheleia zoxide engine-mode burly helpful citar-denote denote org-project geiser-chez sis ace-link gnuplot-mode rainbow-mode drag-stuff circadian ace-window elfeed-webkit elfeed olivetti pdf-tools vimrc-mode ess julia-snail eglot-jl julia-ts-mode julia-mode poetry pyvenv-auto pyvenv code-cells jupyter rainbow-delimiters hl-todo indent-guide aggressive-indent magit diff-hl tempel citar cdlatex auctex org-transclusion org-appear valign org-modern org-ql pcmpl-args shackle popper zoom dired-collapse diredfl ibuffer-vc fanyi osx-dictionary flyspell-correct jinx vundo easy-kill avy goggles which-key posframe no-littering gcmh key-chord cape corfu consult-eglot consult-dir embark-consult embark orderless vertico minions benchmark-init))
 '(package-vc-selected-packages
   '((lua-ts-mode :vc-backend Git :url "https://git.sr.ht/~johnmuhl/lua-ts-mode"))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(mode-line ((t :box (:style released-button)))))
