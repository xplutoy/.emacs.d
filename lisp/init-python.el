;;; -*- lexical-binding: t -*-

;; Author: yangxue <yangxue.cs@foxmail.com>
;; Copyright (C) 2023, yangxue, all right reserved.
;; Created: 2023-09-15 22:06:10
;; Modified: <2023-12-05 20:25:15 yx>
;; Licence: GPLv3

;;; Commentary:

;;

;;; Code:
(defvar yx/default-python-env "~/workspace/.venv/")

(add-hook
 'python-ts-mode-hook
 (lambda()
   (setq-local
    tab-width 4
    python-indent-offset 4
    electric-indent-inhibit t
    imenu-create-index-function 'python-imenu-create-flat-index
    ))
 )

(setq
 python-shell-dedicated t
 python-skeleton-autoinsert t
 python-indent-guess-indent-offset t
 python-indent-guess-indent-offset-verbose nil
 python-shell-virtualenv-root yx/default-python-env
 python-shell-interpreter "jupyter"
 python-shell-interpreter-args "console --simple-prompt"
 python-shell-completion-native-disabled-interpreters '("ipython" "jupyter"))

(define-auto-insert "\\.py$" 'yx/auto-insert-common-header)

(use-package pyvenv
  :hook (after-init . yx/active-default-pyvenv)
  :preface
  (defun yx/active-default-pyvenv ()
    (interactive)
    (pyvenv-activate yx/default-python-env)
    )
  )

(use-package pyvenv-auto
  :hook (python-ts-mode . pyvenv-auto-run))

(use-package poetry
  :hook (python-ts-mode . poetry-tracking-mode))

(use-package jupyter
  :after org
  :demand t
  :config
  (setq jupyter-eval-use-overlays nil)
  ;; @see https://github.com/emacs-jupyter/jupyter/issues/478
  (setf (alist-get "python" org-src-lang-modes nil nil #'equal) 'python-ts)
  )

(provide 'init-python)
;;; init-python.el ends here
