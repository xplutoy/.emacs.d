;;; -*- lexical-binding: t -*-

;; Author: yangxue <yangxue.cs@foxmail.com>
;; Copyright (C) 2024, yangxue, all right reserved.
;; Created: 2024-06-09 19:15:50
;; Licence: GPLv3

;;; Commentary:

;;

;;; Code:

(defun x-iedit-scoped (orig-fn)
  "Call `iedit-mode' with function-local scope, or global scope if called with a universal prefix."
  (interactive)
  (pcase-exhaustive current-prefix-arg
    ('nil (funcall orig-fn '(0)))
    ('(4) (funcall orig-fn))))

(provide 'x-iedit)
;;; x-iedit.el ends here
