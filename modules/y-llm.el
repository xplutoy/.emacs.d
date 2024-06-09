;;; -*- lexical-binding: t -*-

;; Author: yangxue <yangxue.cs@foxmail.com>
;; Copyright (C) 2024, yangxue, all right reserved.
;; Created: 2024-06-07 14:57:53
;; Modified: <2024-06-09 14:36:05 yangx>
;; Licence: GPLv3

;;; Commentary:

;;

;;; Code:
(use-package gptel
  :custom
  (gptel-default-mode 'org-mode)
  (gptel-org-branching-context t)
  :config
  (let* ((host "api.moonshot.cn")
         (key (yx/common-auth-get-field host :secret)))
    (setq-default gptel-model "moonshot-v1-32k"
                  gptel-backend (gptel-make-openai "Moonshot-AI"
                                  :host host
                                  :key key
                                  :stream t
                                  :models '("moonshot-v1-32k"
                                            "moonshot-v1-128k"))))
  (add-hook 'gptel-post-stream-hook 'gptel-auto-scroll)
  (add-hook 'gptel-post-response-functions 'gptel-end-of-response))

(provide 'y-llm)
;;; y-llm.el ends here
