;;; core-helper.el --- help functions  -*- lexical-binding: t; -*-

;; Author: yangxue <yangxue.cs@foxmail.com>
;; Copyright (C) 2023, yangxue, all right reserved.
;; Created: 2023-08-28 17:41:00
;; Modified: <2023-09-30 10:38:47 yx>
;; Licence: GPLv3

;;; Commentary:

;;

;;; Code:
(require 'tempo)

(defvar yx/org-dir         "~/yxdocs/org-notes/")
(defvar yx/zotero-dir      "~/Zotero/")
(defvar yx/gpg-sign-key    "67B86CB8A5630C51!")
(defvar yx/gpg-encrypt-key "8B1F9B207AF00BCF!")

(defconst IS-MAC   (eq system-type 'darwin))
(defconst IS-WIN   (eq system-type 'windows-nt))
(defconst IS-LINUX (eq system-type 'gnu/linux))

(defvar yx/default-open-program
  (cond (IS-WIN   "start")
        (IS-MAC   "open")
        (IS-LINUX "xdg-open")
        (t "")))

(defconst yx/templates-dir
  (expand-file-name "templates" no-littering-etc-directory))

(defconst yx/cal-china-x-days
  ["日" "一" "二" "三" "四" "五" "六"])

(defconst yx/cal-china-x-month-name
  ["正月" "二月" "三月" "四月" "五月" "六月" "七月" "八月" "九月" "十月" "冬月" "腊月"])

(defconst yx/cal-china-x-day-name
  ["初一" "初二" "初三" "初四" "初五" "初六" "初七" "初八" "初九" "初十"
   "十一" "十二" "十三" "十四" "十五" "十六" "十七" "十八" "十九"  "廿"
   "廿一" "廿二" "廿三" "廿四" "廿五" "廿六" "廿七" "廿八" "廿九" "三十"
   "卅一" "卅二" "卅三" "卅四" "卅五" "卅六" "卅七" "卅八" "卅九" "卅十"])

(defun yx/file-contents-str (file)
  "File contents to string."
  (with-temp-buffer
    (insert-file-contents file)
    (buffer-string))
  )

(defun yx/org-agenda-format-date-aligned (date)
  "Format a DATE string for display in the daily/weekly agenda, or timeline.
      This function makes sure that dates are aligned for easy reading."
  (let* ((dayname (aref yx/cal-china-x-days
                        (calendar-day-of-week date)))
         (day (cadr date))
         (month (car date))
         (year (nth 2 date))
         (cn-date (calendar-chinese-from-absolute (calendar-absolute-from-gregorian date)))
         (cn-month (cl-caddr cn-date))
         (cn-day (cl-cadddr cn-date))
         (cn-month-string (concat (aref yx/cal-china-x-month-name
                                        (1- (floor cn-month)))
                                  (if (integerp cn-month)
                                      ""
                                    "[闰]")))
         (cn-day-string (aref yx/cal-china-x-day-name
                              (1- cn-day))))
    (format "%04d-%02d-%02d 周%-8s 农历%s%s" year month
            day dayname cn-month-string cn-day-string)))

(defun yx/is-it-dark-p ()
  "Return t if it's dark outside, otherwise nil."
  (let* ((solar (solar-sunrise-sunset (calendar-current-date)))
         (sunrise (car (nth 0 solar)))
         (sunset (car (nth 1 solar)))
         (time (decode-time (current-time)))
         (hour (nth 2 time))
         (minute (nth 1 time))
         (minute-frac (/ minute 60.0))
         (time-decimal (+ hour minute-frac)))
    (or (> time-decimal sunset) (< time-decimal sunrise)))
  )

(defun yx/diary-sunrise-sunset-split ()
  "Split `solar-sunrise-sunset-string' into sunrise, sunset, and daylight hours."
  (let* ((string (solar-sunrise-sunset-string (calendar-current-date)))
         (regexp (rx (group "Sunrise " (1+ (or digit ":")) (or "am" "pm")) " "
                     (group "(" (1+ alpha) ")") ", "
                     (group "sunset " (1+ (or digit ":")) (or "am" "pm")) " "
                     (group "(" (1+ alpha) ")")
                     (1+ anything)
                     "(" (group (1+ (or digit ":")))
                     ))
         (sunrise (progn
                    (string-match regexp string)
                    (match-string 1 string)) )
         (sunset (capitalize (match-string 3 string)))
         (daylight (format "%s of daylight" (match-string 5 string))))
    (list sunrise sunset daylight))
  )

(defun yx/diary-sunrise ()
  (elt (yx/diary-sunrise-sunset-split) 0))

(defun yx/diary-sunset ()
  (elt (yx/diary-sunrise-sunset-split) 1))

(cond
 (IS-MAC
  (defun yx/notify-send (&rest params)
    "Send notifications via `terminal-notifier'."
    (let ((title (plist-get params :title))
          (body (plist-get params :body)))
      (start-process "terminal-notifier"
                     nil
                     "terminal-notifier"
                     "-group" "Emacs"
                     "-title" title
                     "-message" body
                     "-activte" "org.gnu.Emacs"))))
 (t
  (defalias 'yx/notify-send 'notifications-notify)))

(defun yx/appt-display-with-notification (min-to-app new-time appt-msg)
  (yx/notify-send :title (format "Appointment in %s minutes" min-to-app)
                  :body appt-msg
                  :urgency 'critical)
  (appt-disp-window min-to-app new-time appt-msg))

;; %% yx defined tempo skeleton
(tempo-define-template
 "yx/tex-note-tmpl"
 `(,(yx/file-contents-str (expand-file-name "math-note.tmpl.tex" yx/templates-dir)))
 )

(define-skeleton yx/latex-graphics-skl
  "Insert centered picture."
  nil
  > "\\begin{center}" \n
  > "\\includegraphics[width=" @ (skeleton-read "Width: ") "]{" @ _ "}" \n
  > "\\begin{center}" > \n @)

(define-skeleton yx/auto-insert-h-header
  ""
  (replace-regexp-in-string
   "[^A-Z0-9]" "_"
   (string-replace "+" "P"
                   (upcase
                    (file-name-nondirectory buffer-file-name))))
  "/**\n***************************************************"
  "\n* @author: "
  (user-full-name)
  "\n* @date: "
  (format-time-string "%F %T")
  "\n* @brief: "
  (skeleton-read "brief: ")
  "\n* @modified: <>"
  "\n**************************************************\n*/"
  "\n\n#ifndef " str \n "#define " str
  "\n\n" @ _
  "\n\n#endif"
  )

(define-skeleton yx/auto-insert-c-header
  ""
  nil
  "/**\n***************************************************"
  "\n* @author: "
  (user-full-name)
  "\n* @date: "
  (format-time-string "%F %T")
  "\n* @modified: <>"
  "\n**************************************************\n*/"
  "\n\n" @ _ "\n"
  )

(define-skeleton yx/auto-insert-common-header
  ""
  nil
  "# --------------------------------------------------"
  "\n# Author: "
  (user-full-name)
  "\n# Date: "
  (format-time-string "%F %T")
  "\n# Modified: <>\n#"
  "\n# Description: "
  (skeleton-read "Description: ")
  "\n#\n#\n"
  "# --------------------------------------------------"
  "\n\n" @ _ "\n"
  )

(define-skeleton yx/auto-insert-el-header
  ""
  nil
  ";;; "
  (file-name-nondirectory
   (buffer-file-name))
  " --- "
  (skeleton-read "Descriptions: ")
  "  -*- lexical-binding: t; -*-"
  '(setq lexical-binding t)
  "\n\n;; Author: "
  (user-full-name)
  " <"
  (progn user-mail-address)
  ">"
  "\n;; Copyright (C) "
  (format-time-string "%Y")
  ", "
  (user-full-name)
  ", all right reserved."
  "\n;; Created: "
  (format-time-string "%F %T")
  "\n;; Modified: <>"
  "\n;; Licence: GPLv3"
  "\n\n;;; Commentary:\n\n;; " @ _
  "\n\n;;; Code:\n\n(provide '"
  (file-name-base
   (buffer-file-name))
  ")\n;;; "
  (file-name-nondirectory
   (buffer-file-name))
  " ends here\n"
  )


;; %% end
(provide 'core-helper)
;;; core-helper.el ends here
