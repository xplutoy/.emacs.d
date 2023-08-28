;;; init-help.el --- help functions  -*- lexical-binding: t; -*-

;; Author: yangxue <yangxue.cs@foxmail.com>
;; Copyright (C) 2023, yangxue, all right reserved.
;; Created: 2023-08-28 17:41:00
;; Modified: <2023-08-29 02:12:34 yx>
;; Licence: GPLv3

;;; Commentary:

;;

;;; Code:

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


;; %% end
(provide 'init-help)
;;; init-help.el ends here
