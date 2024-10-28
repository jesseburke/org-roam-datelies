;;; org-roam-datelies.el --- day, week, ...lies-notes for Org-roam -*- coding: utf-8; lexical-binding: t; -*-

;; Author: Jesse Burke <jtb445@gmail.com>
;;
;; URL: https://github.com/jesseburke/org-roam-datelies
;; Keywords: org-mode, roam
;; Version: 0.0.1
;; Package-Requires: ((emacs "27.1") (org-roam "2.2.2"))

;; This file is NOT part of GNU Emacs.

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 3, or (at your option)
;; any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to the
;; Free Software Foundation, Inc., 51 Franklin Street, Fifth Floor,
;; Boston, MA 02110-1301, USA.

;;; vars and customizations

(setq org-roam-database-connector 'sqlite-builtin)

(require 'cl-lib)
(require 'org-roam)
(require 'cal-iso)
(require 'f)

(defvar org-roam-directory)
(defvar org-roam-file-extensions)
(defvar org-roam-capture--info)
(declare-function org-roam-file-p        "org-roam")

(defgroup org-roam-datelies nil
  "day, week, etc files for org-roam."
  :group 'org-roam
  :prefix "org-roam-datelies-"
  :link '(url-link :tag "Github" "https://github.com/jesseburke/org-roam-datelies"))  

(defcustom org-roam-datelies-dir "datelies/"
  "Path to directory for datelie notes (relative to `org-roam-directory')."
  :group 'org-roam-datelies
  :type 'string)

(defcustom orl-day-tag "orl-day" "tag for orl" :group 'org-roam-datelies
  :type 'string)
(defcustom orl-week-tag "orl-week" "tag for orl" :group 'org-roam-datelies
  :type 'string)
(defcustom orl-month-tag "orl-month" "tag for orl" :group 'org-roam-datelies
  :type 'string)
(defcustom orl-quarter-tag "orl-quarter" "tag for orl" :group 'org-roam-datelies
  :type 'string)
(defcustom orl-year-tag "orl-year" "tag for orl" :group 'org-roam-datelies
  :type 'string)
(defcustom orl-ever-tag "orl-ever" "tag for orl" :group 'org-roam-datelies
  :type 'string)

(defvar orl-tag-list (list orl-day-tag orl-week-tag orl-month-tag
                           orl-quarter-tag orl-year-tag orl-ever-tag))

(defun orl-roam-node-has-orl-tags-p (node)
  (let ((node-tag-list (org-roam-node-tags node)))
    (cl-some (lambda (tag) (member tag orl-tag-list)) node-tag-list)))

;;; date and time functions

;;;; time plus ...
(defun ordlies--time-plus-days (time no-days)
  (interactive)
  (time-add time (* 86400 no-days)))

(defun ordlies--time-plus-weeks (time no-weeks)
  (interactive)
  (time-add time (* 86400 7 no-weeks)))

(defun time-plus-months (time no-months)
  "NO-MONTHS should be between -11 and 11."
  (cl-destructuring-bind (sec min hour day month year _ _ _) (decode-time time)
    (pcase (+ month no-months)
      ((pred (lambda (new-month) (<= new-month 12)))
       (encode-time sec min hour day (+ month no-months) year))
      ((pred (lambda (new-month) (> new-month 12)))
       (encode-time sec min hour day (- (+ month no-months) 12) (+ year 1)))
      ((pred (lambda (new-month) (< new-month 1)))
       (encode-time sec min hour day (- 12 (+ month no-months)) (- year 1))))))

(with-no-warnings
  (defun ordlies--time-plus-quarter (time no-months)
    "NO-MONTHS should be 1 or -1; if TIME represents the date 15 june,
for example, then this will return the time of 15 sep, when no-months
  is 1, and the time of 15 march, when no-months is -1."
    (pcase-let ((`(,sec ,min ,hour ,day ,month ,year)
                 (decode-time time)))
      (if (equal no-months 1)
          (if (< month 10) (encode-time sec min hour day (+ month 3) year)
            (encode-time sec min hour day (- month 9) (+ year 1)))
        (if (equal no-months -1)
            (if (> month 3) (encode-time sec min hour day (- month 3) year)
              (encode-time sec min hour day (+ 9 month) (- year 1)))
          (message "ordlies--time-plus-quarter should be 1 or -1")))))) 
    
(defun ordlies--time-plus-years (time no-years)
  (pcase-let ((`(,sec ,min ,hour ,day ,month ,year)
               (decode-time time)))
    (encode-time sec min hour day month (+ year no-years))))

;;;; time to dates

;; This needs more care because week numbers can be a bit tricky.
(defun ordlies--time-to-week-number-and-year (time)
  "Returns week number and year of the week containing TIME. The
first week of a year is the week that contains Jan 1. E.g., Jan.
1, 2024 is on a Monday, so this is W01-2024. And thus, this
function returns (1 2024) when passed a time in the day Dec 31,
2023. This function works by finding the day of the week,
0(Sunday) - 6(Saturday), of time. Uses this to find the Saturday of the
week containing time. Subtracting a given time on this Saturday
from the Saturday of week 1, can calculate how many weeks the
difference is."
  (let* ((time-day-of-week (string-to-number (format-time-string "%w" time)))
         (time-on-week-end-day
          (ordlies--time-plus-days time (- 7 (1+ time-day-of-week))))
         (year (string-to-number (format-time-string "%Y" time-on-week-end-day)))
         (week1-end-time (cadr (ordlies--week1-start-and-end-times year)))
         (time-difference-in-secs (floor (float-time (time-subtract time-on-week-end-day week1-end-time ))))
         (secs-in-a-week (* 7 (* 24 (* 60 60)))))
    (list (1+ (/ time-difference-in-secs secs-in-a-week)) year)))

(defun ordlies--time-to-day-month-quarter-year (time)
  (let* ((str (format-time-string "%Y%q%m%d" time))
         (year (string-to-number (substring str 0 4)))
         (quarter (string-to-number (substring str 4 5)))
         (month (string-to-number (substring str 5 7)))
         (day (string-to-number (substring str 7 9))))       
    (list day month quarter year)))

;;;; date to time
  
(defun ordlies--day-start-and-end-times (day month year)
  (list (encode-time 1 0 0 day month year)
        (encode-time 59 59 11 day month year)))

(defun ordlies--week1-start-and-end-times (year)
  "Returns a time on the start day and a time on the end day of the
week containing Jan. 1, YEAR."
  (let* ((jan1-time (car (ordlies--day-start-and-end-times 1 1 year)))
         (jan1-day-of-week (string-to-number (format-time-string "%w" jan1-time)))
         start-time end-time)
    (if (eq jan1-day-of-week 0)
        (progn (setq start-time (encode-time 1 0 0 1 1 year))
               (setq end-time (encode-time 1 0 0 6 1 year)))
      (setq start-time (encode-time 1 0 0 (- 31 (1- jan1-day-of-week)) 12 (1- year)))
      (setq end-time (encode-time 1 0 0 (- 7 jan1-day-of-week) 1 year)))
    (list start-time end-time)))

(defun ordlies--week-start-and-end-times (week year)
  "Returns a time on the start day and a time on the end day of the
week WEEK on YEAR. Weeks are numbered so that the week that Jan 1
occurs in is Week 1."
  (seq-map (lambda (time) (ordlies--time-plus-weeks time (1- week))) (ordlies--week1-start-and-end-times
                                                             year)))

(defun ordlies--month-start-and-end-times (month year)
  (list (encode-time 0 0 0 1 month year)
        (encode-time 0 59 23 (calendar-last-day-of-month month year) month year)))

(defun ordlies--quarter-start-and-end-times (quarter year)
  (cond ((eq quarter 1) (list (encode-time 0 0 0 1 1 year)  (encode-time 0 59 23 (calendar-last-day-of-month 3 year) 3 year)))
        ((eq quarter 2) (list (encode-time 0 0 0 1 4 year)  (encode-time 0 59 23 (calendar-last-day-of-month 6 year) 6 year)))
        ((eq quarter 3) (list (encode-time 0 0 0 1 7 year)  (encode-time 0 59 23 (calendar-last-day-of-month 9 year) 9 year)))
        ((eq quarter 4) (list (encode-time 0 0 0 1 10 year)
                              (encode-time 0 59 23 (calendar-last-day-of-month 10 year) 12
                                           year)))))

(defun ordlies--year-start-and-end-times (year)
  (list (encode-time 0 0 0 1 1 year)  (encode-time 0 59 23 31 12 year)))

(defun ordlies--time-period-start-and-end-times (time-period time-data)
  "Time-data is a list that depends on time-period, e.g., if time-period
is \='day, then time-data is a list of the form (DAY MONTH YEAR)."
  (pcase time-period
    ('day (apply #'ordlies--day-start-and-end-times time-data))
    ('week (apply #'ordlies--week-start-and-end-times time-data))
    ('month (apply #'ordlies--month-start-and-end-times time-data))
    ('quarter (apply #'ordlies--quarter-start-and-end-times time-data))
    ('year (apply #'ordlies--year-start-and-end-times time-data))))

(defun ordlies--time-in-time-period-p (time-to-check time-period time-data)
  (cl-destructuring-bind (start-time end-time)
      (ordlies--time-period-start-and-end-times time-period time-data)
    (and (or (time-equal-p start-time time-to-check)
             (time-less-p start-time time-to-check))
         (or (time-equal-p time-to-check end-time)
             (time-less-p time-to-check end-time)))))

(defun ordlies--time-period-under-time-period-p (time-period time-data
                                                    time-period-tc
                                                    time-data-tc)
  "Checks whether the time span determined by time-period and
time-data contains the time span determined by time-period-tc (to
check) and time-data-tc. Time-data is a list that depends on
time-period, e.g., if time-period is \='day, then time-data is a
list of the form (DAY MONTH YEAR)."
  (cl-destructuring-bind (start end) (ordlies--time-period-start-and-end-times
                                      time-period-tc time-data-tc)
    (and (ordlies--time-in-time-period-p start time-period time-data)
         (ordlies--time-in-time-period-p end time-period time-data))))

;;; org-roam-datelies-node definition

(cl-defstruct (org-roam-datelies-node (:include org-roam-node) (:constructor org-roam-datelies-node-create))
  time-period time directory template)

(defun ordlies--make-orl--template (file-str head-str)
  `("d" "default" entry
    "* %?"
    :if-new (file+head ,(expand-file-name file-str org-roam-directory) ,head-str)))

(defun org-roam-datelies--create-node (time-period time)
  (let* (directory template)
    (cl-destructuring-bind (_ month quarter year)
        (ordlies--time-to-day-month-quarter-year time)      
      (pcase time-period
        ('day
         (setq directory (concat org-roam-datelies-dir "daily/"))
         (setq template
               (ordlies--make-orl--template (concat directory "%<%Y-%m-%d>.org")
                                   (concat "#+title:%<%Y-%m-%d>\n#+filetags: :"
                                           orl-day-tag ":\n\n" (format-time-string "%A, %F" time) "\n\n"))))
        ('week
         (cl-destructuring-bind (week week-year)
             (ordlies--time-to-week-number-and-year time)
           (cl-destructuring-bind (start-time end-time)
               (ordlies--week-start-and-end-times week week-year)             
             (setq directory (concat org-roam-datelies-dir "weekly/"))
             (setq template
                   (ordlies--make-orl--template
                    (concat directory
                            (concat (number-to-string week-year)
                                    "-W"
                                    (format "%02d" week)
                                    ".org"))
                    (concat "#+title: " (number-to-string week-year)
                            " week " (format "%02d" week) "\n#+filetags: :" orl-week-tag ":\n\n"
                                    (format-time-string "%A, %F"
                                                        start-time)
                                    " -- "(format-time-string "%A, %F" end-time) "\n\n"))))))
         ('month
          (cl-destructuring-bind (start-time end-time)
              (ordlies--month-start-and-end-times month year)
            (setq directory (concat org-roam-datelies-dir "monthly/"))
            (setq template
                  (ordlies--make-orl--template (concat directory "%<%Y-%m>.org")
                                      (concat "#+title: %<%Y %B>\n#+filetags: :" orl-month-tag ":\n\n"
                                              (format-time-string "%A, %F"
                                                                  start-time)
                                              " -- " (format-time-string "%A, %F" end-time) "\n\n")))))
         ('quarter
          (cl-destructuring-bind (start-time end-time)
              (ordlies--quarter-start-and-end-times quarter year)
            (setq directory (concat org-roam-datelies-dir "quarterly/"))
            (setq template
                  (ordlies--make-orl--template (concat directory "%<%Y-%q>.org")
                                      (concat "#+title: %<%Y quarter %q>\n#+filetags: :" orl-quarter-tag ":\n\n"
                                              (format-time-string "%A, %F"
                                                                  start-time)
                                              " -- " (format-time-string "%A, %F" end-time) "\n\n")))))
         ('year
          (cl-destructuring-bind (start-time end-time)
              (ordlies--year-start-and-end-times year)
            (setq directory (concat org-roam-datelies-dir "yearly/"))
            (setq template
                  (ordlies--make-orl--template (concat directory "%<%Y>.org")
                                      (concat "#+title: %<%Y>\n#+filetags: :" orl-year-tag ":\n\n"
                                              (format-time-string "%A, %F"
                                                                  start-time)
                                              " -- " (format-time-string "%A, %F" end-time) "\n\n")))))
         ('ever
          (cl-destructuring-bind (start-time end-time)
              (ordlies--year-start-and-end-times year)
            (setq directory (concat org-roam-datelies-dir "ever/"))
            (setq template
                  (ordlies--make-orl--template (concat directory "ever.org")
                                      (concat "#+title: ever file\n#+filetags: :" orl-ever-tag ":\n\n"
                                              (format-time-string "%A, %F"
                                                                  start-time)
                                              " -- " (format-time-string "%A, %F" end-time) "\n\n")))))))
    (org-roam-datelies-node-create :time-period time-period :time time
                                 :directory directory :template template)))

;;; node util functions

(defun ordlies--get-node-time-period (&optional node)
  "Return day, week, month, quarter, or ever if node is an
Org-roam-lies node, nil otherwise.
If node isn't specified, use node at point."
  (unless node (setq node (org-roam-node-at-point)))
  (let ((tag-list (org-roam-node-tags node)) return-str)
    (dolist (tag tag-list)
      (if (string-prefix-p "orl-" tag)
          (setq return-str (substring tag 4 nil))))
    (if return-str
        (intern return-str)
      (ordlies--type-from-file-name (org-roam-node-file node)))))

(defun ordlies--type-from-file-name (&optional file)
  "Return day, week, month, quarter, or ever if FILE is an
Org-roam-lies note, nil otherwise.
If FILE is not specified, use the current buffer's file-path."
  (interactive)
  (let ((dd (expand-file-name "daily" (expand-file-name org-roam-datelies-dir org-roam-directory)))
        (wd (expand-file-name "weekly" (expand-file-name org-roam-datelies-dir org-roam-directory)))
        (md (expand-file-name "monthly" (expand-file-name org-roam-datelies-dir org-roam-directory)))
        (qd (expand-file-name "quarterly" (expand-file-name org-roam-datelies-dir org-roam-directory)))
        (yd (expand-file-name "yearly" (expand-file-name org-roam-datelies-dir org-roam-directory)))
        (ed (expand-file-name "ever" (expand-file-name org-roam-datelies-dir org-roam-directory))))
    (when-let ((path (expand-file-name
                      (or file
                          (buffer-file-name (buffer-base-buffer))))))
      (setq path (expand-file-name path))
      (save-match-data
        (cond ((f-descendant-of-p path dd) 'day)
              ((f-descendant-of-p path wd) 'week)
              ((f-descendant-of-p path md) 'month)
              ((f-descendant-of-p path qd) 'quarter)
              ((f-descendant-of-p path yd) 'year)
              ((f-descendant-of-p path ed) 'ever))))))

(defun ordlies--time-data-from-file-name (time-period filename)
  "Assumes that dailies are names: YYYY-MM-DD, weeklies YYYY-WW,
  monthlies YYYY-MM, quarterlies YYYY-Q, and yearlies YYYY."
  (setq filename (file-name-base filename))
  (pcase time-period
    ('day
     (let ((year (string-to-number (substring filename 0 4)))
           (month (string-to-number (substring filename 5 7)))
           (day (string-to-number (substring filename 8 10))))
       (list day month year)))
    ('week
     (let ((year (string-to-number (substring filename 0 4)))
           (week-no (string-to-number (substring filename 6 8))))
       (list week-no year)))
    ('month
     (let ((year (string-to-number (substring filename 0 4)))
           (month (string-to-number (substring filename 5 7))))
       (list month year)))
    ('quarter
     (let ((year (string-to-number (substring filename 0 4)))
           (quarter (string-to-number (substring filename 5 6))))
       (list quarter year)))
    ('year
     (list (string-to-number (substring filename 0 4))))))

(defun ordlies--node-start-and-end-times (&optional node)
  "Returns a list of the form (start-time end-time)."
  (unless node (setq node (org-roam-node-at-point)))
  (let* ((timeperiod (ordlies--get-node-time-period))
         (filename (org-roam-node-file node))
         (timedata (ordlies--time-data-from-file-name timeperiod filename)))
    (ordlies--time-period-start-and-end-times timeperiod timedata)))

(defun ordlies--time-in-node-time-period (&optional node)
  "returns a single time in the time period of the current file"
  (car (ordlies--node-start-and-end-times node)))

;;; capture
(add-to-list 'org-roam-capture--template-keywords
             :override-default-time)

;;;###autoload
(defun org-roam-datelies--capture (time node &optional goto)
  "create a weekly, monthly, etc for time, creating it if neccessary"
  (org-roam-capture- :goto (when goto '(4))
                     :node node
                     :templates (list (org-roam-datelies-node-template node))
                     :props (list :override-default-time time)))

(add-hook 'org-roam-capture-preface-hook
          #'org-roam-datelies--override-capture-time-h)

(defun org-roam-datelies--override-capture-time-h ()
  "Override the `:default-time' with the time from `:override-default-time'."
  (prog1 nil
    (when (org-roam-capture--get :override-default-time)
      (org-capture-put :default-time (org-roam-capture--get :override-default-time)))))

;;; find functions
(defun org-roam-datelies-find-this-day ()
  "Find the daily-note for today, creating it if necessary."
  (interactive)
  (org-roam-datelies--capture (current-time)                     
                          (org-roam-datelies--create-node 'day (current-time)) t))

(defun org-roam-datelies-find-date-for-day (&optional prefer-future)
  "Find the daily-note for a date using the calendar."
  (interactive "P")
  (let ((time (let ((org-read-date-prefer-future prefer-future))
                (org-read-date t t nil "Find daily-note: "))))
    (org-roam-datelies--capture time (org-roam-datelies--create-node 'day time) t)))

(defun org-roam-datelies-find-this-week ()
  "Find the weekly-note for this week, creating it if necessary."
  (interactive)
  (org-roam-datelies--capture (current-time)
                          (org-roam-datelies--create-node 'week (current-time)) t))

(defun org-roam-datelies-find-date-for-week (&optional prefer-future)
  "Find the weekly-note for a date using the calendar."
  (interactive "P")
  (let ((time (let ((org-read-date-prefer-future prefer-future))
                (org-read-date t t nil "Find weekly-note: "))))
    (org-roam-datelies--capture time (org-roam-datelies--create-node 'week time) t)))

(defun org-roam-datelies-find-this-month ()
  "Find the monthly-note for this month, creating it if necessary."
  (interactive)
  (org-roam-datelies--capture (current-time)
                          (org-roam-datelies--create-node 'month (current-time)) t))

(defun org-roam-datelies-find-date-for-month (&optional prefer-future)
  "Find the monthly-note for a date using the calendar."
  (interactive "P")
  (let ((time (let ((org-read-date-prefer-future prefer-future))
                (org-read-date t t nil "Find monthly-note: "))))
    (org-roam-datelies--capture time(org-roam-datelies--create-node 'month time) t)))

(defun org-roam-datelies-find-this-quarter ()
  "Find the quarterly-note for this quarter, creating it if necessary."
  (interactive)
  (org-roam-datelies--capture (current-time)
                          (org-roam-datelies--create-node 'quarter (current-time)) t))

(defun org-roam-datelies-find-date-for-quarter (&optional prefer-future)
  "Find the quarterly-note for a date using the calendar."
  (interactive "P")
  (let ((time (let ((org-read-date-prefer-future prefer-future))
                (org-read-date t t nil "Find quarterly-note: "))))
    (org-roam-datelies--capture time                  
                            (org-roam-datelies--create-node 'quarter time) t)))

(defun org-roam-datelies-find-this-year ()
  "Find the yearly-note for this year, creating it if necessary."
  (interactive)
  (org-roam-datelies--capture (current-time)
                          (org-roam-datelies--create-node 'year (current-time)) t))

(defun org-roam-datelies-find-date-for-year (&optional prefer-future)
  "Find the yearly-note for a date using the calendar."
  (interactive "P")
  (let ((time (let ((org-read-date-prefer-future prefer-future))
                (org-read-date t t nil "Find yearly-note: "))))
    (org-roam-datelies--capture time (org-roam-datelies--create-node 'year (current-time)) t)))

(defun org-roam-datelies-find-ever ()
  "Find the everly-note, creating it if necessary."
  (interactive)
  (org-roam-datelies--capture (current-time) (org-roam-datelies--create-node 'ever (current-time)) t))

;;; find relative

(defun org-roam-datelies-find-previous ()
  "Goto the previous datelies note in the current time-period."
  (interactive)
  (let ((time-period (ordlies--get-node-time-period)) time)
    (pcase time-period
      ('day (setq time (ordlies--time-plus-days (ordlies--time-in-node-time-period) -1)))
      ('week (setq time (ordlies--time-plus-weeks (ordlies--time-in-node-time-period) -1)))
      ('month (setq time (time-plus-months (ordlies--time-in-node-time-period) -1)))
      ('quarter (setq time (ordlies--time-plus-quarter (ordlies--time-in-node-time-period) -1)))
      ('year (setq time (ordlies--time-plus-years (ordlies--time-in-node-time-period) -1))))    
    (org-roam-datelies--capture time (org-roam-datelies--create-node time-period time) t)))

(defun org-roam-datelies-find-forward ()
  "Find the previous datelies note in the current time-period"
  (interactive)
  (let ((time-period (ordlies--get-node-time-period)) time)
    (pcase time-period
      ('day (setq time (ordlies--time-plus-days (ordlies--time-in-node-time-period) 1)))
      ('week (setq time (ordlies--time-plus-weeks (ordlies--time-in-node-time-period) 1)))
      ('month (setq time (time-plus-months (ordlies--time-in-node-time-period) 1)))
      ('quarter (setq time (ordlies--time-plus-quarter (ordlies--time-in-node-time-period) 1)))
      ('year (setq time (ordlies--time-plus-years (ordlies--time-in-node-time-period) 1))))
    (org-roam-datelies--capture time (org-roam-datelies--create-node time-period time) t)))

(defun org-roam-datelies-find-up ()
  "if in weekly file, goes to the monthly file for the month
containing the first day of the file's week; analogous if in
monthly or quarterly file."
  (interactive)
  (let* ((time-period (ordlies--get-node-time-period))
         (time (ordlies--time-in-node-time-period)) node)
    (pcase time-period 
      ('day (setq node (org-roam-datelies--create-node 'week time)))
      ('week (setq node (org-roam-datelies--create-node 'month time)))
      ('month (setq node (org-roam-datelies--create-node 'quarter time)))
      ('quarter (setq node (org-roam-datelies--create-node 'year time)))
      ('year (setq node (org-roam-datelies--create-node 'ever time))))
    (org-roam-datelies--capture time node t)))

(defun org-roam-datelies-find-down-first ()
  "If in weekly file, goes to the daily file for the first day of
   the week; if in monthly file, go to first week of the month,
   etc."
  (interactive)
  (let ((time-period (ordlies--get-node-time-period))
        (start-time (car (ordlies--node-start-and-end-times)))
        new-time-period)
    (pcase time-period
      ('week (setq new-time-period 'day))
      ('month (setq new-time-period 'week))
      ('quarter (setq new-time-period 'month))
      ('year (setq new-time-period 'quarter)))
    (org-roam-datelies--capture start-time (org-roam-datelies--create-node new-time-period start-time) t)))

(defun org-roam-datelies-find-down-last ()
  "If in weekly file, goes to the daily file for the last day of
   the week; if in monthly file, go to last week of the month,
   etc."
  (interactive)
  (let ((time-period (ordlies--get-node-time-period))
        (end-time (cadr (ordlies--node-start-and-end-times)))
        new-time-period)
    (pcase time-period
      ('week (setq new-time-period 'day))
      ('month (setq new-time-period 'week))
      ('quarter (setq new-time-period 'month))
      ('year (setq new-time-period 'quarter)))
    (org-roam-datelies--capture end-time (org-roam-datelies--create-node new-time-period end-time) t)))

;;; agenda and related functions

(defun ordlies--files-under (time-period time-data) "The form of TIME-DATA
depends on TIME-PERIOD, e.g., if time-period is \='week, then
time-data should have the form (WEEK YEAR). Will return list of
items of the form (TIME-PERIOD FILE-NAME)." 
       (let (tag-vec return-list)
         (pcase time-period
           ('day (setq tag-vec ["orl-day"]))
           ('week (setq tag-vec ["orl-day" "orl-week"]))
           ('month (setq tag-vec ["orl-day" "orl-week" "orl-month"]))
           ('quarter (setq tag-vec ["orl-day" "orl-week" "orl-month" "orl-quarter"]))
           ('year (setq tag-vec ["orl-day" "orl-week" "orl-month" "orl-quarter" "orl-year"]))
           ('ever (setq tag-vec ["orl-day" "orl-week" "orl-month" "orl-quarter" "orl-year" "orl-ever"])))
         (setq return-list
               (cl-loop for (file tag) in (org-roam-db-query [:select [nodes:file tag]
                                                                      :from tags
                                                                      :inner :join nodes
                                                                      :on (= tags:node-id nodes:id)
                                                                      :where (in tag $v1)] tag-vec)
                        collect (list file (intern (substring tag 4))
                                      (ordlies--time-data-from-file-name
                                       (intern (substring tag 4))
                                       file))))
         (cl-loop for (file time-period-to-check time-data-to-check) in return-list
                  when (ordlies--time-period-under-time-period-p time-period time-data time-period-to-check time-data-to-check)
                  collect file)))

(defun org-roam-datelies-agenda (&optional full-filename)
  "Shows agenda for all of the files under the current
  org-roam-datelies file (e.g., if in a monthly file, this will call
  org-agenda will all of the weekly and daily files belonging to
  that month"
  (interactive)
  (unless full-filename (setq full-filename (buffer-file-name)))
  (let (org-agenda-files)
    (save-window-excursion
      (find-file full-filename)    
      (setq org-agenda-files (ordlies--files-under
                              (ordlies--get-node-time-period)
                              (ordlies--time-data-from-file-name
                               (ordlies--get-node-time-period) full-filename))))
    (org-agenda)))

(defun org-roam-datelies-time-worked (&optional params full-filename)
  "Find the total time worked in all of the files under the current
buffer, or full-filename if provided."
  (interactive)  
  (unless full-filename (setq full-filename (buffer-file-name)))
  (setq params (org-combine-plists org-clocktable-defaults params))
  (let* ((files
         (save-window-excursion
           (find-file full-filename)    
           (ordlies--files-under
                        (ordlies--get-node-time-period)
                        (ordlies--time-data-from-file-name
                         (ordlies--get-node-time-period) full-filename))))
         (tables
	  (if (consp files)
	      (mapcar (lambda (file)
			(with-current-buffer (find-buffer-visiting file)
			  (save-excursion
			    (save-restriction
			      (org-clock-get-table-data file params)))))
		      files)))
         (worked-minutes 0))
    (pcase-dolist (`(,_ ,file-time ,_) tables)
      (setq worked-minutes (+ worked-minutes file-time)))
    (message "Worked %s hours" (org-duration-from-minutes
                                worked-minutes 'h:mm))))

(defun org-roam-datelies-clock-whole-day ()
  "When called on an entry in an orl-day buffer, adds time clocked
from 9:00 until 16:36."
  (interactive)
  (let* ((day-string (file-name-base (buffer-file-name (buffer-base-buffer))))
        (start-time
         (org-time-string-to-time (concat day-string " 09:00")))
        (end-time
         (org-time-string-to-time (concat day-string " 16:36"))))
    (org-clock-in nil start-time)
    (org-clock-out nil nil end-time)))

;;; refile

(defun ordlies--refile-get-file-name (direction)
  "Assumes current buffer is an org-roam-datelies buffer. DIRECTION is a character. Returns
  the file name of the org-roam-datelies in that direction from current one."
  (let* ((command-suffix
          (pcase direction
            (?j "previous")
            (?k "down-first")
            (?l "forward")
            (?i "up")
            (?d "this-day")
            (?w "this-week")
            (?m "this-month")
            (?y "this-year")
            (?e "ever")))
         (command-name (concat "org-roam-datelies-find-" command-suffix))         
         (target-file
          (save-window-excursion
            (save-excursion
              (funcall (intern command-name))
              (buffer-file-name)))))
    target-file))


(defun ordlies--make-subtree-and-get-refile-pos (target-file heading-path-list)
  "Checks target-file to see if HEADING-PATH-LIST exists, creating subtrees as nescessary,
  and returns the point in the buffer right after the end of
  HEADING-PATH-LIST (the beginning of the line after the heading of the
  last item in the list)."
  (with-current-buffer (find-file-noselect target-file)  ;; Open the target file
    ;; Create parent headings if they don't exist
    (let ((cur-level 0))
      (dolist (heading heading-path-list)
        (setq cur-level (1+ cur-level))
        (if-let ((existing-heading (org-find-exact-headline-in-buffer heading nil t)))
            (progn (goto-char existing-heading)
                   (forward-line 1))
          (goto-char (point-max))  ;; Move to end of buffer
          (org-insert-heading nil nil cur-level)     ;; Insert new heading
          (insert heading))
        (org-narrow-to-subtree)))  ;; Narrow to the newly created heading
    (widen)  ;; Ensure we are at the top level
    (point)))

(defcustom org-roam-datelies-refile-keep-subtree-parents t
  "Whether refile should keep an entry at the same position in its subtree, creating it's
  parents if necessary."
  :group 'org-roam-datelies
  :type 'boolean)

(defun org-roam-datelies-refile (copy-p direction)
  "DIRECTION should be a character and either j for previous, l for
forward, i for up, k for down, d for today, w for this week, m for
  this month, y for this year, or e for ever. If called with a prefix
argument, then copy the entry to location."
  (interactive "P\ncChoose where to refile.")  
  (let* ((target-file (ordlies--refile-get-file-name direction))         
         (refile-pos
          (if org-roam-datelies-refile-keep-subtree-parents
              (ordlies--make-subtree-and-get-refile-pos target-file (org-get-outline-path))
            nil))
         (org-refile-targets `((,target-file :maxlevel . 9)))
         org-refile-keep)
    (if copy-p (setq org-refile-keep t))
    (org-refile nil nil `(nil ,target-file nil ,refile-pos))))

;;; org-roam-datelies-map (keymap)
(define-prefix-command 'org-roam-datelies-map)
(define-prefix-command 'orl-choose-date-map)
(define-prefix-command 'orl-down-map)

(define-key org-roam-datelies-map (kbd "j") #'org-roam-datelies-find-previous)
(define-key org-roam-datelies-map (kbd "l") #'org-roam-datelies-find-forward)
(define-key org-roam-datelies-map (kbd "i") #'org-roam-datelies-find-up)
(define-key org-roam-datelies-map (kbd "d") #'org-roam-datelies-find-this-day)
(define-key org-roam-datelies-map (kbd "w") #'org-roam-datelies-find-this-week)
(define-key org-roam-datelies-map (kbd "m") #'org-roam-datelies-find-this-month)
(define-key org-roam-datelies-map (kbd "q") #'org-roam-datelies-find-this-quarter)
(define-key org-roam-datelies-map (kbd "y") #'org-roam-datelies-find-this-year)
(define-key org-roam-datelies-map (kbd "e") #'org-roam-datelies-find-ever)
(define-key org-roam-datelies-map (kbd "u") (lambda () (interactive)
                                            (jb/up-heading)
                                            (forward-char)
                                            (backward-char)))
(define-key org-roam-datelies-map (kbd "c") #'orl-choose-date-map)
(define-key org-roam-datelies-map (kbd "r") #'org-roam-datelies-refile)
(define-key org-roam-datelies-map (kbd "k") #'orl-down-map)
(keymap-set org-roam-datelies-map (kbd "t") 'org-roam-datelies-time-worked)

(define-key org-roam-datelies-map (kbd "M-j") #'org-roam-datelies-find-previous)
(define-key org-roam-datelies-map (kbd "M-l") #'org-roam-datelies-find-forward)
(define-key org-roam-datelies-map (kbd "M-i") #'org-roam-datelies-find-up)
(define-key org-roam-datelies-map (kbd "M-d") #'org-roam-datelies-find-this-day)
(define-key org-roam-datelies-map (kbd "M-w") #'org-roam-datelies-find-this-week)
(define-key org-roam-datelies-map (kbd "M-m") #'org-roam-datelies-find-this-month)
(define-key org-roam-datelies-map (kbd "M-q") #'org-roam-datelies-find-this-quarter)
(define-key org-roam-datelies-map (kbd "M-y") #'org-roam-datelies-find-this-year)
(define-key org-roam-datelies-map (kbd "M-e") #'org-roam-datelies-find-ever)
(define-key org-roam-datelies-map (kbd "M-u") (lambda () (interactive)
                                            (jb/up-heading)
                                            (forward-char)
                                            (backward-char)))
(define-key org-roam-datelies-map (kbd "M-c") #'orl-choose-date-map)
(define-key org-roam-datelies-map (kbd "M-r") #'org-roam-datelies-refile)
(define-key org-roam-datelies-map (kbd "M-k") #'orl-down-map)


(define-key orl-choose-date-map (kbd "d") #'org-roam-datelies-find-date-for-day)
(define-key orl-choose-date-map (kbd "w") #'org-roam-datelies-find-date-for-week)
(define-key orl-choose-date-map (kbd "m") #'org-roam-datelies-find-date-for-month)        
(define-key orl-choose-date-map (kbd "q") #'org-roam-datelies-find-date-for-quarter)
(define-key orl-choose-date-map (kbd "y") #'org-roam-datelies-find-date-for-year)

(define-key orl-down-map (kbd "j") #'org-roam-datelies-find-down-first)
(define-key orl-down-map (kbd "l") #'org-roam-datelies-find-down-last)


(dolist (command '(org-roam-datelies-find-this-day
                   org-roam-datelies-find-this-week
                   org-roam-datelies-find-this-month
                   org-roam-datelies-find-this-quarter
                   org-roam-datelies-find-this-year
                   org-roam-datelies-find-ever
                   org-roam-datelies-find-previous
                   org-roam-datelies-find-forward
                   org-roam-datelies-find-up
                   org-roam-datelies-find-down-last
                   org-roam-datelies-find-down-first))
  (put command 'repeat-map 'org-roam-datelies-map))

(provide 'org-roam-datelies)

