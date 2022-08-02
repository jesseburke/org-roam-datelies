;;; org-roam-lies.el --- day, week, ...lies-notes for Org-roam -*- coding: utf-8; lexical-binding: t; -*-

;; Author: Jesse Burke <jtb445@gmail.com>
;;
;; URL: https://github.com/org-roam/org-roam
;; Keywords: org-mode, roam, convenience
;; Version: 2.0.0
;; Package-Requires: ((emacs "26.1") (dash "2.13") (f "0.17.2") (org "9.4") (emacsql "3.0.0") (emacsql-sqlite "1.0.0") (magit-section "2.90.1"))

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
(require 'org-roam)
(require 'org-capture)
(require 'cal-iso)
(require 'f)

(defvar org-roam-directory)
(defvar org-roam-file-extensions)
(declare-function org-roam-file-p        "org-roam")

(defcustom org-roam-dailies-dir "daily/"
  "Path to daily-notes, relative to `org-roam-directory'."
  :group 'org-roam
  :type 'string)

(defcustom org-roam-weeklies-dir "weekly/"
  "Path to weekly-notes, relative to `org-roam-directory'."
  :group 'org-roam
  :type 'string)

(defcustom org-roam-monthlies-dir "monthly/"
  "Path to monthly-notes, relative to `org-roam-directory'."
  :group 'org-roam
  :type 'string)

(defcustom org-roam-quarterlies-dir "quarterly/"
  "Path to quarterly-notes, relative to `org-roam-directory'."
  :group 'org-roam
  :type 'string)

(defcustom org-roam-yearlies-dir "yearly/"
  "Path to yearly-notes, relative to `org-roam-directory'."
  :group 'org-roam
  :type 'string)

(defcustom org-roam-everlies-dir "ever/"
  "Path to everly-notes, relative to `org-roam-directory'."
  :group 'org-roam
  :type 'string)

;;; time functions

(defun time-in-time-period-p (time-to-check time-period time-data)
  (cl-destructuring-bind (start-time end-time)
      (time-period-start-and-end-times time-period time-data)
    (and (time-less-p start-time time-to-check) (time-less-p
                                                 time-to-check end-time))))

(defun day-start-and-end-times (day month year)
  (list (encode-time 1 0 0 day month year)
        (encode-time 59 59 11 day month year)))

(defun week-start-and-end-times (week year)
  (let ((start-time (iso-week-to-time year (- week 1) 0))
        (end-time (iso-week-to-time year week 6 23 59)))
    (list start-time end-time)))

(defun month-start-and-end-times (month year)
  (list (encode-time 0 0 0 1 month year)
        (encode-time 0 59 23 (calendar-last-day-of-month month year) month year)))

(defun quarter-start-and-end-times (quarter year)
  (cond ((eq quarter 1) (list (encode-time 0 0 0 1 1 year)  (encode-time 0 59 23 (calendar-last-day-of-month 3 year) 3 year)))
        ((eq quarter 2) (list (encode-time 0 0 0 1 4 year)  (encode-time 0 59 23 (calendar-last-day-of-month 6 year) 6 year)))
        ((eq quarter 3) (list (encode-time 0 0 0 1 7 year)  (encode-time 0 59 23 (calendar-last-day-of-month 9 year) 9 year)))
        ((eq quarter 4) (list (encode-time 0 0 0 1 10 year)
                              (encode-time 0 59 23 (calendar-last-day-of-month 10 year) 12
                                           year)))))

(defun year-start-and-end-times (year)
  (list (encode-time 0 0 0 1 1 year)  (encode-time 0 59 23 31 12 year)))

(defun time-period-start-and-end-times (time-period time-data)
  "Time-data is a list that depends on time-period, e.g., if time-period
is 'day, then time-data is a list of the form (DAY MONTH YEAR)."
  (pcase time-period
    ('day (apply #'day-start-and-end-times time-data))
    ('week (apply #'week-start-and-end-times time-data))
    ('month (apply #'month-start-and-end-times time-data))
    ('quarter (apply #'quarter-start-and-end-times time-data))
    ('year (apply #'year-start-and-end-times time-data))))

(defun time-period-under-time-period-p (time-period time-data
                                                    time-period-tc
                                                    time-data-tc)
  "Checks whether the time span determined by time-period and
time-data contains the time span determined by time-period-tc (to
check) and time-data-tc. Time-data is a list that depends on
time-period, e.g., if time-period is 'day, then time-data is a
list of the form (DAY MONTH YEAR)."
  (cl-destructuring-bind (start end) (time-period-start-and-end-times
                                      time-period-tc time-data-tc)
    (and (time-in-time-period-p start time-period time-data)
         (time-in-time-period-p end time-period time-data))))

(defun time-plus-days (time no-days)
  (interactive)
  (time-add time (* 86400 no-days)))

(defun iso-week-to-time (year week day &optional hour min)
  (unless hour (setq hour 0))
  (unless min (setq min 0))
  (pcase-let ((`(,m ,d ,y)
               (calendar-gregorian-from-absolute
                (calendar-iso-to-absolute (list week day year)))))
    (encode-time 0 min hour d m y)))

(defun time-plus-weeks (time no-weeks)
  (interactive)
  (time-add time (* 86400 7 no-weeks)))

(defun time-plus-months (time no-months)
  "no-months should be between -11 and 11"
  (cl-destructuring-bind (sec min hour day month year _ _ _) (decode-time time)
    (pcase (+ month no-months)
      ((pred (lambda (new-month) (<= new-month 12)))
       (encode-time sec min hour day (+ month no-months) year))
      ((pred (lambda (new-month) (> new-month 12)))
       (encode-time sec min hour day (- (+ month no-months) 12) (+ year 1)))
      ((pred (lambda (new-month) (< new-month 1)))
       (encode-time sec min hour day (- 12 (+ month no-months)) (- year 1))))))

(defun time-plus-quarter (time)
  "if time represents the date, e.g., 15 june, then this will return the time of 15 sep."
  (pcase-let ((`(,sec ,min ,hour ,day ,month ,year)
               (decode-time time)))
    (if (< month 10) (encode-time sec min hour day (+ month 3) year)
      (encode-time sec min hour day (- month 9) (+ year 1)))))   

(defun time-minus-quarter (time)
  "If time represents the date, e.g., 15 june, then this will return the time of 15 march."
  (pcase-let ((`(,sec ,min ,hour ,day ,month ,year)
               (decode-time time)))
    (if (> month 3) (encode-time sec min hour day (- month 3) year)
      (encode-time sec min hour day (+ 9 month) (- year 1)))))

(defun time-plus-years (time no-years)
  (pcase-let ((`(,sec ,min ,hour ,day ,month ,year)
               (decode-time time)))
    (encode-time sec min hour day month (+ year no-years))))

(defun time-to-day-week-month-quarter-year (time)
  (let* ((str (format-time-string "%Y%q%m%U%d" time))
         (year (string-to-number (substring str 0 4)))
         (quarter (string-to-number (substring str 4 5)))
         (month (string-to-number (substring str 5 7)))
         (week (string-to-number (substring str 7 9)))
         (day (string-to-number (substring str 9 11))))
    (list day week month quarter year)))

;;; org-roam-lies-node definition

(cl-defstruct (org-roam-lies-node (:include org-roam-node) (:predicate nil) (:constructor org-roam-lies-node-create))
  time-period time directory template)

(defvar orl-day-tag "orl-day")
(defvar orl-week-tag "orl-week")
(defvar orl-month-tag "orl-month")
(defvar orl-quarter-tag "orl-quarter")
(defvar orl-year-tag "orl-year")
(defvar orl-ever-tag "orl-ever")

(defun create-org-roam-lies-node (time-period time)
  (let* (start-time end-time directory header template)
    (cl-destructuring-bind (day week month quarter year) (time-to-day-week-month-quarter-year time)
      (pcase time-period
        ('day
         (setq directory org-roam-dailies-dir)            
         (setq template
               (make-template (concat org-roam-dailies-dir "%<%Y-%m-%d>.org")
                              (concat "#+title:%<%Y-%m-%d>\n#+filetags: :"
                                      orl-day-tag ":\n\n" (format-time-string "%A, %F" time) "\n\n"))))
        ('week
         (setq directory org-roam-weeklies-dir)
         (cl-destructuring-bind (st et) (week-start-and-end-times week year)
           (setq start-time st end-time et))
         (setq template
               (make-template (concat org-roam-weeklies-dir "%<%Y-W%U>.org")
                              (concat "#+title: %<%Y week %U>\n#+filetags: :" orl-week-tag ":\n\n"
                                      (format-time-string "%A, %F"
                                                          start-time)
                                      " -- "(format-time-string "%A, %F" end-time) "\n\n"))))
        ('month
         (setq directory org-roam-monthlies-dir)
         (cl-destructuring-bind (st et) (month-start-and-end-times month year)
           (setq start-time st end-time et))
         (setq template
               (make-template (concat org-roam-monthlies-dir "%<%Y-%m>.org")
                              (concat "#+title: %<%Y %B>\n#+filetags: :" orl-month-tag ":\n\n"
                                      (format-time-string "%A, %F"
                                                          start-time)
                                      " -- " (format-time-string "%A, %F" end-time) "\n\n"))))
        ('quarter
         (setq directory org-roam-quarterlies-dir)
         (cl-destructuring-bind (st et) (quarter-start-and-end-times quarter year)
           (setq start-time st end-time et))
         (setq template
               (make-template (concat org-roam-quarterlies-dir "%<%Y-%q>.org")
                              (concat "#+title: %<%Y quarter %q>\n#+filetags: :" orl-quarter-tag ":\n\n"
                                      (format-time-string "%A, %F"
                                                          start-time)
                                      " -- " (format-time-string "%A, %F" end-time) "\n\n"))))
        ('year
         (setq directory org-roam-yearlies-dir)
         (cl-destructuring-bind (st et) (year-start-and-end-times year)
           (setq start-time st end-time et))
         (setq template
               (make-template (concat org-roam-yearlies-dir "%<%Y>.org")
                              (concat "#+title: %<%Y>\n#+filetags: :" orl-year-tag ":\n\n"
                                      (format-time-string "%A, %F"
                                                          start-time)
                                      " -- " (format-time-string "%A, %F" end-time) "\n\n"))))
        ('ever
         (setq directory org-roam-everlies-dir)
         (cl-destructuring-bind (st et) (year-start-and-end-times year)
           (setq start-time st end-time et))
         (setq template
               (make-template (concat org-roam-everlies-dir "ever.org")
                              (concat "#+title: ever file\n#+filetags: :" orl-ever-tag ":\n\n"
                                      (format-time-string "%A, %F"
                                                          start-time)
                                      " -- " (format-time-string "%A, %F" end-time) "\n\n")))))
      (org-roam-lies-node-create :time-period time-period :time time
                                 :directory directory :template template))))

(defun make-template (file-str head-str)
  `("d" "default" entry
    "* %?"
    :if-new (file+head ,file-str ,head-str)))

;;; util functions

(defun org-roam-lies-get-node-time-period (&optional node)
  "Return day, week, month, quarter, or ever if node is an Org-roam-lies node, nil otherwise.
If node isn't specified, use node at point."
  (unless node (setq node (org-roam-node-at-point)))
  (let ((tag-list (org-roam-node-tags node)) return-str)
    (dolist (tag tag-list)
      (if (string-prefix-p "orl-" tag)
          (setq return-str (substring tag 4 nil))))
    (if return-str
        (intern return-str)
      (org-roam-lies-type-from-file-name (org-roam-node-file node)))))

(defun org-roam-lies-type-from-file-name (&optional file)
  "Return day, week, month, quarter, or ever if FILE is an Org-roam-lies note, nil otherwise.
If FILE is not specified, use the current buffer's file-path."
  (interactive)
  (let ((dd (expand-file-name org-roam-dailies-dir org-roam-directory))
        (wd (expand-file-name org-roam-weeklies-dir org-roam-directory))
        (md (expand-file-name org-roam-monthlies-dir org-roam-directory))
        (qd (expand-file-name org-roam-quarterlies-dir org-roam-directory))
        (yd (expand-file-name org-roam-yearlies-dir org-roam-directory))
        (ed (expand-file-name org-roam-everlies-dir org-roam-directory)))
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

(defun orl--time-data-from-file-name (time-period filename)
  "Assumes that dailies are names: YYYY-MM-DD, weeklies YYYY-wWW,
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

(defun node-start-and-end-times (&optional node)
  "Returns a list of the form (start-time end-time)."
  (unless node (setq node (org-roam-node-at-point)))
  (let* ((timeperiod (org-roam-lies-get-node-time-period))
         (filename (org-roam-node-file node))
         (timedata (orl--time-data-from-file-name timeperiod filename)))
    (time-period-start-and-end-times timeperiod timedata)))

(defun time-in-time-period (&optional node)
  "returns a single time in the time period of the current file"
  (car (node-start-and-end-times)))

;;; capture and visit functions

(defun org-roam-lies--capture (time node &optional goto)
  "create a weekly, monthly, etc for time, creating it if neccessary"
  (org-roam-capture- :goto (when goto '(4))
                     :node node
                     :templates (list (org-roam-lies-node-template node))
                     :props (list :override-default-time time)))

(add-hook 'org-roam-capture-preface-hook
          #'org-roam-lies--override-capture-time-h)

(defun org-roam-lies--override-capture-time-h ()
  "Override the `:default-time' with the time from `:override-default-time'."
  (prog1 nil
    (when (org-roam-capture--get :override-default-time)
      (org-capture-put :default-time (org-roam-capture--get :override-default-time)))))

(defun org-roam-lies-find-this-day ()
  "Find the daily-note for today, creating it if necessary."
  (interactive)
  (org-roam-lies--capture (current-time)                     
                          (create-org-roam-lies-node 'day (current-time)) t))

(defun org-roam-lies-find-date-for-day (&optional prefer-future)
  "Find the daily-note for a date using the calendar."
  (interactive "P")
  (let ((time (let ((org-read-date-prefer-future prefer-future))
                (org-read-date t t nil "Find daily-note: "))))
    (org-roam-lies--capture time (create-org-roam-lies-node 'day time) t)))

(defun org-roam-lies-find-this-week ()
  "Find the weekly-note for this week, creating it if necessary."
  (interactive)
  (org-roam-lies--capture (current-time)
                          (create-org-roam-lies-node 'week (current-time)) t))

(defun org-roam-lies-find-date-for-week (&optional prefer-future)
  "Find the weekly-note for a date using the calendar."
  (interactive "P")
  (let ((time (let ((org-read-date-prefer-future prefer-future))
                (org-read-date t t nil "Find weekly-note: "))))
    (org-roam-lies--capture time (create-org-roam-lies-node 'week time) t)))

(defun org-roam-lies-find-this-month ()
  "Find the monthly-note for this month, creating it if necessary."
  (interactive)
  (org-roam-lies--capture (current-time)
                          (create-org-roam-lies-node 'month (current-time)) t))

(defun org-roam-lies-find-date-for-month (&optional prefer-future)
  "Find the monthly-note for a date using the calendar."
  (interactive "P")
  (let ((time (let ((org-read-date-prefer-future prefer-future))
                (org-read-date t t nil "Find monthly-note: "))))
    (org-roam-lies--capture time(create-org-roam-lies-node 'month time) t)))

(defun org-roam-lies-find-this-quarter ()
  "Find the quarterly-note for this quarter, creating it if necessary."
  (interactive)
  (org-roam-lies--capture (current-time)
                          (create-org-roam-lies-node 'quarter (current-time)) t))

(defun org-roam-lies-find-date-for-quarter (&optional prefer-future)
  "Find the quarterly-note for a date using the calendar."
  (interactive "P")
  (let ((time (let ((org-read-date-prefer-future prefer-future))
                (org-read-date t t nil "Find quarterly-note: "))))
    (org-roam-lies--capture time                  
                            (create-org-roam-lies-node 'quarter time) t)))

(defun org-roam-lies-find-this-year ()
  "Find the yearly-note for this year, creating it if necessary."
  (interactive)
  (org-roam-lies--capture (current-time)
                          (create-org-roam-lies-node 'year (current-time)) t))

(defun org-roam-lies-find-date-for-year (&optional prefer-future)
  "Find the yearly-note for a date using the calendar."
  (interactive "P")
  (let ((time (let ((org-read-date-prefer-future prefer-future))
                (org-read-date t t nil "Find yearly-note: "))))
    (org-roam-lies--capture time (create-org-roam-lies-node 'year (current-time)) t)))

(defun org-roam-lies-find-ever ()
  "Find the everly-note, creating it if necessary."
  (interactive)
  (org-roam-lies--capture (current-time) (create-org-roam-lies-node 'ever (current-time)) t))

;;; move relative

(defun org-roam-lies-find-previous ()
  "Goto the previous lies note in the current time-period."
  (interactive)
  (let ((time-period (org-roam-lies-get-node-time-period)) time)
    (pcase time-period
      ('day (setq time (time-plus-days (time-in-time-period) -1)))
      ('week (setq time (time-plus-weeks (time-in-time-period) -1)))
      ('month (setq time (time-plus-months (time-in-time-period) -1)))
      ('quarter (setq time (time-minus-quarter (time-in-time-period))))
      ('year (setq time (time-plus-years (time-in-time-period) -1))))
    (org-roam-lies--capture time (create-org-roam-lies-node time-period time) t)))

(defun org-roam-lies-find-forward ()
  "Find the previous lies note in the current time-period"
  (interactive)
  (let ((time-period (org-roam-lies-get-node-time-period)) time)
    (pcase time-period
      ('day (setq time (time-plus-days (time-in-time-period) 1)))
      ('week (setq time (time-plus-weeks (time-in-time-period) 1)))
      ('month (setq time (time-plus-months (time-in-time-period) 1)))
      ('quarter (setq time (time-plus-quarter (time-in-time-period))))
      ('year (setq time (time-plus-years (time-in-time-period) 1))))
    (org-roam-lies--capture time (create-org-roam-lies-node time-period time) t)))

(defun org-roam-lies-find-up ()
  "if in weekly file, goes to the monthly file for the month
containing the first day of the file's week; analogous if in
monthly or quarterly file."
  (interactive)
  (let* ((time-period (org-roam-lies-get-node-time-period))
         (time (time-in-time-period)) node)
    (pcase time-period 
      ('day (setq node (create-org-roam-lies-node 'week time)))
      ('week (setq node (create-org-roam-lies-node 'month time)))
      ('month (setq node (create-org-roam-lies-node 'quarter time)))
      ('quarter (setq node (create-org-roam-lies-node 'year time)))
      ('year (setq node (create-org-roam-lies-node 'ever time))))
    (org-roam-lies--capture time node t)))

(defun org-roam-lies-find-down-first ()
  "If in weekly file, goes to the daily file for the first day of
   the week; if in monthly file, go to first week of the month,
   etc."
  (interactive)
  (let ((time-period (org-roam-lies-get-node-time-period))
        (start-time (car (node-start-and-end-times)))
        new-time-period)
    (pcase time-period
      ('week (setq new-time-period 'day))
      ('month (setq new-time-period 'week))
      ('quarter (setq new-time-period 'month))
      ('year (setq new-time-period 'quarter)))
    (org-roam-lies--capture start-time (create-org-roam-lies-node new-time-period start-time) t)))

(defun org-roam-lies-find-down-last ()
  "If in weekly file, goes to the daily file for the last day of
   the week; if in monthly file, go to last week of the month,
   etc."
  (interactive)
  (let ((time-period (org-roam-lies-get-node-time-period))
        (end-time (cadr (node-start-and-end-times)))
        new-time-period)
    (pcase time-period
      ('week (setq new-time-period 'day))
      ('month (setq new-time-period 'week))
      ('quarter (setq new-time-period 'month))
      ('year (setq new-time-period 'quarter)))
    (org-roam-lies--capture end-time (create-org-roam-lies-node new-time-period end-time) t)))

;;; agenda and related functions

(defun orl--files-under (time-period time-data) "The form of TIME-DATA
depends on TIME-PERIOD, e.g., if time-period is 'week, then time-data
should have the form (WEEK YEAR). Will return list of items of the form (TIME-PERIOD FILE-NAME)." 
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
                        collect (list file (intern (substring tag 4)) (orl--time-data-from-file-name (intern (substring tag 4)) file))))
         (cl-loop for (file tp-tc td-tc) in return-list
                  when (time-period-under-time-period-p time-period time-data tp-tc td-tc)
                  collect file)))

(defun org-roam-lies-agenda (&optional full-filename)
  "Shows agenda for all of the files under the current
  org-roam-lies file (e.g., if in a monthly file, this will call
  org-agenda will all of the weekly and daily files belonging to
  that month"
  (interactive)
  (unless full-filename (setq full-filename (buffer-file-name)))
  (let (org-agenda-files)
    (save-window-excursion
      (find-file full-filename)    
      (setq org-agenda-files (orl--files-under
                              (org-roam-lies-get-node-time-period)
                              (orl--time-data-from-file-name
                               (org-roam-lies-get-node-time-period) full-filename))))
    (org-agenda)))

(defun orl-move (copy-p direction)
  "DIRECTION should be a character and either j for previous, l for
forward, i for up, and k for down. If called with a prefix
argument, then copy the entry to location."
  (interactive "P\ncChoose where to refile.")  
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
         (command-name (concat "org-roam-lies-find-" command-suffix))
         (file-name
          (save-window-excursion
            (save-excursion
              (funcall (intern command-name))
              (buffer-file-name)))))
    (let (org-refile-keep)
      (if copy-p (setq org-refile-keep t))
      (org-refile nil nil (list nil file-name)))))

;;; org-roam-lies-map (keymap)
(define-prefix-command 'org-roam-lies-map)
(define-prefix-command 'orl-choose-date-map)
(define-prefix-command 'orl-down-map)

(define-key org-roam-lies-map (kbd "j") #'org-roam-lies-find-previous)
(define-key org-roam-lies-map (kbd "l") #'org-roam-lies-find-forward)
(define-key org-roam-lies-map (kbd "i") #'org-roam-lies-find-up)
(define-key org-roam-lies-map (kbd "d") #'org-roam-lies-find-this-day)
(define-key org-roam-lies-map (kbd "w") #'org-roam-lies-find-this-week)
(define-key org-roam-lies-map (kbd "m") #'org-roam-lies-find-this-month)
(define-key org-roam-lies-map (kbd "q") #'org-roam-lies-find-this-quarter)
(define-key org-roam-lies-map (kbd "y") #'org-roam-lies-find-this-year)
(define-key org-roam-lies-map (kbd "e") #'org-roam-lies-find-ever)
(define-key org-roam-lies-map (kbd "u") (lambda () (interactive)
                                            (jb/up-heading)
                                            (forward-char)
                                            (backward-char)))
(define-key org-roam-lies-map (kbd "c") #'orl-choose-date-map)

(define-key org-roam-lies-map (kbd "M-j") #'org-roam-lies-find-previous)
(define-key org-roam-lies-map (kbd "M-l") #'org-roam-lies-find-forward)
(define-key org-roam-lies-map (kbd "M-i") #'org-roam-lies-find-up)
(define-key org-roam-lies-map (kbd "M-d") #'org-roam-lies-find-this-day)
(define-key org-roam-lies-map (kbd "M-w") #'org-roam-lies-find-this-week)
(define-key org-roam-lies-map (kbd "M-m") #'org-roam-lies-find-this-month)
(define-key org-roam-lies-map (kbd "M-q") #'org-roam-lies-find-this-quarter)
(define-key org-roam-lies-map (kbd "M-y") #'org-roam-lies-find-this-year)
(define-key org-roam-lies-map (kbd "M-e") #'org-roam-lies-find-ever)
(define-key org-roam-lies-map (kbd "M-u") (lambda () (interactive)
                                            (jb/up-heading)
                                            (forward-char)
                                            (backward-char)))
(define-key org-roam-lies-map (kbd "M-c") #'orl-choose-date-map)
(define-key org-roam-lies-map (kbd "M-k") #'orl-down-map)


(define-key orl-choose-date-map (kbd "d") #'org-roam-lies-find-date-for-day)
(define-key orl-choose-date-map (kbd "w") #'org-roam-lies-find-date-for-week)
(define-key orl-choose-date-map (kbd "m") #'org-roam-lies-find-date-for-month)        
(define-key orl-choose-date-map (kbd "q") #'org-roam-lies-find-date-for-quarter)
(define-key orl-choose-date-map (kbd "y") #'org-roam-lies-find-date-for-year)

(define-key orl-down-map (kbd "j") #'org-roam-lies-find-down-first)
(define-key orl-down-map (kbd "l") #'org-roam-lies-find-down-last)


(dolist (command '(org-roam-lies-find-this-day
                   org-roam-lies-find-this-week
                   org-roam-lies-find-this-month
                   org-roam-lies-find-this-quarter
                   org-roam-lies-find-this-year
                   org-roam-lies-find-ever
                   org-roam-lies-find-previous
                   org-roam-lies-find-forward
                   org-roam-lies-find-up
                   org-roam-lies-find-down-last
                   org-roam-lies-find-down-first))
  (put command 'repeat-map 'org-roam-lies-map))

(provide 'org-roam-lies)
