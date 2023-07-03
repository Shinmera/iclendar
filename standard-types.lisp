(in-package #:org.shirakumo.iclendar)

(define-list-type address-list address)
(define-list-type second-list (integer 0 60))
(define-list-type minute-list (integer 0 59))
(define-list-type hour-list (integer 0 23))
(define-list-type week-day-list week-day-num)
(define-list-type month-day-list (integer 1 31))
(define-list-type year-day-list (integer 1 366))
(define-list-type week-list (integer 1 53))
(define-list-type month-list (integer 1 12))

(deftype attachment-value ()
  '(or uri pathname (vector (unsigned-byte 8))))

(deftype address ()
  'uri)

(deftype week-day ()
  '(member :sunday :monday :tuesday :wednesday :thursday :friday :saturday))

(defstruct (week-day-num (:constructor make-weekday-num (week-day &optional week)))
  (week NIL :type (or null (integer -53 53)))
  (week-day NIL :type week-day))

(define-print-object week-day-num NIL "~@[~d ~]~s"
  week-day-num-week week-day-num-week-day)

(defstruct (date)
  (year (c-year) :type (integer 0))
  (month (c-month) :type (integer 1 12))
  (date (c-date) :type (integer 1 31)))

(define-print-object date NIL "~d.~2,'0d.~2,'0d"
  date-year date-month date-date)

(defstruct (date-time (:include date))
  (hour (c-hour) :type (integer 0 23))
  (minute (c-minute) :type (integer 0 59))
  (second (c-second) :type (integer 0 60))
  (utc-p T :type boolean))

(define-print-object date-time NIL "~d.~2,'0d.~2,'0d ~d:~2,'0d:~2,'0d~@[ UTC~]"
  date-time-year date-time-month date-time-date date-time-hour date-time-minute date-time-second
  date-time-utc-p)

(defstruct (time-span)
  (week NIL :type (or null (integer 0)))
  (hour NIL :type (or null (integer 0)))
  (minute NIL :type (or null (integer 0)))
  (second NIL :type (or null (integer 0)))
  (day NIL :type (or null (integer 0)))
  (inc-p T :type boolean))

(define-print-object time-span NIL "~:[-~;+~]~@[ ~dw~]~@[ ~dd~]~@[ ~dh~]~@[ ~dm~]~@[ ~ds~]"
  time-span-inc-p time-span-week time-span-day time-span-hour time-span-minute time-span-second)

(defstruct (period (:constructor make-period (start limit)))
  (start NIL :type date-time)
  (limit NIL :type (or time-span date-time)))

(define-print-object period NIL "~a - ~a" period-start period-limit)

(defstruct (recurrence (:constructor make-recurrence (frequency &key end-date count interval by-seconds by-minutes by-hours by-days by-month-days by-year-days by-weeks by-months by-set-pos week-start)))
  (frequency NIL :type (member :secondly :minutely :hourly :daily :weekly :monthly :yearly))
  (end-date NIL :type (or null date date-time))
  (count NIL :type (or null (integer 0)))
  (interval NIL :type (or null (integer 0)))
  (by-seconds NIL :type second-list)
  (by-minutes NIL :type minute-list)
  (by-hours NIL :type hour-list)
  (by-days NIL :type week-day-list)
  (by-month-days NIL :type month-day-list)
  (by-year-days NIL :type year-day-list)
  (by-weeks NIL :type week-list)
  (by-months NIL :type month-list)
  (by-set-pos NIL :type year-day-list)
  (week-start NIL :type (or null week-day)))

(define-print-object recurrence NIL "~a~@[ - ~a~]~@[ x~d~]~@[ in ~d~]"
  recurrence-frequency recurrence-end-date recurrence-count recurrence-interval)

(deftype uri ()
  'string)

(defstruct (utc-offset)
  (hour 0 :type (integer 0 23))
  (minute 0 :type (integer 0 59))
  (second 0 :type (integer 0 60))
  (inc-p T :type boolean))

(define-print-object utc-offset NIL "~:[-~;+~]~d:~2,'0d:~2,'0d"
  utc-offset-inc-p utc-offset-hour utc-offset-minute utc-offset-second)

(defstruct (geo (:constructor make-geo (lat lng)))
  (lat 0.0 :type float)
  (lng 0.0 :type float))

(define-print-object geo NIL "~f,~f"
  geo-lat geo-lng)

(deftype text ()
  'string)
