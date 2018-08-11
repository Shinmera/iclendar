#|
 This file is a part of iclendar
 (c) 2018 Shirakumo http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package #:org.shirakumo.iclendar)

(defmacro define-list-type (name inner)
  (let ((predicate (intern (let ((*print-case* (readtable-case *readtable*)))
                             (format NIL "~a-~a" name 'p)))))
    `(progn (defun ,predicate (list)
              (loop for entry in list
                    always (typep entry ',inner)))
            (deftype ,name ()
              '(and list (satisfies ,predicate))))))

(define-list-type address-list address)
(define-list-type second-list (integer 0 60))
(define-list-type minute-list (integer 0 59))
(define-list-type hour-list (integer 0 23))
(define-list-type week-day-list week-day-num)
(define-list-type month-day-list (integer 1 31))
(define-list-type year-day-list (integer 1 366))
(define-list-type week-list (integer 1 53))
(define-list-type month-list (integer 1 12))

(deftype attachment ()
  '(or uri pathname (vector (unsigned-byte 8))))

(deftype address ()
  'uri)

(deftype week-day ()
  '(member :sunday :monday :tuesday :wednesday :thursday :friday :saturday))

(defstruct (week-day-num (:constructor make-weekday-num (week-day &optional week)))
  (week NIL :type (or null (integer 1 53)))
  (week-day NIL :type week-day))

(defstruct (date)
  (year 1900 :type (integer 0))
  (month 1 :type (integer 1 12))
  (date 1 :type (integer 1 31)))

(defstruct (date-time (:include date))
  (hour 0 :type (integer 0 23))
  (minute 0 :type (integer 0 59))
  (second 0 :type (integer 0 60))
  (utc-p NIL :type boolean))

(defstruct (time-span)
  (week NIL :type (or null (integer 0)))
  (hour NIL :type (or null (integer 0)))
  (minute NIL :type (or null (integer 0)))
  (second NIL :type (or null (integer 0)))
  (day NIL :type (or null (integer 0)))
  (inc-p T :type boolean))

(defstruct (period (:constructor make-period (start limit)))
  (start NIL :type date-time)
  (limit NIL :type (or time-span date-time)))

(defstruct (recurrence (:constructor make-recurrency (frequency &key end-date count interval by-seconds by-minutes by-hours by-days by-month-days by-year-days by-weeks by-months by-set-pos week-start)))
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

(deftype uri ()
  'string)

(defstruct (utc-offset)
  (hour 0 :type (integer 0 23))
  (minute 0 :type (integer 0 59))
  (second 0 :type (integer 0 60))
  (inc-p T :type boolean))

(defstruct (geo (:constructor make-geo (lat lng)))
  (lat 0.0 :type float)
  (lng 0.0 :type float))

(deftype text ()
  'string)

(deftype language ()
  'string)
