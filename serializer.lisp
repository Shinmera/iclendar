#|
 This file is a part of iclendar
 (c) 2018 Shirakumo http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package #:org.shirakumo.iclendar)

(defgeneric serialize-object (object stream))

(defun s (stream object &rest args)
  (declare (ignore args))
  (serialize-object object stream))

(defun serialize (object &optional (output T) &key (if-exists :error))
  (etypecase output
    (stream
     (serialize-object object (make-icalendar-stream output)))
    ((eql T)
     (serialize-object object (make-icalendar-stream *standard-output*)))
    ((or pathname string)
     (with-open-file (stream output :direction :output
                                    :if-exists if-exists)
       (serialize-object object (make-icalendar-stream stream))))
    ((eql NIL)
     (with-output-to-string (stream)
       (serialize-object object (make-icalendar-stream stream))))))

(defmethod serialize-object ((symbol symbol) stream)
  (case symbol
    ((T) (format stream "TRUE"))
    ((NIL) (format stream "FALSE"))
    (T (format stream "~:@(~a~)" (symbol-name symbol)))))

(defmethod serialize-object ((string string) stream)
  (write-string string stream))

(defmethod serialize-object ((object week-day-num) stream)
  (format stream "~@[~d~]~a" (week-day-num-week object)
          (ecase (week-day-num-week-day object)
            (:monday "MO") (:tuesday "TU") (:wednesday "WE")
            (:thursday "TH") (:friday "FR") (:saturday "SA") (:sunday "SU"))))

(defmethod serialize-object ((object date) stream)
  (format stream "~4,'0d~2,'0d~2,'0d"
          (date-year object) (date-month object) (date-date object)))

(defmethod serialize-object ((object date-time) stream)
  (format stream "~4,'0d~2,'0d~2,'0dT~2,'0d~2,'0d~2,'0d~@[Z~]"
          (date-time-year object) (date-time-month object) (date-time-date object)
          (date-time-hour object) (date-time-minute object) (date-time-second object)
          (date-time-utc-p object)))

(defmethod serialize-object ((object time-span) stream)
  (format stream "~:[-~;+~]P~@[~dD~]~:[~;T~@[~dH~]~@[~dM~]~@[~dS~]~]~@[~dW~]"
          (time-span-inc-p object) (time-span-day object)
          (or (time-span-hour object) (time-span-minute object) (time-span-second object))
          (time-span-hour object) (time-span-minute object) (time-span-second object)
          (time-span-week object)))

(defmethod serialize-object ((object period) stream)
  (format stream "~/iclendar::s//~/iclendar::s/"
          (period-start object) (period-limit object)))

(defmethod serialize-object ((object recurrence) stream)
  (format stream "FREQ=~a"
          (ecase (recurrence-frequency object)
            (:secondly "SECONDLY") (:minutely "MINUTELY") (:hourly "HOURLY")
            (:daily "DAILY") (:weekly "WEEKLY") (:monthly "MONTHLY") (:yearly "YEARLY")))
  (format stream "~@[;UNTIL=~/iclendar::s/~]" (recurrence-end-date object))
  (format stream "~@[;COUNT=~d~]" (recurrence-count object))
  (format stream "~@[;INTERVAL=~d~]" (recurrence-interval object))
  (format stream "~@[;BYSECOND=~{~a~^,~}~]" (recurrence-by-seconds object))
  (format stream "~@[;BYMINUTE=~{~a~^,~}~]" (recurrence-by-minutes object))
  (format stream "~@[;BYHOUR=~{~a~^,~}~]" (recurrence-by-hours object))
  (format stream "~@[;BYDAY=~{~/iclendar::s/~^,~}~]" (recurrence-by-days object))
  (format stream "~@[;BYMONTHDAY=~{~a~^,~}~]" (recurrence-by-month-days object))
  (format stream "~@[;BYYEARDAY=~{~a~^,~}~]" (recurrence-by-year-days object))
  (format stream "~@[;BYWEEKNO=~{~a~^,~}~]" (recurrence-by-weeks object))
  (format stream "~@[;BYMONTH=~{~a~^,~}~]" (recurrence-by-months object))
  (format stream "~@[;BYSETPOS=~{~a~^,~}~]" (recurrence-by-set-pos object))
  (format stream "~@[;WKST=~a~]" (ecase (recurrence-week-start object)
                                   (:monday "MO") (:tuesday "TU") (:wednesday "WE")
                                   (:thursday "TH") (:friday "FR") (:saturday "SA") (:sunday "SU"))))

(defmethod serialize-object ((object utc-offset) stream)
  (format stream "~:[-~;+~]~2,'0d~2,'0d~@[~2,'0d~]"
          (utc-offset-inc-p object) (utc-offset-hour object)
          (utc-offset-minute object) (when (/= 0 (utc-offset-second object))
                                       (utc-offset-second object))))

(defmethod serialize-object ((object geo) stream)
  (format stream "~f;~f" (geo-lat object) (geo-lng object)))

(defmethod serialize-object ((property property) stream)
  (format stream "~a~{;~a=\"~/iclendar::s/\"~}:~/iclendar::s/"
          (identifier property) (parameters property) (value property)))

(defmethod serialize-object ((property attachment) stream)
  (format stream "~a~{;~a=\"~/iclendar::s/\"~}"
          (identifier property) (parameters property))
  (etypecase (value property)
    ((or pathname (vector (unsigned-byte 8)))
     ;; Force parameters
     (unless (slot-boundp property 'encoding)
       (format stream ";ENCODING=BASE64"))
     (unless (slot-boundp property 'value-type)
       (format stream ";VALUE=BINARY"))
     (format stream ":")
     (etypecase (value property)
       (pathname
        ;; CL-BASE64 can't do stream-to-stream, so we need to load into memory first.
        (with-open-file (stream (value property) :element-type '(unsigned-byte 8))
          (loop with buffer = (make-array 4096 :element-type '(unsigned-byte 8) :fill-pointer 4096 :adjustable T)
                for start = 0 then read
                for read = (read-sequence buffer stream :start start)
                while (= read (length buffer))
                do (adjust-array buffer (+ (length buffer) 4096))
                finally (setf (fill-pointer buffer) read)
                        (cl-base64:usb8-array-to-base64-stream buffer stream))))
       ((vector (unsigned-byte 8))
        (cl-base64:usb8-array-to-base64-stream (value property) stream))))
    (string
     (format stream ":~a" (value property))))
  (terpri stream))

(defmethod serialize-object :around ((component component) stream)
  (format stream "BEGIN:~a" (identifier component))
  (terpri stream)
  (call-next-method)
  (format stream "END:~a" (identifier component))
  (terpri stream))

(defmethod serialize-object ((component component) stream)
  (dolist (property (properties component))
    (serialize-object property stream)))

(defmethod serialize-object :after ((component component-container) stream)
  (dolist (component (components component))
    (serialize-object component stream)))
