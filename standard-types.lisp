#|
 This file is a part of iclendar
 (c) 2018 Shirakumo http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package #:org.shirakumo.iclendar)

(deftype date-time ())

(deftype date ())

(deftype uri ()
  'string)

(deftype attachment ()
  '(or uri pathname (vector (unsigned-byte 8))))

(deftype address ()
  'uri)

(defun address-list-p (list)
  (loop for entry in list
        always (typep entry 'address)))

(deftype address-list ()
  '(and address (satisfies address-list-p)))

(deftype recurrence ())

(deftype duration ())

(deftype duration ())

(deftype geo ())

(deftype period ())

(deftype utc-offset ())

(deftype text ()
  'string)

(deftype language ()
  'string)
