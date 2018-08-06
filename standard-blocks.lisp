#|
 This file is a part of iclendar
 (c) 2018 Shirakumo http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package #:org.shirakumo.iclendar)

(defclass component-container ()
  ((components :initform NIL :initarg :components :reader components)))

(define-block calendar (component-container)
  (product scale transport-method version)
  ()
  (:identifier "VCALENDAR"))

(define-block component ()
  ()
  ((unknown-properties :initarg :unknown-properties :reader unknown-properties))
  (:default-initargs :unknown-properties ()))

(define-block calendar-component (component)
  (attendees comments request-status stamp start uid url)
  ())

(define-block date-component (calendar-component)
  (attachments categories classification contacts created exception-dates
              last-modification organizer recurrence recurrence-dates recurrence-rule
              related resources sequence-number status summary)
  ())

(define-block task-component (date-component)
  (description duration geographic-location location priority)
  ())

(define-block event (task-component)
  (end transparency)
  ((end :requirement (not duration))
   (duration :requirement (not end)))
  (:identifier "VEVENT"))

(define-block todo (task-component)
  (completed completeness due duration)
  ((due :requirement (not duration))
   (duration :requirement (not due)))
  (:identifier "VTODO"))

(define-block journal (date-component)
  ()
  ()
  (:identifier "VJOURNAL"))

(define-block free/busy (calendar-component)
  (contacts end free/busy organizer)
  ((contacts :requirement :optional))
  (:identifier "VFREEBUSY"))

(define-block time-zone (component component-container)
  (tzid last-modification tzurl)
  ()
  (:identifier "VTIMEZONE"))

(define-block alarm (component)
  (action duration repeat trigger)
  ((duration :requirement repeat)
   (repeat :requirement duration))
  (:identifier . "VALARM"))

(define-block audio-alarm (alarm)
  (attachment)
  ((attachment :requirement :optional)))

(define-block display-alarm (alarm)
  (description)
  ((description :requirement :required)))

(define-block email-alarm (alarm)
  (attachment attendee description summary)
  ((description :requirement :required)
   (summary :requirement :required)))

(define-block time-zone-component (component)
  (comment-component start-component recurrence-dates recurrence-rule offset-to offset-from tznames)
  ())

(define-block time-zone-standard (time-zone-component)
  () ()
  (:identifier "STANDARD"))

(define-block time-zone-daylight (time-zone-component)
  () ()
  (:identifier "DAYLIGHT"))

(define-block iana (component)
  () ())

(define-block extension (component)
  ()
  ((identifier :initarg :identifier :accessor identifier)))
