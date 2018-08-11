#|
 This file is a part of iclendar
 (c) 2018 Shirakumo http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package #:org.shirakumo.iclendar)

(defclass component-container ()
  ((components :initform NIL :initarg :components :reader components)))

(define-component (calendar "VCALENDAR") (component-container)
  ((product :constraint :required :property product)
   (scale :property scale)
   (transport-method :property transport-method)
   (version :constraint :required :property version)))

(define-component calendar-component ()
  ((attendees :constraint :multiple :property attendee)
   (comments :constraint :multiple :property comment)
   (request-status :constraint :multiple :property request-status)
   (stamp :constraint :required :property stamp)
   (start :constraint :required :property start)
   (uid :constraint :required :property uid)
   (url :property url)))

(define-component date-component (calendar-component)
  ((attachments :constraint :multiple :property attachment)
   (categories :constraint :multiple :property category)
   (classification :property classification)
   (contacts :constraint :multiple :property contact)
   (created :property created)
   (exception-dates :constraint :multiple :property exception-date)
   (last-modification :property last-modification)
   (organizer :property organizer)
   (recurrence-id :property recurrence-id)
   (recurrence-dates :constraint :multiple :property recurrence-date)
   (recurrence-rule :property recurrence-rule)
   (related :property related)
   (resources :constraint :multiple :property resource)
   (sequence-number :property sequence-number)
   (status :property status)
   (summary :property summary)))

(define-component task-component (date-component)
  ((description :property description)
   (duration :property duration)
   (geographic-location :property geographic-location)
   (location :property location)
   (priority :property priority)))

(define-component (event "VEVENT") (task-component)
  ((transparency :property transparency)
   (end :constraint (not duration) :property end)
   (duration :constraint (not end))))

(define-component (todo "VTODO") (task-component)
  ((completed :property completed)
   (completeness :property completeness)
   (due :constraint (not duration) :property due)
   (duration :constraint (not due))))

(define-component (journal "VJOURNAL") (date-component)
  ())

(define-component (free/busy "VFREEBUSY") (calendar-component)
  ((end :property end)
   (periods :constraint :multiple :property free/busy-period)
   (organizer :property organizer)
   (contact :constraint :optional :property contact)))

(define-component (time-zone "VTIMEZONE") (component-container)
  ((tzid :constraint :required :property tzid)
   (last-modification :property last-modification)
   (tzurl :property tzurl)))

(define-component (alarm "VALARM") ()
  ((action :constraint :required :property action)
   (trigger :constraint :required :property trigger)
   (duration :constraint (and repeat) :property duration)
   (repeat :constraint (and duration) :property repeat)))

(define-component audio-alarm (alarm)
  ((attachment :constraint :optional :property attachment)))

(define-component display-alarm (alarm)
  ((description :constraint :required :property description)))

(define-component email-alarm (alarm)
  ((attachments :constraint :multiple :property attachment)
   (attendee :property attendee)
   (description :constraint :required :property description)
   (summary :constraint :required :property summary)))

(define-component time-zone-component ()
  ((comments :constraint :multiple :property comment)
   (start :constraint :required :property start)
   (recurrence-dates :constraint :multiple :property recurrence-date)
   (recurrence-rule :property recurrence-rule)
   (offset-to :constraint :required :property offset-to)
   (offset-from :constraint :required :property offset-from)
   (tznames :constraint :multiple :property tzname)))

(define-component (time-zone-standard "STANDARD") (time-zone-component)
  ())

(define-component (time-zone-daylight "DAYLIGHT") (time-zone-component)
  ())

(define-component x-component ()
  ((identifier :initarg :identifier :accessor identifier)))
