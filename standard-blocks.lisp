#|
 This file is a part of iclendar
 (c) 2018 Shirakumo http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package #:org.shirakumo.iclendar)

(define-block calendar ()
  ((components
    :identifier NIL
    :requirement :multiple)
   (transport-method
    :identifier "METHOD"
    :requirement :optional)
   (product
    :identifier "PRODID"
    :requirement :required)
   (scale
    :identifier "CALSCALE"
    :requirement :optional)
   (version
    :identifier "VERSION"
    :requirement :required))
  (:identifier "VCALENDAR"))

(define-block component ()
  ((extensions
    :identifier NIL
    :requirement :multiple)
   (iana
    :identifier NIL
    :requirement :multiple)))

(define-block calendar-component (component)
  ((attendees
    :identifier "ATTENDEE"
    :requirement :multiple
    :type address)
   (comments
    :identifier "COMMENT"
    :requirement :multiple)
   (request-status
    :identifier "REQUEST-STATUS"
    :requirement :multiple)
   (stamp
    :identifier "DTSTAMP"
    :requirement :required
    :type date-time)
   (start
    :identifier "DTSTART"
    :requirement (not method)
    :type (or date-time date))
   (uid
    :identifier "UID"
    :requirement :required)
   (url
    :identifier "URL"
    :requirement :optional
    :type uri)))

(define-block date-component (calendar-component)
  ((attachments
    :identifier "ATTACH"
    :requirement :multiple
    :type attachment)
   (categories
    :identifier "CATEGORIES"
    :requirement :multiple)
   (classification
    :identifier "CLASS"
    :requirement :optional
    :type (or string (member :public :private :conditential)))
   (contacts
    :identifier "CONTACT"
    :requirement :multiple)
   (created
    :identifier "CREATED"
    :requirement :optional
    :type date-time)
   (exception-dates
    :identifier "EXDATE"
    :requirement :multiple
    :type (or date-time date))
   (last-modification
    :identifier "LAST-MODIFIED"
    :requirement :optional
    :type date-time)
   (organizer
    :identifier "ORGANIZER"
    :requirement :optional
    :type address)
   (recurrence-dates
    :identifier "RDATE"
    :requirement :multiple
    :type (or date-time date))
   (recurrence
    :identifier "RECURRENCE-ID"
    :requirement :optional
    :type (or date-time date))
   (related
    :identifier "RELATED-TO"
    :requirement :optional)
   (resources
    :identifier "RESOURCES"
    :requirement :multiple)
   (recurrence-rule
    :identifier "RRULE"
    :requirement :optional
    :type recurrence)
   (sequence-number
    :identifier "SEQUENCE"
    :requirement :optional
    :type (integer 0))
   (status
    :identifier "STATUS"
    :requirement :optional)
   (summary
    :identifier "SUMMARY"
    :requirement :optional)))

(define-block task-component (date-component)
  ((description
    :identifier "DESCRIPTION"
    :requirement :optional)
   (duration
    :identifier "DURATION"
    :requirement :optional
    :type duration)
   (geographic-location
    :identifier "GEO"
    :requirement :optional
    :type geo)
   (location
    :identifier "LOCATION"
    :requirement :optional)
   (priority
    :identifier "PRIORITY"
    :requirement :optional
    :type (integer 0 9))))

(define-block event (task-component)
  ((end
    :identifier "DTEND"
    :requirement (not duration)
    :type (or date-time date))
   (duration
    :identifier "DURATION"
    :requirement (not end)
    :type duration)
   (transparency
    :identifier "TRANSP"
    :requirement :optional))
  (:identifier "VEVENT"))

(define-block todo (task-component)
  ((completed
    :identifier "COMPLETED"
    :requirement :optional
    :type date-time)
   (completeness
    :identifier "PERCENT-COMPLETE"
    :requirement :optional
    :type (integer 0 100))
   (due
    :identifier "DUE"
    :requirement (not duration)
    :type (or date-time date))
   (duration
    :identifier "DURATION"
    :requirement (not due)
    :type duration))
  (:identifier "VTODO"))

(define-block journal (date-component)
  ()
  (:identifier "VJOURNAL"))

(define-block free/busy (calendar-component)
  ((contact
    :identifier "CONTACT"
    :requirement :optional)
   (end
    :identifier "DTEND"
    :requirement :optional
    :type (or date-time date))
   (free/busy
    :identifier "FREEBUSY"
    :requirement :multiple
    :type period)
   (organizer
    :identifier "ORGANIZER"
    :requirement :optional
    :type address))
  (:identifier "VFREEBUSY"))

(define-block time-zone (component)
  ((tzid
    :identifier "TZID"
    :requirement :required)
   (last-modification
    :identifier "LAST-MODIFIED"
    :requirement :optional
    :type date-time)
   (tzurl
    :identifier "TZURL"
    :requirement :optional
    :type uri)
   (components
    :identifier NIL
    :requirement :multiple))
  (:identifier "VTIMEZONE"))

(define-block alarm (component)
  ((action
    :identifier "ACTION"
    :requirement :required)
   (trigger
    :identifier "TRIGGER"
    :requirement :required
    :type (or duration date-time))
   (duration
    :identifier "DURATION"
    :requirement repeat
    :type duration)
   (repeat
    :identifier "REPEAT"
    :requirement duration
    :type (integer 0)))
  (:identifier . "VALARM"))

(define-block audio-alarm (alarm)
  ((attachment
    :identifier "ATTACH"
    :requirement :optional
    :type attachment)))

(define-block display-alarm (alarm)
  ((description
    :identifier "DESCRIPTION"
    :requirement :required)))

(define-block email-alarm (alarm)
  ((attachments
    :identifier "ATTACH"
    :requirement :multiple
    :type attachment)
   (description
    :identifier "DESCRIPTION"
    :requirement :required)
   (summary
    :identifier "SUMMARY"
    :requirement :required)
   (attendees
    :identifier "ATTENDEE"
    :requirement :multiple
    :type address)))

(define-block time-zone-component (component)
  ((start
    :identifier "DTSTART"
    :requirement :required
    :type (or date-time date))
   (offset-to
    :identifier "TZOFFSETTO"
    :requirement :required
    :type utc-offset)
   (offset-from
    :identifier "TZOFFSETFROM"
    :requirement :required
    :type utc-offset)
   (recurrence-rule
    :identifier "RRULE"
    :requirement :optional
    :type recurrence)
   (comments
    :identifier "COMMENT"
    :requirement :multiple)
   (recurrence-dates
    :identifier "RDATE"
    :requirement :multiple
    :type (or date-time date))
   (tznames
    :identifier "TZNAME"
    :requirement :multiple)))

(define-block time-zone-standard (time-zone-component)
  ()
  (:identifier "STANDARD"))

(define-block time-zone-daylight (time-zone-component)
  ()
  (:identifier "DAYLIGHT"))

(define-block iana (component)
  ())

(define-block extension (component)
  ((identifier :initarg :identifier :accessor identifier)))
