#|
 This file is a part of iclendar
 (c) 2018 Shirakumo http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package #:org.shirakumo.iclendar)

(define-property (action "ACTION")
  :requirement :required)

(define-property (attachments "ATTACH")
  :requirement :multiple
  :type attachment
  :parameters (encoding value-type))

(define-property (attendees "ATTENDEE")
  :requirement :multiple
  :type address
  :parameters (language calendar-user-type membership role participation-status reply-requested delegatee delegator sent-by common-name directory-entry))

(define-property (categories "CATEGORIES")
  :requirement :multiple
  :parameters (language))

(define-property (classification "CLASS")
  :requirement :optional
  :type (or string (member :public :private :conditential)))

(define-property (comments "COMMENT")
  :requirement :multiple
  :parameters (alternate-representation language))

(define-property (completed "COMPLETED")
  :requirement :optional
  :type date-time)

(define-property (completeness "PERCENT-COMPLETE")
  :requirement :optional
  :type (integer 0 100))

(define-property (contacts "CONTACT")
  :requirement :multiple
  :parameters (alternate-representation language))

(define-property (created "CREATED")
  :requirement :optional
  :type date-time)

(define-property (description "DESCRIPTION")
  :requirement :optional
  :parameters (alternate-representation language))

(define-property (due "DUE")
  :requirement (not duration)
  :type (or date-time date)
  :parameters (value-type time-zone-identifier))

(define-property (duration "DURATION")
  :requirement :optional
  :type duration)

(define-property (end "DTEND")
  :requirement :optional
  :type (or date-time date)
  :parameters (value-type time-zone-identifier))

(define-property (exception-dates "EXDATE")
  :requirement :multiple
  :type (or date-time date)
  :parameters (value-type time-zone-identifier))

(define-property (free/busy "FREEBUSY")
  :requirement :multiple
  :type period
  :parameters (free/busy-type))

(define-property (geographic-location "GEO")
  :requirement :optional
  :type geo)

(define-property (last-modification "LAST-MODIFIED")
  :requirement :optional
  :type date-time)

(define-property (location "LOCATION")
  :requirement :optional
  :parameters (alternate-representation language))

(define-property (offset-from "TZOFFSETFROM")
  :requirement :required
  :type utc-offset)

(define-property (offset-to "TZOFFSETTO")
  :requirement :required
  :type utc-offset)

(define-property (organizer "ORGANIZER")
  :requirement :optional
  :type address
  :parameters (language common-name directory-entry sent-by))

(define-property (priority "PRIORITY")
  :requirement :optional
  :type (integer 0 9))

(define-property (product "PRODID")
  :requirement :required)

(define-property (recurrence "RECURRENCE-ID")
  :requirement :optional
  :type (or date-time date)
  :parameters (value-type time-zone-identifier recurrence-identifier-range))

(define-property (recurrence-dates "RDATE")
  :requirement :multiple
  :type (or date-time date)
  :parameters (value-type time-zone-identifier))

(define-property (recurrence-rule "RRULE")
  :requirement :optional
  :type recurrence)

(define-property (related "RELATED-TO")
  :requirement :optional
  :parameters (relationship-type))

(define-property (repeat "REPEAT")
  :requirement duration
  :type (integer 0))

(define-property (request-status "REQUEST-STATUS")
  :requirement :multiple)

(define-property (resources "RESOURCES")
  :requirement :multiple
  :parameters (alternate-representation language))

(define-property (scale "CALSCALE")
  :requirement :optional)

(define-property (sequence-number "SEQUENCE")
  :requirement :optional
  :type (integer 0))

(define-property (stamp "DTSTAMP")
  :requirement :required
  :type date-time)

(define-property (start "DTSTART")
  :requirement :required
  :type (or date-time date)
  :parameters (value-type time-zone-identifier))

(define-property (status "STATUS")
  :requirement :optional)

(define-property (summary "SUMMARY")
  :requirement :optional
  :parameters (alternate-representation language))

(define-property (transparency "TRANSP")
  :requirement :optional)

(define-property (transport-method "METHOD")
  :requirement :optional)

(define-property (trigger "TRIGGER")
  :requirement :required
  :type (or duration date-time)
  :parameters (value-type time-zone-identifier trigger-on))

(define-property (tzid "TZID")
  :requirement :required)

(define-property (tznames "TZNAME")
  :requirement :multiple
  :parameters (language))

(define-property (tzurl "TZURL")
  :requirement :optional
  :type uri)

(define-property (uid "UID")
  :requirement :required)

(define-property (url "URL")
  :requirement :optional
  :type uri)

(define-property (version "VERSION")
  :requirement :required)
