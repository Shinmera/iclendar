#|
 This file is a part of iclendar
 (c) 2018 Shirakumo http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package #:org.shirakumo.iclendar)

(define-property (action "ACTION"))

(define-property (attachment "ATTACH")
  :type attachment-value
  :parameters (encoding value-type format-type))

(define-property (attendee "ATTENDEE")
  :type address
  :parameters (language calendar-user-type membership role participation-status reply-requested delegatee delegator sent-by common-name directory-entry))

(define-property (category "CATEGORIES")
  :parameters (language))

(define-property (classification "CLASS")
  :type (or string (member :public :private :confitential)))

(define-property (comment "COMMENT")
  :parameters (alternate-representation language))

(define-property (completed "COMPLETED")
  :type date-time)

(define-property (completeness "PERCENT-COMPLETE")
  :type (integer 0 100))

(define-property (contact "CONTACT")
  :parameters (alternate-representation language))

(define-property (created "CREATED")
  :type date-time)

(define-property (description "DESCRIPTION")
  :parameters (alternate-representation language))

(define-property (due "DUE")
  :type (or date-time date)
  :parameters (value-type time-zone-identifier))

(define-property (duration "DURATION")
  :type time-span)

(define-property (end "DTEND")
  :type (or date-time date)
  :parameters (value-type time-zone-identifier))

(define-property (exception-date "EXDATE")
  :type (or date-time date)
  :parameters (value-type time-zone-identifier))

(define-property (free/busy-period "FREEBUSY")
  :type period
  :parameters (free/busy-type))

(define-property (geographic-location "GEO")
  :type geo)

(define-property (last-modification "LAST-MODIFIED")
  :type date-time)

(define-property (location "LOCATION")
  :parameters (alternate-representation language))

(define-property (offset-from "TZOFFSETFROM")
  :type utc-offset)

(define-property (offset-to "TZOFFSETTO")
  :type utc-offset)

(define-property (organizer "ORGANIZER")
  :type address
  :parameters (language common-name directory-entry sent-by))

(define-property (priority "PRIORITY")
  :type (integer 0 9))

(define-property (product "PRODID"))

(define-property (recurrence-id "RECURRENCE-ID")
  :type (or date-time date)
  :parameters (value-type time-zone-identifier recurrence-identifier-range))

(define-property (recurrence-date "RDATE")
  :type (or date-time date)
  :parameters (value-type time-zone-identifier))

(define-property (recurrence-rule "RRULE")
  :type recurrence)

(define-property (related "RELATED-TO")
  :parameters (relationship-type))

(define-property (repeat "REPEAT")
  :type (integer 0))

(define-property (request-status "REQUEST-STATUS"))

(define-property (resource "RESOURCES")
  :parameters (alternate-representation language))

(define-property (scale "CALSCALE"))

(define-property (sequence-number "SEQUENCE")
  :type (integer 0))

(define-property (stamp "DTSTAMP")
  :type date-time)

(define-property (start "DTSTART")
  :type (or date-time date)
  :parameters (value-type time-zone-identifier))

(define-property (status "STATUS"))

(define-property (summary "SUMMARY")
  :parameters (alternate-representation language))

(define-property (transparency "TRANSP")
  :type (or string (member :opaque :transparent)))

(define-property (transport-method "METHOD"))

(define-property (trigger "TRIGGER")
  :type (or time-span date-time)
  :parameters (value-type time-zone-identifier trigger-on))

(define-property (tzid "TZID"))

(define-property (tzname "TZNAME")
  :parameters (language))

(define-property (tzurl "TZURL")
  :type uri)

(define-property (uid "UID"))

(define-property (url "URL")
  :type uri)

(define-property (version "VERSION"))
