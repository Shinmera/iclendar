#|
 This file is a part of iclendar
 (c) 2018 Shirakumo http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(defpackage #:iclendar
  (:nicknames #:org.shirakumo.iclendar)
  (:use #:cl)
  (:shadow #:block)
  ;; protocol.lisp
  (:export
   #:define-parameter
   #:property
   #:value
   #:parameters
   #:identifier
   #:x-parameters
   #:define-property
   #:component
   #:x-properties
   #:properties
   #:define-component)
  ;; serializer.lisp
  (:export
   #:serialize
   #:serialize-object)
  ;; standard-components.lisp
  (:export
   #:component-container
   #:components
   #:calendar
   #:product
   #:scale
   #:transport-method
   #:version
   #:calendar-component
   #:attendees
   #:comments
   #:request-status
   #:stamp
   #:start
   #:uid
   #:url
   #:date-component
   #:attachments
   #:categories
   #:classification
   #:contacts
   #:created
   #:exception-dates
   #:last-modification
   #:organizer
   #:recurrence-id
   #:recurrence-dates
   #:recurrence-rule
   #:related
   #:resources
   #:sequence-number
   #:status
   #:summary
   #:task-component
   #:description
   #:duration
   #:geographic-location
   #:location
   #:priority
   #:event
   #:transparency
   #:end
   #:duration
   #:todo
   #:completed
   #:completeness
   #:due
   #:duration
   #:journal
   #:free/busy
   #:end
   #:periods
   #:organizer
   #:contact
   #:time-zone
   #:uid
   #:last-modification
   #:url
   #:alarm
   #:action
   #:trigger
   #:duration
   #:repeat
   #:audio-alarm
   #:attachment
   #:display-alarm
   #:description
   #:email-alarm
   #:attachments
   #:attendee
   #:description
   #:summary
   #:time-zone-component
   #:comments
   #:start
   #:recurrence-dates
   #:recurrence-rule
   #:offset-to
   #:offset-from
   #:tz-names
   #:time-zone-standard
   #:time-zone-daylight
   #:x-component
   #:identifier)
  ;; standard-parameters.lisp
  (:export
   #:alternate-representation
   #:common-name
   #:calendar-user-type
   #:delegator
   #:delegatee
   #:directory-entry
   #:encoding
   #:format-type
   #:free/busy-type
   #:language
   #:membership
   #:participation-status
   #:recurrence-identifier-range
   #:trigger-on
   #:relationship-type
   #:role
   #:reply-requested
   #:sent-by
   #:time-zone-identifier
   #:value-type)
  ;; standard-properties.lisp
  (:export
   #:action
   #:attachment
   #:attendee
   #:category
   #:classification
   #:comment
   #:completed
   #:completeness
   #:contact
   #:created
   #:description
   #:due
   #:duration
   #:end
   #:exception-date
   #:free/busy-period
   #:geographic-location
   #:last-modification
   #:location
   #:offset-from
   #:offset-to
   #:organizer
   #:priority
   #:product
   #:recurrence-id
   #:recurrence-date
   #:recurrence-rule
   #:related
   #:repeat
   #:request-status
   #:resource
   #:scale
   #:sequence-number
   #:stamp
   #:start
   #:status
   #:summary
   #:transparency
   #:transport-method
   #:trigger
   #:tzid
   #:tzname
   #:tzurl
   #:uid
   #:url
   #:version)
  ;; standard-types.lisp
  (:export
   #:address-list
   #:second-list
   #:minute-list
   #:hour-list
   #:week-day-list
   #:month-day-list
   #:year-day-list
   #:week-list
   #:month-list
   #:attachment-value
   #:address
   #:week-day
   #:week-day-num
   #:make-week-day-num
   #:week-day-num-week
   #:week-day-num-week-day
   #:date
   #:make-date
   #:date-year
   #:date-month
   #:date-date
   #:date-time
   #:make-date-time
   #:date-time-month
   #:date-time-year
   #:date-time-date
   #:date-time-hour
   #:date-time-minute
   #:date-time-second
   #:date-time-utc-p
   #:time-span
   #:make-time-span
   #:time-span-week
   #:time-span-hour
   #:time-span-minute
   #:time-span-second
   #:time-span-day
   #:time-span-inc-p
   #:period
   #:make-period
   #:period-start
   #:period-limit
   #:recurrence
   #:make-recurrence
   #:recurrence-frequency
   #:recurrence-end-date
   #:recurrence-count
   #:recurrence-interval
   #:recurrence-by-seconds
   #:recurrence-by-minutes
   #:recurrence-by-hours
   #:recurrence-by-days
   #:recurrence-by-month-days
   #:recurrence-by-year-days
   #:recurrence-by-weeks
   #:recurrence-by-months
   #:recurrence-by-set-pos
   #:recurrence-week-start
   #:uri
   #:utc-offset
   #:make-utc-offset
   #:utc-offset-hour
   #:utc-offset-minute
   #:utc-offset-second
   #:utc-offset-inc-p
   #:geo
   #:make-geo
   #:geo-lat
   #:geo-lng
   #:text))