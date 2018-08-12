#|
 This file is a part of iclendar
 (c) 2018 Shirakumo http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package #:org.shirakumo.iclendar)

;; protocol.lisp
(docs:define-docs
  (function define-parameter
    "Define a new property parameter.

The body may contain a plist of additional information. Currently the following
keys are recognised:

  :TYPE     --- The type of the slot's value. Defaults to TEXT
  :INITARGS --- A list of initarg names. Defaults to the keyword version of the
                NAME
  :READERS  --- A list of names for slot readers. Defaults to a list of simply
                NAME
  :WRITERS  --- A list of names for slot writers. Defaults to a list of simply
                (setf NAME)

See TEXT")

  (type property
    "Base class for properties.

A property definition is the container for a property's value and its metadata
parameters. The constraints a property might have are instead defined on the
container.

If a property's slot name names a parameter class, the slot is turned into a
parameter slot.

See VALUE
See PARAMETERS
See IDENTIFIER
See X-PARAMETERS
See DEFINE-PROPERTY")

  (function value
    "Accessor to the value of a property.

See PROPERTY")

  (function parameters
    "Returns an alist of parameters that are defined and set on the property.

The alist has the following structure:
  PARAMETERS ::= ((IDENTIFIER VALUE)*)
  IDENTIFIER --- The parameter's identifier string.
  VALUE      --- The parameter's value.

See PROPERTY")

  (function identifier
    "Returns the identifier of the class or object.

See PROPERTY
See COMPONENT")

  (function x-parameters
    "Accesses the hash-table of additional, non-standard parameters on the property.

See PROPERTY")

  (function define-property
    "Define a new property class.

The body may contain a plist of additional information. Currently the following
keys are recognised:

  TYPE        --- The type of the property's value. Defaults to TEXT.
  PARAMETERS  --- The list of metadata parameters the property allows. This
                  list should contain symbols of parameter classes.

See DEFINE-PARAMETER
See PROPERTY")
  
  (type component
    "Base class of calendar components.

Components are property containers. Some components may also contain child-
components. A component may have either standard-slots, or property-slots.
Property slots will always contain, if bound, either a single property instance
or a list of property instances depending on the slot's constraint.
A property slot can be set with either a property instance directly, or with
the value of a property instance. The value will automatically be encapsulated
in an appropriate property instance. Property slots are type and constraint
checked.

See X-PROPERTIES
See IDENTIFIER
See PROPERTIES
See DEFINE-COMPONENT")

  (function x-properties
    "Accesses the list of additional, non-standard properties on the component.

See COMPONENT")

  (function properties
    "Returns a list of property instances available in the component.

See PROPERTY
See COMPONENT")

  (function define-component
    "Define a new component class.

If a slot should be a property-, rather than a standard-slot, you must supply
the :PROPERTY slot initarg. The :PROPERTY initarg must name a property class.
Property-slots allow constraints to be enforced on their values using the
:CONSTRAINT initarg. Constraints must have the following form:

   CONSTRAINT    ::= MULTIPLICITY | RELATION
   MULTIPLICITY  ::= :REQUIRED | :OPTIONAL | :MULTIPLE
   RELATION      ::= (and SLOT-NAME) | (not SLOT-NAME)
   SLOT-NAME     --- The name of another property-slot on the component.

If the multiplicity is :REQUIRED, the slot may not be unbound. If it is
:MULTIPLE, the slot may contain a list of property instances. If it is a
relational and, the other slot must be bound as well if this slot is bound. If
it is a relational not, the other slot must not be bound if this slot is bound.

See PROPERTY
See DEFINE-PROPERTY
See COMPONENT"))

;; serializer.lisp
(docs:define-docs
  (function serialize
    "Serialize the given object to the specified output.

The output may be one of the following types:

  STREAM               --- The object is serialised to the given output stream.
  (EQL T)              --- The object is serialised to *STANDARD-OUTPUT*.
  (OR PATHNAME STRING) --- The object is serialised to the given file. The
                           keyword argument :IF-EXISTS is passed on to OPEN.
  (EQL NIL)            --- The object is serialised to a string.

The serialisation is handled by SERIALIZE-OBJECT.

See SERIALIZE-OBJECT")

  (function serialize-object
    "Serialize the object to the given stream.

This function has methods for each of the standard-types, as well as for the
generic property and component classes defined by the iCalendar standard. If
you need special behaviour for the serialisation of a new type, property, or
component, you should add appropriate methods to this function.

See SERIALIZE"))

;; standard-components.lisp
(docs:define-docs
  (type component-container
    "Superclass for classes that can contain child-components.

See COMPONENTS")

  (function components
    "Accesses the list of child components in the container.

See COMPONENT-CONTAINER")

  (type calendar
    "

See PRODUCT
See SCALE
See TRANSPORT-METHOD
See VERSION
See COMPONENT")

  (function product
    "

See CALENDAR")

  (function scale
    "

See CALENDAR")

  (function transport-method
    "

See CALENDAR")

  (function version
    "

See CALENDAR")

  (type calendar-component
    "

See ATTENDEES
See COMMENTS
See REQUEST-STATUS
See STAMP
See START
See UID
See URL
See COMPONENT")

  (function attendees
    "

See CALENDAR-COMPONENT")

  (function comments
    "

See CALENDAR-COMPONENT
See TIME-ZONE-COMPONENT")

  (function request-status
    "

See CALENDAR-COMPONENT")

  (function stamp
    "

See CALENDAR-COMPONENT")

  (function start
    "

See CALENDAR-COMPONENT
See TIME-ZONE-COMPONENT")

  (function uid
    "

See CALENDAR-COMPONENT
See TIME-ZONE")

  (function url
    "

See CALENDAR-COMPONENT
See TIME-ZONE")

  (type date-component
    "

See ATTACHMENTS
See CATEGORIES
See CLASSIFICATION
See CONTACTS
See CRETED
See EXCEPTION-DATES
See LAST-MODIFICATION
See ORGANIZER
See RECURRENCE-ID
See RECURRENCE-DATES
See RECURRENCE-RULE
See RELATED
See RESOURCES
See SEQUENCE-NUMBER
See STATUS
See SUMMARY
See CALENDAR-COMPONENT")

  (function attachments
    "

See DATE-COMPONENT
See EMAIL-ALARM")

  (function categories
    "

See DATE-COMPONENT")

  (function classification
    "

See DATE-COMPONENT")

  (function contacts
    "

See DATE-COMPONENT")

  (function created
    "

See DATE-COMPONENT")

  (function exception-dates
    "

See DATE-COMPONENT")

  (function last-modification
    "

See DATE-COMPONENT
See TIME-ZONE")

  (function organizer
    "

See DATE-COMPONENT
See FREE/BUSY")

  (function recurrence-id
    "

See DATE-COMPONENT")

  (function recurrence-dates
    "

See DATE-COMPONENT
See TIME-ZONE-COMPONENT")

  (function recurrence-rule
    "

See DATE-COMPONENT
See TIME-ZONE-COMPONENT")

  (function related
    "

See DATE-COMPONENT")

  (function resources
    "

See DATE-COMPONENT")

  (function sequence-number
    "

See DATE-COMPONENT")

  (function status
    "

See DATE-COMPONENT")

  (function summary
    "

See DATE-COMPONENT
See EMAIL-ALARM")

  (type task-component
    "

See DESCRIPTION
See DURATION
See GEOGRAPHIC-LOCATION
See LOCATION
See PRIORITY
See DATE-COMPONENT")

  (function description
    "

See TASK-COMPONENT
See DISPLAY-ALARM
See EMAIL-ALARM")

  (function duration
    "

See TASK-COMPONENT")

  (function geographic-location
    "

See TASK-COMPONENT")

  (function location
    "

See TASK-COMPONENT")

  (function priority
    "

See TASK-COMPONENT")

  (type event
    "

See TRANSPARENCY
See END
See DURATION
See TASK-COMPONENT")

  (function transparency
    "

See EVENT")

  (function end
    "

See EVENT
See FREE/BUSY")

  (type todo
    "

See COMPLETED
See COMPLETENESS
See DUE
See DURATION
See TASK-COMPONENT")

  (function completed
    "

See TODO")

  (function completeness
    "

See TODO")

  (function due
    "

See TODO")

  (type journal
    "

See DATE-COMPONENT")

  (type free/busy
    "

See END
See PERIODS
See ORGANIZER
See CONTACT
See CALENDAR-COMPONENT")

  (function periods
    "

See FREE/BUSY")

  (function contact
    "

See FREE/BUSY")

  (type time-zone
    "

See UID
See LAST-MODIFICATION
See URL
See COMPONENT
See COMPONENT-CONTAINER")

  (type alarm
    "

See ACTION
See TRIGGER
See DURATION
See REPEAT
See COMPONENT")

  (function action
    "

See ALARM")

  (function trigger
    "

See ALARM")

  (function repeat
    "

See ALARM")

  (type audio-alarm
    "

See ALARM")

  (function attachment
    "

See ATTACHMENT
See AUDIO-ALARM")

  (type display-alarm
    "

See DESCRIPTION
See ALARM")

  (type email-alarm
    "

See ATTACHMENTS
See ATTENDEE
See DESCRIPTION
See SUMMARY
See ALARM")

  (function attendee
    "

See EMAIL-ALARM")

  (type time-zone-component
    "

See COMMENTS
See START
See RECURRENCE-DATES
See RECURRENCE-RULE
See OFFSET-TO
See OFFSET-FROM
See TZ-NAMES
See COMPONENT")

  (function offset-to
    "

See TIME-ZONE-COMPONENT")

  (function offset-from
    "

See TIME-ZONE-COMPONENT")

  (function tz-names
    "

See TIME-ZONE-COMPONENT")

  (type time-zone-standard
    "

See TIME-ZONE-COMPONENT")

  (type time-zone-daylight
    "

See TIME-ZONE-DAYLIGHT")

  (type x-component
    "

See IDENTIFIER
See COMPONENT"))

;; standard-parameters.lisp
(docs:define-docs
  (function alternate-representation
    "

See COMMENT
See CONTACT
See DESCRIPTION
See LOCATION
See RESOURCE
See SUMMARY")

  (function common-name
    "

See ATTENDEE
See ORGANIZER")

  (function calendar-user-type
    "

See ATTENDEE")

  (function delegator
    "

See ATTENDEE")

  (function delegatee
    "

See ATTENDEE")

  (function directory-entry
    "

See ATTENDEE
See ORGANIZER")

  (function encoding
    "

See ATTACHMENT")

  (function format-type
    "

See ATTACHMENT")

  (function free/busy-type
    "

See FREE/BUSY-PERIOD")

  (function language
    "

See ATTENDEE
See CATEGORIES
See COMMENT
See CONTACT
See DESCRIPTION
See LOCATION
See ORGANIZER
See RESOURCE
See SUMMARY
See TZNAME")

  (function membership
    "

See ATTENDEE")

  (function participation-status
    "

See ATTENDEE")

  (function recurrence-identifier-range
    "")

  (function trigger-on
    "

See TRIGGER")

  (function relationship-type
    "

See RELATED")

  (function role
    "

See ATTENDEE")

  (function reply-requested
    "

See ATTENDEE")

  (function sent-by
    "

See ATTENDEE
See ORGANIZER")

  (function time-zone-identifier
    "

See DUE
See END
See EXCEPTION-DATE
See RECURRENCE-ID
See RECURRENCE-DATE
See START
See TRIGGER")

  (function value-type
    "

See ATTACHMENT
See DTEND
See EXCEPTION-DATE
See RECURRENCE-ID
See RECURRENCE-DATE
See START
See TRIGGER"))

;; standard-properties.lisp
(docs:define-docs
  (type action
    "")

  (type attachment
    "")

  (type attendee
    "")

  (type category
    "")

  (type classification
    "")

  (type comment
    "")

  (type completed
    "")

  (type completeness
    "")

  (type contact
    "")

  (type created
    "")

  (type description
    "")

  (type due
    "")

  (type duration
    "")

  (type end
    "")

  (type exception-date
    "")

  (type free/busy-period
    "")

  (type geographic-location
    "")

  (type last-modification
    "")

  (type location
    "")

  (type offset-from
    "")

  (type offset-to
    "")

  (type organizer
    "")

  (type priority
    "")

  (type product
    "")

  (type recurrence-id
    "")

  (type recurrence-date
    "")

  (type recurrence-rule
    "")

  (type related
    "")

  (type repeat
    "")

  (type request-status
    "")

  (type resource
    "")

  (type scale
    "")

  (type sequence-number
    "")

  (type stamp
    "")

  (type start
    "")

  (type status
    "")

  (type summary
    "")

  (type transparency
    "")

  (type transport-method
    "")

  (type trigger
    "")

  (type tzid
    "")

  (type tzname
    "")

  (type tzurl
    "")

  (type uid
    "")

  (type url
    "")

  (type version
    ""))

;; standard-types.lisp
(docs:define-docs
  (type address-list
    "Type for a list with each item being of type ADDRESS

See ADDRESS")

  (type second-list
    "Type for a list with each item being an integer in [0,60].")

  (type minute-list
    "Type for a list with each item being an integer in [0,59].")

  (type hour-list
    "Type for a list with each item being an integer in [0,23].")

  (type week-day-list
    "Type for a list with each item being a WEEK-DAY-NUM.

See WEEK-DAY-NUM")

  (type month-day-list
    "Type for a list with each item being an integer in [1,31].")

  (type year-day-list
    "Type for a list with each item being an integer in [1,366].")

  (type week-list
    "Type for a list with each item being an integer in [1,53].")

  (type month-list
    "Type for a list with each item being an integer in [1,12].")

  (type attachment
    "Type for attachment values.

See URI
See CL:PATHNAME
See CL:VECTOR")

  (type address
    "Type for addresses to a resource.

Not to be confused by a real-world postal address.

See URI")

  (type week-day
    "Type for a week-day.

Can be one of :SUNDAY :MONDAY :TUESDAY :WEDNESDAY :THURSDAY :FRIDAY :SATURDAY")

  (type week-day-num
    "Type for a week-day recurrence constraint.

See WEEK-DAY-NUM-WEEK
See WEEK-DAY-NUM-WEEK-DAY")

  (function week-day-num-week
    "

See WEEK-DAY-NUM")

  (function week-day-num-week-day
    "

See WEEK-DAY-NUM")

  (type date
    "

See DATE-YEAR
See DATE-MONTH
See DATE-DATE")

  (function date-year
    "

See DATE")

  (function date-month
    "

See DATE")

  (function date-date
    "

See DATE")

  (type date-time
    "

See DATE-TIME-YEAR
See DATE-TIME-MONTH
See DATE-TIME-DATE
See DATE-TIME-HOUR
See DATE-TIME-MINUTE
See DATE-TIME-SECOND
See DATE-TIME-UTC-P")

  (function date-time-month
    "

See DATE-TIME")

  (function date-time-year
    "

See DATE-TIME")

  (function date-time-date
    "

See DATE-TIME")

  (function date-time-hour
    "

See DATE-TIME")

  (function date-time-minute
    "

See DATE-TIME")

  (function date-time-second
    "

See DATE-TIME")

  (function date-time-utc-p
    "

See DATE-TIME")

  (type time-span
    "

See TIME-SPAN-WEEK
See TIME-SPAN-HOUR
See TIME-SPAN-MINUTE
See TIME-SPAN-SECOND
See TIME-SPAN-DAY
See TIME-SPAN-INC-P")

  (function time-span-week
    "

See TIME-SPAN")

  (function time-span-hour
    "

See TIME-SPAN")

  (function time-span-minute
    "

See TIME-SPAN")

  (function time-span-second
    "

See TIME-SPAN")

  (function time-span-day
    "

See TIME-SPAN")

  (function time-span-inc-p
    "

See TIME-SPAN")

  (type period
    "

See PERIOD-START
See PERIOD-LIMIT")

  (function period-start
    "

See PERIOD")

  (function period-limit
    "

See PERIOD")

  (type recurrence
    "

See RECURRENCE-FREQUENCY
See RECURRENCE-END-DATE
See RECURRENCE-COUNT
See RECURRENCE-INTERVAL
See RECURRENCE-BY-SECONDS
See RECURRENCE-BY-MINUTES
See RECURRENCE-BY-HOURS
See RECURRENCE-BY-DAYS
See RECURRENCE-BY-MONTH-DAYS
See RECURRENCE-BY-YEAR-DAYS
See RECURRENCE-BY-WEEKS
See RECURRENCE-BY-MONTHS
See RECURRENCE-BY-SET-POS
See RECURRENCE-WEEK-START")

  (function recurrence-frequency
    "

See RECURRENCE")

  (function recurrence-end-date
    "

See RECURRENCE")

  (function recurrence-count
    "

See RECURRENCE")

  (function recurrence-interval
    "

See RECURRENCE")

  (function recurrence-by-seconds
    "

See RECURRENCE")

  (function recurrence-by-minutes
    "

See RECURRENCE")

  (function recurrence-by-hours
    "

See RECURRENCE")

  (function recurrence-by-days
    "

See RECURRENCE")

  (function recurrence-by-month-days
    "

See RECURRENCE")

  (function recurrence-by-year-days
    "

See RECURRENCE")

  (function recurrence-by-weeks
    "

See RECURRENCE")

  (function recurrence-by-months
    "

See RECURRENCE")

  (function recurrence-by-set-pos
    "

See RECURRENCE")

  (function recurrence-week-start
    "

See RECURRENCE")

  (type uri
    "Type for unified resource identifiers.

This is just a string. No actual checks beyond this are done.")

  (type utc-offset
    "Representation of a time-zone offset from UTC.

See UTC-OFFSET-HOUR
See UTC-OFFSET-MINUTE
See UTC-OFFSET-SECOND
See UTC-OFFSET-INC-P")

  (function utc-offset-hour
    "The number of hours [0,23] offset from UTC.

See UTC-OFFSET")

  (function utc-offset-minute
    "The number of minutes [0,59] offset from UTC.

See UTC-OFFSET")

  (function utc-offset-second
    "The number of seconds [0,60] offset from UTC.

See UTC-OFFSET")

  (function utc-offset-inc-p
    "Whether the offset is added to UTC.

See UTC-OFFSET")

  (type geo
    "Representation for a geographic location.

See GEO-LAT
See GEO-LNG")

  (function geo-lat
    "Accesses the latitude of the location as a float.

See GEO")

  (function geo-lng
    "Accesses the longitude of the location as a float.

See GEO")

  (type text
    "Type for text values.

This is just a string.")

  (type language
    "Type for language codes.

This is just a string."))
