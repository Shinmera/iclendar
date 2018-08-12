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
    "The calendar object is the top-level object in an iCalendar stream.

It defines the base iCalendar metadata and contains other child components that make up the
actual data. The version parameter defaults to \"2.0\", corresponding to RFC5545's version.

See PRODUCT
See SCALE
See TRANSPORT-METHOD
See VERSION
See COMPONENT")

  (function product
    "Accessor to the product ID of the calendar.

The product ID is required and should identify the product that's emitting the calendar object.

See CALENDAR
See PRODUCT")

  (function scale
    "Accessor to the scale of the calendar.

See CALENDAR
See SCALE")

  (function transport-method
    "Accessor to the transport-method of the calendar.

See CALENDAR
See TRANSPORT-METHOD")

  (function version
    "Accessor to the version of the calendar.

See CALENDAR
See VERSION")

  (type calendar-component
    "This is a mixin class for common properties in subclasses.

See ATTENDEES
See COMMENTS
See REQUEST-STATUS
See STAMP
See START
See UID
See URL
See COMPONENT")

  (function attendees
    "Accessor to the attendees list of the component.

See ATTENDEE
See CALENDAR-COMPONENT")

  (function comments
    "Accessor to the comments list of the component.

See COMMENT
See CALENDAR-COMPONENT
See TIME-ZONE-COMPONENT")

  (function request-status
    "Accessor to the request status of the component.

See REQUEST-STATUS
See CALENDAR-COMPONENT")

  (function stamp
    "Accessor to the creation stamp of the component.

See STAMP
See CALENDAR-COMPONENT")

  (function start
    "Accessor to the start date of the component.

See START
See CALENDAR-COMPONENT
See TIME-ZONE-COMPONENT")

  (function uid
    "Accessor to the component's unique ID.

See UID
See TZID
See CALENDAR-COMPONENT
See TIME-ZONE")

  (function url
    "Accessor to the component's URL.

See URL
See TZURL
See CALENDAR-COMPONENT
See TIME-ZONE")

  (type date-component
    "This is a mixin class for common properties in subclasses.

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
    "Accessor to the attachments list of the component.

See ATTACHMENT
See DATE-COMPONENT
See EMAIL-ALARM")

  (function categories
    "Accessor to the category list of the component.

See CATEGORY
See DATE-COMPONENT")

  (function classification
    "Accessor to the classification of the component.

See CLASSIFICATION
See DATE-COMPONENT")

  (function contacts
    "Accessor to the contacts list of the component.

See CONTACT
See DATE-COMPONENT")

  (function created
    "Accessor to the created date of the component.

See CREATED
See DATE-COMPONENT")

  (function exception-dates
    "Accessor to the list of exception dates of the component.

See EXCEPTION-DATE
See DATE-COMPONENT")

  (function last-modification
    "Accessor to the last modification date of the component.

See LAST-MODIFICATION
See DATE-COMPONENT
See TIME-ZONE")

  (function organizer
    "Accessor to the organizer of the component.

See ORGANIZER
See DATE-COMPONENT
See FREE/BUSY")

  (function recurrence-id
    "Accessor to the recurrence ID of the component.

See RECURRENCE-ID
See DATE-COMPONENT")

  (function recurrence-dates
    "Accessor to the list of recurrence dates of the component.

See RECURRENCE-DATE
See DATE-COMPONENT
See TIME-ZONE-COMPONENT")

  (function recurrence-rule
    "Accessor to the recurrence rule descriptor of the component.

See RECURRENCE-RULE
See DATE-COMPONENT
See TIME-ZONE-COMPONENT")

  (function related
    "Accessor to the related entity of the component.

See RELATED
See DATE-COMPONENT")

  (function resources
    "Accessor to the list of resources of the component.

See RESOURCES
See DATE-COMPONENT")

  (function sequence-number
    "Accessor to the sequence number of the component.

See SEQUENCE-NUMBER
See DATE-COMPONENT")

  (function status
    "Accessor to the status of the component.

See STATUS
See DATE-COMPONENT")

  (function summary
    "Accessor to the summary of the component.

See SUMMARY
See DATE-COMPONENT
See EMAIL-ALARM")

  (type task-component
    "This is a mixin class for common properties in subclasses.

See DESCRIPTION
See DURATION
See GEOGRAPHIC-LOCATION
See LOCATION
See PRIORITY
See DATE-COMPONENT")

  (function description
    "Accessor to the description of the component.

See DESCRIPTION
See TASK-COMPONENT
See DISPLAY-ALARM
See EMAIL-ALARM")

  (function duration
    "Accessor to the duration of the component.

See DURATION
See TASK-COMPONENT")

  (function geographic-location
    "Accessor to the geographic location of the component.

See GEOGRAPHIC-LOCATION
See TASK-COMPONENT")

  (function location
    "Accessor to the non-geographic location of the component.

See LOCATION
See TASK-COMPONENT")

  (function priority
    "Accessor to the priority of the component.

See PRIORITY
See TASK-COMPONENT")

  (type event
    "Component for a generic event kind of object in a calendar.

From RFC5545:
  A VEVENT calendar component is a grouping of
  component properties, possibly including VALARM calendar
  components, that represents a scheduled amount of time on a
  calendar.  For example, it can be an activity; such as a one-hour
  long, department meeting from 8:00 AM to 9:00 AM, tomorrow.
  Generally, an event will take up time on an individual calendar.
  Hence, the event will appear as an opaque interval in a search for
  busy time.  Alternately, the event can have its Time Transparency
  set to TRANSPARENT in order to prevent blocking of the event in
  searches for busy time.

  The VEVENT is also the calendar component used to specify an
  anniversary or daily reminder within a calendar.  These events
  have a DATE value type for the DTSTART property instead of the
  default value type of DATE-TIME.  If such a VEVENT has a DTEND
  property, it MUST be specified as a DATE value also.  The
  anniversary type of VEVENT can span more than one date (i.e.,
  DTEND property value is set to a calendar date after the
  DTSTART property value).  If such a VEVENT has a DURATION
  property, it MUST be specified as a dur-day or dur-week value.

  The DTSTART property for a VEVENT specifies the inclusive
  start of the event.  For recurring events, it also specifies the
  very first instance in the recurrence set.  The DTEND property
  for a VEVENT calendar component specifies the non-inclusive end
  of the event.  For cases where a VEVENT calendar component
  specifies a DTSTART property with a DATE value type but no
  DTEND nor DURATION property, the event's duration is taken to
  be one day.  For cases where a VEVENT calendar component
  specifies a DTSTART property with a DATE-TIME value type but no
  DTEND property, the event ends on the same calendar date and
  time of day specified by the DTSTART property.

  The VEVENT calendar component cannot be nested within another
  calendar component.  However, VEVENT calendar components can be
  related to each other or to a VTODO or to a VJOURNAL calendar
  component with the RELATED-TO property.

See TRANSPARENCY
See END
See DURATION
See TASK-COMPONENT")

  (function transparency
    "Accessor to the transparency of the component.

See TRANSPARENCY
See EVENT")

  (function end
    "Accessor to the end date of the component.

See END
See EVENT
See FREE/BUSY")

  (type todo
    "Component representing a todo item in a calendar.

From RFC5545:
  A VTODO calendar component is a grouping of component
  properties and possibly VALARM calendar components that
  represent an action-item or assignment.  For example, it can be
  used to represent an item of work assigned to an individual; such
  as \"turn in travel expense today\".

  The VTODO calendar component cannot be nested within another
  calendar component.  However, VTODO calendar components can be
  related to each other or to a VEVENT or to a VJOURNAL calendar
  component with the RELATED-TO property.

  A VTODO calendar component without the DTSTART and DUE (or
  DURATION) properties specifies a to-do that will be associated
  with each successive calendar date, until it is completed.

See COMPLETED
See COMPLETENESS
See DUE
See DURATION
See TASK-COMPONENT")

  (function completed
    "Accessor to whether the component is completed or not.

See COMPLETED
See TODO")

  (function completeness
    "Accessor to the completeness percentage of the component.

See COMPLETENESS
See TODO")

  (function due
    "Accessor to the due date of the component-

See DUE
See TODO")

  (type journal
    "Component describing a generic journal entry in a calendar.

From RFC5545:
  A VJOURNAL calendar component is a grouping of
  component properties that represent one or more descriptive text
  notes associated with a particular calendar date.  The DTSTART
  property is used to specify the calendar date with which the
  journal entry is associated.  Generally, it will have a DATE value
  data type, but it can also be used to specify a DATE-TIME value
  data type.  Examples of a journal entry include a daily record of
  a legislative body or a journal entry of individual telephone
  contacts for the day or an ordered list of accomplishments for the
  day.  The VJOURNAL calendar component can also be used to
  associate a document with a calendar date.

  The VJOURNAL calendar component does not take up time on a
  calendar.  Hence, it does not play a role in free or busy time
  searches -- it is as though it has a time transparency value of
  TRANSPARENT.  It is transparent to any such searches.

  The VJOURNAL calendar component cannot be nested within another
  calendar component.  However, VJOURNAL calendar components can
  be related to each other or to a VEVENT or to a VTODO calendar
  component, with the RELATED-TO property.

See DATE-COMPONENT")

  (type free/busy
    "Component to describe a region of time during which a person is free or busy.

From RFC5545:
  A VFREEBUSY calendar component is a grouping of
  component properties that represents either a request for free or
  busy time information, a reply to a request for free or busy time
  information, or a published set of busy time information.

  When used to request free/busy time information, the ATTENDEE
  property specifies the calendar users whose free/busy time is
  being requested; the ORGANIZER property specifies the calendar
  user who is requesting the free/busy time; the DTSTART and
  DTEND properties specify the window of time for which the free/
  busy time is being requested; the UID and DTSTAMP properties
  are specified to assist in proper sequencing of multiple free/busy
  time requests.

  When used to reply to a request for free/busy time, the ATTENDEE
  property specifies the calendar user responding to the free/busy
  time request; the ORGANIZER property specifies the calendar user
  that originally requested the free/busy time; the FREEBUSY
  property specifies the free/busy time information (if it exists);
  and the UID and DTSTAMP properties are specified to assist in
  proper sequencing of multiple free/busy time replies.

  When used to publish busy time, the ORGANIZER property specifies
  the calendar user associated with the published busy time; the
  DTSTART and DTEND properties specify an inclusive time window
  that surrounds the busy time information; the FREEBUSY property
  specifies the published busy time information; and the DTSTAMP
  property specifies the DATE-TIME that iCalendar object was
  created.

  The VFREEBUSY calendar component cannot be nested within another
  calendar component.  Multiple VFREEBUSY calendar components can
  be specified within an iCalendar object.  This permits the
  grouping of free/busy information into logical collections, such
  as monthly groups of busy time information.

  The VFREEBUSY calendar component is intended for use in
  iCalendar object methods involving requests for free time,
  requests for busy time, requests for both free and busy, and the
  associated replies.

  Free/Busy information is represented with the FREEBUSY property.
  This property provides a terse representation of time periods.
  One or more FREEBUSY properties can be specified in the
  VFREEBUSY calendar component.

  When present in a VFREEBUSY calendar component, the DTSTART
  and DTEND properties SHOULD be specified prior to any FREEBUSY
  properties.

  The recurrence properties (RRULE, RDATE, EXDATE) are not
  permitted within a VFREEBUSY calendar component.  Any recurring
  events are resolved into their individual busy time periods using
  the FREEBUSY property.

See END
See PERIODS
See ORGANIZER
See CONTACT
See CALENDAR-COMPONENT")

  (function periods
    "Accessor to the list of free/busy periods of the component.

See FREE/BUSY-PERIOD
See FREE/BUSY")

  (function contact
    "Accessor to the contact of the component.

See CONTACT
See FREE/BUSY")

  (type time-zone
    "Component to describe a grouping of parts to describe a time zone.

From RFC5545:
  A time zone is unambiguously defined by the set of time
  measurement rules determined by the governing body for a given
  geographic area.  These rules describe, at a minimum, the base
  offset from UTC for the time zone, often referred to as the
  Standard Time offset.  Many locations adjust their Standard Time
  forward or backward by one hour, in order to accommodate seasonal
  changes in number of daylight hours, often referred to as Daylight
  Saving Time.  Some locations adjust their time by a fraction of an
  hour.  Standard Time is also known as Winter Time.  Daylight
  Saving Time is also known as Advanced Time, Summer Time, or Legal
  Time in certain countries.  The following table shows the changes
  in time zone rules in effect for New York City starting from 1967.
  Each line represents a description or rule for a particular
  observance.

                     Effective Observance Rule

     +-----------+--------------------------+--------+--------------+
     | Date      | (Date-Time)              | Offset | Abbreviation |
     +-----------+--------------------------+--------+--------------+
     | 1967-1973 | last Sun in Apr, 02:00   | -0400  | EDT          |
     |           |                          |        |              |
     | 1967-2006 | last Sun in Oct, 02:00   | -0500  | EST          |
     |           |                          |        |              |
     | 1974-1974 | Jan 6, 02:00             | -0400  | EDT          |
     |           |                          |        |              |
     | 1975-1975 | Feb 23, 02:00            | -0400  | EDT          |
     |           |                          |        |              |
     | 1976-1986 | last Sun in Apr, 02:00   | -0400  | EDT          |
     |           |                          |        |              |
     | 1987-2006 | first Sun in Apr, 02:00  | -0400  | EDT          |
     |           |                          |        |              |
     | 2007-*    | second Sun in Mar, 02:00 | -0400  | EDT          |
     |           |                          |        |              |
     | 2007-*    | first Sun in Nov, 02:00  | -0500  | EST          |
     +-----------+--------------------------+--------+--------------+

   Note: The specification of a global time zone registry is not
     addressed by this document and is left for future study.
     However, implementers may find the TZ database [TZDB] a useful
     reference.  It is an informal, public-domain collection of time
     zone information, which is currently being maintained by
     volunteer Internet participants, and is used in several
     operating systems.  This database contains current and
     historical time zone information for a wide variety of
     locations around the globe; it provides a time zone identifier
     for every unique time zone rule set in actual use since 1970,
     with historical data going back to the introduction of standard
     time.

  Interoperability between two calendaring and scheduling
  applications, especially for recurring events, to-dos or journal
  entries, is dependent on the ability to capture and convey date
  and time information in an unambiguous format.  The specification
  of current time zone information is integral to this behavior.

  If present, the VTIMEZONE calendar component defines the set of
  Standard Time and Daylight Saving Time observances (or rules) for
  a particular time zone for a given interval of time.  The
  VTIMEZONE calendar component cannot be nested within other
  calendar components.  Multiple VTIMEZONE calendar components can
  exist in an iCalendar object.  In this situation, each VTIMEZONE
  MUST represent a unique time zone definition.  This is necessary
  for some classes of events, such as airline flights, that start in
  one time zone and end in another.

  The VTIMEZONE calendar component MUST include the TZID
  property and at least one definition of a STANDARD or DAYLIGHT
  sub-component.  The STANDARD or DAYLIGHT sub-component MUST
  include the DTSTART, TZOFFSETFROM, and TZOFFSETTO
  properties.

  An individual VTIMEZONE calendar component MUST be specified for
  each unique TZID parameter value specified in the iCalendar
  object.  In addition, a VTIMEZONE calendar component, referred
  to by a recurring calendar component, MUST provide valid time zone
  information for all recurrence instances.

  Each VTIMEZONE calendar component consists of a collection of
  one or more sub-components that describe the rule for a particular
  observance (either a Standard Time or a Daylight Saving Time
  observance).  The STANDARD sub-component consists of a
  collection of properties that describe Standard Time.  The
  DAYLIGHT sub-component consists of a collection of properties
  that describe Daylight Saving Time.  In general, this collection
  of properties consists of:

  *  the first onset DATE-TIME for the observance;

  *  the last onset DATE-TIME for the observance, if a last onset is
     known;

  *  the offset to be applied for the observance;

  *  a rule that describes the day and time when the observance
     takes effect;

  *  an optional name for the observance.

  For a given time zone, there may be multiple unique definitions of
  the observances over a period of time.  Each observance is
  described using either a STANDARD or DAYLIGHT sub-component.
  The collection of these sub-components is used to describe the
  time zone for a given period of time.  The offset to apply at any
  given time is found by locating the observance that has the last
  onset date and time before the time in question, and using the
  offset value from that observance.

  The top-level properties in a VTIMEZONE calendar component are:

  The mandatory TZID property is a text value that uniquely
  identifies the VTIMEZONE calendar component within the scope of
  an iCalendar object.

  The optional LAST-MODIFIED property is a UTC value that
  specifies the date and time that this time zone definition was
  last updated.

  The optional TZURL property is a url value that points to a
  published VTIMEZONE definition.  TZURL SHOULD refer to a
  resource that is accessible by anyone who might need to interpret
  the object.  This SHOULD NOT normally be a file URL or other URL
  that is not widely accessible.

  The collection of properties that are used to define the
  STANDARD and DAYLIGHT sub-components include:

  The mandatory DTSTART property gives the effective onset date
  and local time for the time zone sub-component definition.
  DTSTART in this usage MUST be specified as a date with a local
  time value.

  The mandatory TZOFFSETFROM property gives the UTC offset that is
  in use when the onset of this time zone observance begins.
  TZOFFSETFROM is combined with DTSTART to define the effective
  onset for the time zone sub-component definition.  For example,
  the following represents the time at which the observance of
  Standard Time took effect in Fall 1967 for New York City:

   DTSTART:19671029T020000

   TZOFFSETFROM:-0400

  The mandatory TZOFFSETTO property gives the UTC offset for the
  time zone sub-component (Standard Time or Daylight Saving Time)
  when this observance is in use.

  The optional TZNAME property is the customary name for the time
  zone.  This could be used for displaying dates.

  The onset DATE-TIME values for the observance defined by the time
  zone sub-component is defined by the DTSTART, RRULE, and
  RDATE properties.

  The RRULE property defines the recurrence rule for the onset of
  the observance defined by this time zone sub-component.  Some
  specific requirements for the usage of RRULE for this purpose
  include:

  *  If observance is known to have an effective end date, the
     UNTIL recurrence rule parameter MUST be used to specify the
     last valid onset of this observance (i.e., the UNTIL DATE-TIME
     will be equal to the last instance generated by the recurrence
     pattern).  It MUST be specified in UTC time.

  *  The DTSTART and the TZOFFSETFROM properties MUST be used
     when generating the onset DATE-TIME values (instances) from the
     RRULE.

  The RDATE property can also be used to define the onset of the
  observance by giving the individual onset date and times.  RDATE
  in this usage MUST be specified as a date with local time value,
  relative to the UTC offset specified in the TZOFFSETFROM
  property.

  The optional COMMENT property is also allowed for descriptive
  explanatory text.

See UID
See LAST-MODIFICATION
See URL
See COMPONENT
See COMPONENT-CONTAINER")

  (type alarm
    "Component to describe an alert or alarm that should be sent out.

Since there are three distinct grouping of properties for alarms in a
calendar, in iClendar the alarms are split into three distinct subclasses:

  AUDIO-ALARM
  DISPLAY-ALARM
  EMAIL-ALARM

You should use these classes instead of the ALARM superclass.

From RFC5545:
  A VALARM calendar component is a grouping of
  component properties that is a reminder or alarm for an event or a
  to-do.  For example, it may be used to define a reminder for a
  pending event or an overdue to-do.

  The VALARM calendar component MUST include the ACTION and
  TRIGGER properties.  The ACTION property further constrains
  the VALARM calendar component in the following ways:

  When the action is AUDIO, the alarm can also include one and
  only one ATTACH property, which MUST point to a sound resource,
  which is rendered when the alarm is triggered.

  When the action is DISPLAY, the alarm MUST also include a
  DESCRIPTION property, which contains the text to be displayed
  when the alarm is triggered.

  When the action is EMAIL, the alarm MUST include a DESCRIPTION
  property, which contains the text to be used as the message body,
  a SUMMARY property, which contains the text to be used as the
  message subject, and one or more ATTENDEE properties, which
  contain the email address of attendees to receive the message.  It
  can also include one or more ATTACH properties, which are
  intended to be sent as message attachments.  When the alarm is
  triggered, the email message is sent.

  The VALARM calendar component MUST only appear within either a
  VEVENT or VTODO calendar component.  VALARM calendar
  components cannot be nested.  Multiple mutually independent
  VALARM calendar components can be specified for a single
  VEVENT or VTODO calendar component.

  The TRIGGER property specifies when the alarm will be triggered.
  The TRIGGER property specifies a duration prior to the start of
  an event or a to-do.  The TRIGGER edge may be explicitly set to
  be relative to the START or END of the event or to-do with the
  RELATED parameter of the TRIGGER property.  The TRIGGER
  property value type can alternatively be set to an absolute
  calendar date with UTC time.

  In an alarm set to trigger on the START of an event or to-do,
  the DTSTART property MUST be present in the associated event or
  to-do.  In an alarm in a VEVENT calendar component set to
  trigger on the END of the event, either the DTEND property
  MUST be present, or the DTSTART and DURATION properties MUST
  both be present.  In an alarm in a VTODO calendar component set
  to trigger on the END of the to-do, either the DUE property
  MUST be present, or the DTSTART and DURATION properties MUST
  both be present.

  The alarm can be defined such that it triggers repeatedly.  A
  definition of an alarm with a repeating trigger MUST include both
  the DURATION and REPEAT properties.  The DURATION property
  specifies the delay period, after which the alarm will repeat.
  The REPEAT property specifies the number of additional
  repetitions that the alarm will be triggered.  This repetition
  count is in addition to the initial triggering of the alarm.  Both
  of these properties MUST be present in order to specify a
  repeating alarm.  If one of these two properties is absent, then
  the alarm will not repeat beyond the initial trigger.

  The ACTION property is used within the VALARM calendar
  component to specify the type of action invoked when the alarm is
  triggered.  The VALARM properties provide enough information for
  a specific action to be invoked.  It is typically the
  responsibility of a Calendar User Agent (CUA) to deliver the
  alarm in the specified fashion.  An ACTION property value of
  AUDIO specifies an alarm that causes a sound to be played to alert
  the user; DISPLAY specifies an alarm that causes a text message to
  be displayed to the user; and EMAIL specifies an alarm that causes
  an electronic email message to be delivered to one or more email
  addresses.

  In an AUDIO alarm, if the optional ATTACH property is included,
  it MUST specify an audio sound resource.  The intention is that
  the sound will be played as the alarm effect.  If an ATTACH
  property is specified that does not refer to a sound resource, or
  if the specified sound resource cannot be rendered (because its
  format is unsupported, or because it cannot be retrieved), then
  the CUA or other entity responsible for playing the sound may
  choose a fallback action, such as playing a built-in default
  sound, or playing no sound at all.

  In a DISPLAY alarm, the intended alarm effect is for the text
  value of the DESCRIPTION property to be displayed to the user.

  In an EMAIL alarm, the intended alarm effect is for an email
  message to be composed and delivered to all the addresses
  specified by the ATTENDEE properties in the VALARM calendar
  component.  The DESCRIPTION property of the VALARM calendar
  component MUST be used as the body text of the message, and the
  SUMMARY property MUST be used as the subject text.  Any ATTACH
  properties in the VALARM calendar component SHOULD be sent as
  attachments to the message.

     Note: Implementations should carefully consider whether they
     accept alarm components from untrusted sources, e.g., when
     importing calendar objects from external sources.  One
     reasonable policy is to always ignore alarm components that the
     calendar user has not set herself, or at least ask for
     confirmation in such a case.

See ACTION
See TRIGGER
See DURATION
See REPEAT
See COMPONENT")

  (function action
    "Accessor to the action of the component.

See ACTION
See ALARM")

  (function trigger
    "Accessor to the trigger of the component.

See TRIGGER
See ALARM")

  (function repeat
    "Accessor to the repeat count of the component.

See REPEAT
See ALARM")

  (type audio-alarm
    "An alarm that plays an audio clip.

See ATTACHMENT
See ALARM")

  (function attachment
    "Accessor to the audio clip that is played for the audio alarm.

See ATTACHMENT
See AUDIO-ALARM")

  (type display-alarm
    "An alarm that displays a note.

See DESCRIPTION
See ALARM")

  (type email-alarm
    "An alarm that sends an email.

See ATTACHMENTS
See ATTENDEE
See DESCRIPTION
See SUMMARY
See ALARM")

  (function attendee
    "Accessor to the attendee of the component.

See ATTENDEE
See EMAIL-ALARM")

  (type time-zone-component
    "This is a child component describing a part of a time zone.

Since time zone components can take on two forms, iClendar implements the
following two subclasses that should be used instead of this:

  TIME-ZONE-STANDARD
  TIME-ZONE-DAYLIGHT

Please see TIME-ZONE for a full description.

See COMMENTS
See START
See RECURRENCE-DATES
See RECURRENCE-RULE
See OFFSET-TO
See OFFSET-FROM
See TZ-NAMES
See TIME-ZONE
See TIME-ZONE-STANDARD
See TIME-ZONE-DAYLIGHT
See COMPONENT")

  (function offset-to
    "Accessor to what this component is offset to.

See OFFSET-TO
See TIME-ZONE-COMPONENT")

  (function offset-from
    "Accessor to what this component is offset from.

See OFFSET-FROM
See TIME-ZONE-COMPONENT")

  (function tz-names
    "Accessor to the list of time zone names of the component.

See TZNAME
See TIME-ZONE-COMPONENT")

  (type time-zone-standard
    "Standard time zone component description.

See TIME-ZONE-COMPONENT")

  (type time-zone-daylight
    "Daylight savings time zone component description.

See TIME-ZONE-COMPONENT")

  (type x-component
    "This is a component to describe X-prefixed, extension components.

See IDENTIFIER
See COMPONENT"))

;; standard-parameters.lisp
(docs:define-docs
  (function alternate-representation
    "This parameter links to an alternate representation of the value's contents.

The value must be TEXT.

From RFC5545:
  This parameter specifies a URI that points to an
  alternate representation for a textual property value.  A property
  specifying this parameter MUST also include a value that reflects
  the default representation of the text value.  The URI parameter
  value MUST be specified in a quoted-string.

    Note: While there is no restriction imposed on the URI schemes
    allowed for this parameter, Content Identifier (CID) [RFC2392],
    HTTP [RFC2616], and HTTPS [RFC2818] are the URI schemes most
    commonly used by current implementations.

See COMMENT
See CONTACT
See DESCRIPTION
See LOCATION
See RESOURCE
See SUMMARY
See TEXT")

  (function common-name
    "This parameter defines a common name between the property and a user.

The value must be TEXT.

From RFC5545:
  This parameter can be specified on properties with a
  CAL-ADDRESS value type.  The parameter specifies the common name
  to be associated with the calendar user specified by the property.
  The parameter value is text.  The parameter value can be used for
  display text to be associated with the calendar address specified
  by the property.

See ATTENDEE
See ORGANIZER
See TEXT")

  (function calendar-user-type
    "This parameter defines the type of user of the property.

The value must be a string or one of :INDIVIDUAL :GROUP :RESOURCE :ROOM :UNKNOWN.

From RF5545:
  This parameter can be specified on properties with a
  CAL-ADDRESS value type.  The parameter identifies the type of
  calendar user specified by the property.  If not specified on a
  property that allows this parameter, the default is INDIVIDUAL.
  Applications MUST treat x-name and iana-token values they don't
  recognize the same way as they would the UNKNOWN value.

See ATTENDEE")

  (function delegator
    "This parameter defines users who have delegated the property.

The value must be an ADDRESS-LIST.

From RFC5545:
  This parameter can be specified on properties with a
  CAL-ADDRESS value type.  This parameter specifies those calendar
  users that have delegated their participation in a group-scheduled
  event or to-do to the calendar user specified by the property.
  The individual calendar address parameter values MUST each be
  specified in a quoted-string.

See ATTENDEE
See ADDRESS-LIST")

  (function delegatee
    "This parameter defines users to whom this property has been delegated.

The value must be an ADDRESS-LIST.

From RFC5545:
  This parameter can be specified on properties with a
  CAL-ADDRESS value type.  This parameter specifies those calendar
  users whom have been delegated participation in a group-scheduled
  event or to-do by the calendar user specified by the property.
  The individual calendar address parameter values MUST each be
  specified in a quoted-string.

See ATTENDEE
See ADDRESS-LIST")

  (function directory-entry
    "This parameter associates a directory with the property.

The value must be a URI.

From RFC5545:
  This parameter can be specified on properties with a
  CAL-ADDRESS value type.  The parameter specifies a reference to
  the directory entry associated with the calendar user specified by
  the property.  The parameter value is a URI.  The URI parameter
  value MUST be specified in a quoted-string.

     Note: While there is no restriction imposed on the URI schemes
     allowed for this parameter, CID [RFC2392], DATA [RFC2397], FILE
     [RFC1738], FTP [RFC1738], HTTP [RFC2616], HTTPS [RFC2818], LDAP
     [RFC4516], and MID [RFC2392] are the URI schemes most commonly
     used by current implementations.

See ATTENDEE
See ORGANIZER
See URI")

  (function encoding
    "This parameter defines the encoding of the property value.

Must be one of :8BIT :BASE64.

From RFC5545:
  This property parameter identifies the inline encoding
  used in a property value.  The default encoding is \"8BIT\",
  corresponding to a property value consisting of text.  The
  \"BASE64\" encoding type corresponds to a property value encoded
  using the \"BASE64\" encoding defined in [RFC2045].

  If the value type parameter is \";VALUE=BINARY\", then the inline
  encoding parameter MUST be specified with the value
  \";ENCODING=BASE64\".

See ATTACHMENT")

  (function format-type
    "This parameter defines the MIME type of the property value.

Must be a mime type as a string.

From RFC5545:
  This parameter can be specified on properties that are
  used to reference an object.  The parameter specifies the media
  type [RFC4288] of the referenced object.  For example, on the
  ATTACH property, an FTP type URI value does not, by itself,
  necessarily convey the type of content associated with the
  resource.  The parameter value MUST be the text for either an
  IANA-registered media type or a non-standard media type.

See ATTACHMENT")

  (function free/busy-type
    "This parameter decides whether the free/busy-duration property is free or busy.

Must be a string or one of the following: :FREE :BUSY :BUSY-UNAVAILABLE :BUSY-TENTATIVE.

From RFC5545:
  This parameter specifies the free or busy time type.
  The value FREE indicates that the time interval is free for
  scheduling.  The value BUSY indicates that the time interval is
  busy because one or more events have been scheduled for that
  interval.  The value BUSY-UNAVAILABLE indicates that the time
  interval is busy and that the interval can not be scheduled.  The
  value BUSY-TENTATIVE indicates that the time interval is busy
  because one or more events have been tentatively scheduled for
  that interval.  If not specified on a property that allows this
  parameter, the default is BUSY.  Applications MUST treat x-name
  and iana-token values they don't recognize the same way as they
  would the BUSY value.

See FREE/BUSY-PERIOD")

  (function language
    "This parameter defines the language of the property's value.

Must be a LANGUAGE.

From RFC5545:
  This parameter identifies the language of the text in
  the property value and of all property parameter values of the
  property.  The value of the LANGUAGE property parameter is that
  defined in [RFC5646].

  For transport in a MIME entity, the Content-Language header field
  can be used to set the default language for the entire body part.
  Otherwise, no default language is assumed.

See ATTENDEE
See CATEGORIES
See COMMENT
See CONTACT
See DESCRIPTION
See LOCATION
See ORGANIZER
See RESOURCE
See SUMMARY
See TZNAME
See LANGUAGE")

  (function membership
    "This parameter identifies people who are a member of the property.

Must be an ADDRESS-LIST.

From RFC5545:
  This parameter can be specified on properties with a
  CAL-ADDRESS value type.  The parameter identifies the groups or
  list membership for the calendar user specified by the property.
  The parameter value is either a single calendar address in a
  quoted-string or a COMMA-separated list of calendar addresses,
  each in a quoted-string.  The individual calendar address
  parameter values MUST each be specified in a quoted-string.

See ATTENDEE
See ADDRESS-LIST")

  (function participation-status
    "This parameter describes the status of a participation.

Must be a string or one of :NEEDS-ACTION :ACCEPTED :DECLINED :TENTATIVE
:DELEGATED :COMPLETED :IN-PROCESS.

From RFC5545:
  This parameter can be specified on properties with a
  CAL-ADDRESS value type.  The parameter identifies the
  participation status for the calendar user specified by the
  property value.  The parameter values differ depending on whether
  they are associated with a group-scheduled VEVENT, VTODO, or
  VJOURNAL.  The values MUST match one of the values allowed for
  the given calendar component.  If not specified on a property that
  allows this parameter, the default value is NEEDS-ACTION.
  Applications MUST treat x-name and iana-token values they don't
  recognize the same way as they would the NEEDS-ACTION value.

See ATTENDEE")

  (function recurrence-identifier-range
    "This parameter describes the range the recurrence property affects.

Must be :THIS-AND-FUTURE.

From RFC5545:
  This parameter can be specified on a property that
  specifies a recurrence identifier.  The parameter specifies the
  effective range of recurrence instances that is specified by the
  property.  The effective range is from the recurrence identifier
  specified by the property.  If this parameter is not specified on
  an allowed property, then the default range is the single instance
  specified by the recurrence identifier value of the property.  The
  parameter value can only be \"THISANDFUTURE\" to indicate a range
  defined by the recurrence identifier and all subsequent instances.
  The value \"THISANDPRIOR\" is deprecated by this revision of
  iCalendar and MUST NOT be generated by applications.")

  (function trigger-on
    "This parameter describes when the trigger should be applied.

Must be one of :START :END.

From RFC5545:
  This parameter can be specified on properties that
  specify an alarm trigger with a \"DURATION\" value type.  The
  parameter specifies whether the alarm will trigger relative to the
  start or end of the calendar component.  The parameter value START
  will set the alarm to trigger off the start of the calendar
  component; the parameter value END will set the alarm to trigger
  off the end of the calendar component.  If the parameter is not
  specified on an allowable property, then the default is START.

See TRIGGER")

  (function relationship-type
    "This parameter describes the type of relationship this property has to another calendar.

Must be a string or one of :PARENT :CHILD :SIBLING.

From RFC5545:
  This parameter can be specified on a property that
  references another related calendar.  The parameter specifies the
  hierarchical relationship type of the calendar component
  referenced by the property.  The parameter value can be PARENT, to
  indicate that the referenced calendar component is a superior of
  calendar component; CHILD to indicate that the referenced calendar
  component is a subordinate of the calendar component; or SIBLING
  to indicate that the referenced calendar component is a peer of
  the calendar component.  If this parameter is not specified on an
  allowable property, the default relationship type is PARENT.
  Applications MUST treat x-name and iana-token values they don't
  recognize the same way as they would the PARENT value.

See RELATED")

  (function role
    "This parameter describes the role of the property's value in relation to the component.

Must be a string or one of :CHAIR :REQ-PARTICIPANT :OPT-PARTICIPANT :NON-PARTICIPANT.

From RFC5545:
  This parameter can be specified on properties with a
  CAL-ADDRESS value type.  The parameter specifies the participation
  role for the calendar user specified by the property in the group
  schedule calendar component.  If not specified on a property that
  allows this parameter, the default value is REQ-PARTICIPANT.
  Applications MUST treat x-name and iana-token values they don't
  recognize the same way as they would the REQ-PARTICIPANT value.

See ATTENDEE")

  (function reply-requested
    "This parameter describes whether a reply has been requested of the property's users.

Must be a BOOLEAN.

From RFC5545:
  This parameter can be specified on properties with a
  CAL-ADDRESS value type.  The parameter identifies the expectation
  of a reply from the calendar user specified by the property value.
  This parameter is used by the \"Organizer\" to request a
  participation status reply from an \"Attendee\" of a group-scheduled
  event or to-do.  If not specified on a property that allows this
  parameter, the default value is FALSE.

See ATTENDEE")

  (function sent-by
    "This parameter describes who this property was sent by.

Must be an ADDRESS.

From RFC5545:
  This parameter can be specified on properties with a
  CAL-ADDRESS value type.  The parameter specifies the calendar user
  that is acting on behalf of the calendar user specified by the
  property.  The parameter value MUST be a mailto URI as defined in
  [RFC2368].  The individual calendar address parameter values MUST
  each be specified in a quoted-string.

See ATTENDEE
See ORGANIZER
See ADDRESS")

  (function time-zone-identifier
    "This parameter describes a time zone identifier for the property's value.

Must be a TEXT.

From RFC5545:
  This parameter MUST be specified on the DTSTART,
  DTEND, DUE, EXDATE, and RDATE properties when either a
  DATE-TIME or TIME value type is specified and when the value is
  neither a UTC or a floating time.  Refer to the DATE-TIME or
  TIME value type definition for a description of UTC and floating
  time formats.  This property parameter specifies a text value
  that uniquely identifies the VTIMEZONE calendar component to be
  used when evaluating the time portion of the property.  The value
  of the TZID property parameter will be equal to the value of the
  TZID property for the matching time zone definition.  An
  individual VTIMEZONE calendar component MUST be specified for
  each unique TZID parameter value specified in the iCalendar
  object.

  The parameter MUST be specified on properties with a DATE-TIME
  value if the DATE-TIME is not either a UTC or a floating time.
  Failure to include and follow VTIMEZONE definitions in iCalendar
  objects may lead to inconsistent understanding of the local time
  at any given location.

  The presence of the SOLIDUS character as a prefix, indicates that
  this TZID represents a unique ID in a globally defined time zone
  registry (when such registry is defined).

     Note: This document does not define a naming convention for
     time zone identifiers.  Implementers may want to use the naming
     conventions defined in existing time zone specifications such
     as the public-domain TZ database [TZDB].  The specification of
     globally unique time zone identifiers is not addressed by this
     document and is left for future study.

  The following are examples of this property parameter:

   DTSTART;TZID=America/New_York:19980119T020000

   DTEND;TZID=America/New_York:19980119T030000

  The TZID property parameter MUST NOT be applied to DATE
  properties and DATE-TIME or TIME properties whose time values are
  specified in UTC.

  The use of local time in a DATE-TIME or TIME value without the
  TZID property parameter is to be interpreted as floating time,
  regardless of the existence of VTIMEZONE calendar components in
  the iCalendar object.

  For more information, see the sections on the value types DATE-
  TIME and TIME.

See DUE
See END
See EXCEPTION-DATE
See RECURRENCE-ID
See RECURRENCE-DATE
See START
See TRIGGER
See TEXT")

  (function value-type
    "This parameter explicitly defines the type of the property's value.

Must be a string or one of :BINARY :BOOLEAN :CAL-ADDRESS :DATE :DATE-TIME
:DURATION :FLOAT :INTEGER :PERIOD :RECUR :TEXT :TIME :URI :UTC-OFFSET.

From RFC5545:
  This parameter specifies the value type and format of
  the property value.  The property values MUST be of a single value
  type.  For example, a RDATE property cannot have a combination
  of DATE-TIME and TIME value types.

  If the property's value is the default value type, then this
  parameter need not be specified.  However, if the property's
  default value type is overridden by some other allowable value
  type, then this parameter MUST be specified.

  Applications MUST preserve the value data for x-name and iana-
  token values that they don't recognize without attempting to
  interpret or parse the value data.

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
    "

See PROPERTY")

  (type attachment
    "

See PROPERTY
See ENCODING
See VALUE-TYPE
See FORMAT-TYPE")

  (type attendee
    "

See PROPERTY
See LANGUAGE
See CALENDAR-USER-TYPE
See MEMBERSHIP
See ROLE
See PARTICIPATION-STATUS
See REPLY-REQUESTED
See DELEGATEE
See DELEGATOR
See SENT-BY
See COMMON-NAME
See DIRECTORY-ENTRY")

  (type category
    "

See PROPERTY
See LANGUAGE")

  (type classification
    "

See PROPERTY")

  (type comment
    "

See PROPERTY
See ALTERNATE-REPRESENTATION
See LANGUAGE")

  (type completed
    "

See PROPERTY")

  (type completeness
    "

See PROPERTY")

  (type contact
    "

See PROPERTY
See ALTERNATE-REPRESENTATION
See LANGUAGE")

  (type created
    "

See PROPERTY")

  (type description
    "

See PROPERTY
See ALTERNATE-REPRESENTATION
See LANGUAGE")

  (type due
    "

See PROPERTY
See VALUE-TYPE
See TIME-ZONE-IDENTIFIER")

  (type duration
    "

See PROPERTY")

  (type end
    "

See PROPERTY
See VALUE-TYPE
See TIME-ZONE-IDENTIFIER")

  (type exception-date
    "

See PROPERTY
See VALUE-TYPE
See TIME-ZONE-IDENTIFIER")

  (type free/busy-period
    "

See PROPERTY
See FREE/BUSY-TYPE")

  (type geographic-location
    "

See PROPERTY")

  (type last-modification
    "

See PROPERTY")

  (type location
    "

See PROPERTY
See ALTERNATE-REPRESENTATION
See LANGUAGE")

  (type offset-from
    "

See PROPERTY")

  (type offset-to
    "

See PROPERTY")

  (type organizer
    "

See PROPERTY
See LANGUAGE
See COMMON-NAME
See DIRECTORY-ENTRY
See SENT-BY")

  (type priority
    "

See PROPERTY")

  (type product
    "

See PROPERTY")

  (type recurrence-id
    "

See PROPERTY
See VALUE-TYPE
See TIME-ZONE-IDENTIFIER
See RECURRENCE-IDENTIFIER-RANGE")

  (type recurrence-date
    "

See PROPERTY
See VALUE-TYPE
See TIME-ZONE-IDENTIFIER")

  (type recurrence-rule
    "

See PROPERTY")

  (type related
    "

See PROPERTY
See RELATIONSHIP-TYPE")

  (type repeat
    "

See PROPERTY")

  (type request-status
    "

See PROPERTY")

  (type resource
    "

See PROPERTY
See ALTERNATE-REPRESENTATION
See LANGUAGE")

  (type scale
    "

See PROPERTY")

  (type sequence-number
    "

See PROPERTY")

  (type stamp
    "

See PROPERTY")

  (type start
    "

See PROPERTY
See VALUE-TYPE
See TIME-ZONE-IDENTIFIER")

  (type status
    "

See PROPERTY")

  (type summary
    "

See PROPERTY
See ALTERNATE-REPRESENTATION
See LANGUAGE")

  (type transparency
    "

See PROPERTY")

  (type transport-method
    "

See PROPERTY")

  (type trigger
    "

See PROPERTY
See VALUE-TYPE
See TIME-ZONE-IDENTIFIER
See TRIGGER-ON")

  (type tzid
    "

See PROPERTY")

  (type tzname
    "

See PROPERTY
See LANGUAGE")

  (type tzurl
    "

See PROPERTY")

  (type uid
    "

See PROPERTY")

  (type url
    "

See PROPERTY")

  (type version
    "

See PROPERTY"))

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
