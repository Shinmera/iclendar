(in-package #:org.shirakumo.iclendar)

;; protocol.lisp
(docs:define-docs
  (function define-parameter
    "Define a new property parameter.

The body may contain a plist of additional information. Currently the following
keys are recognised:

  :TYPE     --- The type of the slot's value. Defaults to TEXT

A parameter will automatically push a reader of the same name, a writer of
the name (setf NAME), and an initarg of the name but in keyword place.

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

Returns the object that was passed, unless the output is NIL.

See SERIALIZE-OBJECT")

  (function serialize-object
    "Serialize the object to the given stream.

This function has methods for each of the standard-types, as well as for the
generic property and component classes defined by the iCalendar standard. If
you need special behaviour for the serialisation of a new type, property, or
component, you should add appropriate methods to this function.

Always returns the object parameter.

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

The required STAMP and UID fields are automatically filled with default values:

  STAMP -- (ICLENDAR:MAKE-DATE-TIME)
  UID   -- (ICLENDAR::MAKE-UID)

MAKE-UID returns a random UUID-like string.

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
See CREATED
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

Must be a TEXT.

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
See TEXT")

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
    "Describes the action that should be taken when the alarm is signalled.

From RFC5545:
  Each VALARM calendar component has a particular type
  of action with which it is associated.  This property specifies
  the type of action.  Applications MUST ignore alarms with x-name
  and iana-token values they don't recognize.

See PROPERTY")

  (type attachment
    "This property describes a file attachment to the component.

Must be an ATTACHMENT-VALUE

From RFC5545:
  This property is used in VEVENT, VTODO, and
  VJOURNAL calendar components to associate a resource (e.g.,
  document) with the calendar component.  This property is used in
  VALARM calendar components to specify an audio sound resource or
  an email message attachment.  This property can be specified as a
  URI pointing to a resource or as inline binary encoded content.

  When this property is specified as inline binary encoded content,
  calendar applications MAY attempt to guess the media type of the
  resource via inspection of its content if and only if the media
  type of the resource is not given by the FMTTYPE parameter.  If
  the media type remains unknown, calendar applications SHOULD treat
  it as type \"application/octet-stream\".

See PROPERTY
See ENCODING
See VALUE-TYPE
See FORMAT-TYPE
See ATTACHMENT-VALUE")

  (type attendee
    "This property describes a person who attends the calendar's event.

Must be an ADDRESS.

From RFC5545:
  This property MUST only be specified within calendar
  components to specify participants, non-participants, and the
  chair of a group-scheduled calendar entity.  The property is
  specified within an EMAIL category of the VALARM calendar
  component to specify an email address that is to receive the email
  type of iCalendar alarm.

  The property parameter CN is for the common or displayable name
  associated with the calendar address; ROLE, for the intended
  role that the attendee will have in the calendar component;
  PARTSTAT, for the status of the attendee's participation;
  RSVP, for indicating whether the favor of a reply is requested;
  CUTYPE, to indicate the type of calendar user; MEMBER, to
  indicate the groups that the attendee belongs to; DELEGATED-TO,
  to indicate the calendar users that the original request was
  delegated to; and DELEGATED-FROM, to indicate whom the request
  was delegated from; SENT-BY, to indicate whom is acting on
  behalf of the ATTENDEE; and DIR, to indicate the URI that
  points to the directory information corresponding to the attendee.
  These property parameters can be specified on an ATTENDEE
  property in either a VEVENT, VTODO, or VJOURNAL calendar
  component.  They MUST NOT be specified in an ATTENDEE property
  in a VFREEBUSY or VALARM calendar component.  If the

  LANGUAGE property parameter is specified, the identified
  language applies to the CN parameter.

  A recipient delegated a request MUST inherit the RSVP and ROLE
  values from the attendee that delegated the request to them.

  Multiple attendees can be specified by including multiple
  ATTENDEE properties within the calendar component.

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
See DIRECTORY-ENTRY
See ADDRESS")

  (type category
    "This property describes a category of the component.

Must be a TEXT.

From RFC5545:
  This property is used to specify categories or subtypes
  of the calendar component.  The categories are useful in searching
  for a calendar component of a particular type and category.
  Within the VEVENT, VTODO, or VJOURNAL calendar components,
  more than one category can be specified as a COMMA-separated list
  of categories.

See PROPERTY
See LANGUAGE
See TEXT")

  (type classification
    "This property describes the classification/confidentiality of an entry.

Must be a string or one of :PUBLIC :PRIVATE :CONFIDENTIAL.

From RFC5545:
  An access classification is only one component of the
  general security system within a calendar application.  It
  provides a method of capturing the scope of the access the
  calendar owner intends for information within an individual
  calendar entry.  The access classification of an individual
  iCalendar component is useful when measured along with the other
  security components of a calendar system (e.g., calendar user
  authentication, authorization, access rights, access role, etc.).
  Hence, the semantics of the individual access classifications
  cannot be completely defined by this memo alone.  Additionally,
  due to the blind nature of most exchange processes using this
  memo, these access classifications cannot serve as an enforcement
  statement for a system receiving an iCalendar object.  Rather,
  they provide a method for capturing the intention of the calendar
  owner for the access to the calendar component.  If not specified
  in a component that allows this property, the default value is
  PUBLIC.  Applications MUST treat x-name and iana-token values they
  don't recognize the same way as they would the PRIVATE value.

See PROPERTY")

  (type comment
    "This property is used to specify a comment to the calendar user.

Must be a TEXT.

See PROPERTY
See ALTERNATE-REPRESENTATION
See LANGUAGE
See TEXT")

  (type completed
    "This property defines the date and time that a to-do was actually completed.

Must be a DATE-TIME.

See PROPERTY
See DATE-TIME")

  (type completeness
    "This property describes the percentage of completeness of the component.

Must be an integer in [0,100].

From RFC5545:
  The property value is a positive integer between 0 and
  100.  A value of 0 indicates the to-do has not yet been started.
  A value of 100 indicates that the to-do has been completed.
  Integer values in between indicate the percent partially complete.

  When a to-do is assigned to multiple individuals, the property
  value indicates the percent complete for that portion of the to-do
  assigned to the assignee or delegatee.  For example, if a to-do is
  assigned to both individuals A and B.  A reply from A with a
  percent complete of 70 indicates that A has completed 70% of
  the to-do assigned to them.  A reply from B with a percent
  complete of 50 indicates B has completed 50% of the to-do
  assigned to them.

See PROPERTY")

  (type contact
    "This property describes a representative contact person.

Must be a TEXT.

From RFC5545:
  The property value consists of textual contact
  information.  An alternative representation for the property value
  can also be specified that refers to a URI pointing to an
  alternate form, such as a vCard [RFC2426], for the contact
  information.

See PROPERTY
See ALTERNATE-REPRESENTATION
See LANGUAGE
See TEXT")

  (type created
    "This property describes the creation time of the component.

Must be a DATE-TIME.

From RFC5545:
  This property specifies the date and time that the
  calendar information was created by the calendar user agent in the
  calendar store.

See PROPERTY
See DATE-TIME")

  (type description
    "This property describes a human-readable description of the component.

Must be a TEXT.

From RFC5545:
  This property is used in the VEVENT and VTODO to
  capture lengthy textual descriptions associated with the activity.

  This property is used in the VJOURNAL calendar component to
  capture one or more textual journal entries.

  This property is used in the VALARM calendar component to
  capture the display text for a DISPLAY category of alarm, and to
  capture the body text for an EMAIL category of alarm.


See PROPERTY
See ALTERNATE-REPRESENTATION
See LANGUAGE
See TEXT")

  (type due
    "This property describes a due date for the component.

Must be a DATE-TIME or a DATE.

From RFC5545:
  This property defines the date and time before which a
  to-do is expected to be completed.  For cases where this property
  is specified in a VTODO calendar component that also specifies a
  DTSTART property, the value type of this property MUST be the
  same as the DTSTART property, and the value of this property
  MUST be later in time than the value of the DTSTART property.
  Furthermore, this property MUST be specified as a date with local
  time if and only if the DTSTART property is also specified as a
  date with local time.

See PROPERTY
See VALUE-TYPE
See TIME-ZONE-IDENTIFIER
See DATE
See DATE-TIME")

  (type duration
    "This property describes a runtime duration of the component.

Must be a TIME-SPAN.

From RFC5545:
  In a VEVENT calendar component the property may be
  used to specify a duration of the event, instead of an explicit
  end DATE-TIME.  In a VTODO calendar component the property may
  be used to specify a duration for the to-do, instead of an
  explicit due DATE-TIME.  In a VALARM calendar component the
  property may be used to specify the delay period prior to
  repeating an alarm.  When the DURATION property relates to a
  DTSTART property that is specified as a DATE value, then the
  DURATION property MUST be specified as a dur-day or dur-week
  value.

See PROPERTY
See TIME-SPAN")

  (type end
    "This property describes an end date for the component.

Must be a DATE-TIME or a DATE.

From RFC5545:
  Within the VEVENT calendar component, this property
  defines the date and time by which the event ends.  The value type
  of this property MUST be the same as the DTSTART property, and
  its value MUST be later in time than the value of the DTSTART
  property.  Furthermore, this property MUST be specified as a date
  with local time if and only if the DTSTART property is also
  specified as a date with local time.

  Within the VFREEBUSY calendar component, this property defines
  the end date and time for the free or busy time information.  The
  time MUST be specified in the UTC time format.  The value MUST be
  later in time than the value of the DTSTART property.

See PROPERTY
See VALUE-TYPE
See TIME-ZONE-IDENTIFIER
See DATE-TIME
See DATE")

  (type exception-date
    "This property describes an exceptional date on which the component does not recur.

Must be a DATE-TIME or a DATE.

From RFC5545:
   The exception dates, if specified, are used in
  computing the recurrence set.  The recurrence set is the complete
  set of recurrence instances for a calendar component.  The
  recurrence set is generated by considering the initial DTSTART
  property along with the RRULE, RDATE, and EXDATE properties
  contained within the recurring component.  The DTSTART property
  defines the first instance in the recurrence set.  The DTSTART
  property value SHOULD match the pattern of the recurrence rule, if
  specified.  The recurrence set generated with a DTSTART property
  value that doesn't match the pattern of the rule is undefined.
  The final recurrence set is generated by gathering all of the
  start DATE-TIME values generated by any of the specified RRULE
  and RDATE properties, and then excluding any start DATE-TIME
  values specified by EXDATE properties.  This implies that start
  DATE-TIME values specified by EXDATE properties take precedence
  over those specified by inclusion properties (i.e., RDATE and
  RRULE).  When duplicate instances are generated by the RRULE
  and RDATE properties, only one recurrence is considered.
  Duplicate instances are ignored.

  The EXDATE property can be used to exclude the value specified
  in DTSTART.  However, in such cases, the original DTSTART date
  MUST still be maintained by the calendaring and scheduling system
  because the original DTSTART value has inherent usage
  dependencies by other properties such as the RECURRENCE-ID.

See PROPERTY
See VALUE-TYPE
See TIME-ZONE-IDENTIFIER
See DATE-TIME
See DATE")

  (type free/busy-period
    "This property describes a free or busy time period.

Must be a PERIOD.

From RFC5545:
  These time periods can be specified as either a start
  and end DATE-TIME or a start DATE-TIME and DURATION.  The date and
  time MUST be a UTC time format.

  FREEBUSY properties within the VFREEBUSY calendar component
  SHOULD be sorted in ascending order, based on start time and then
  end time, with the earliest periods first.

  The FREEBUSY property can specify more than one value, separated
  by the COMMA character.  In such cases, the FREEBUSY property
  values MUST all be of the same FBTYPE property parameter type
  (e.g., all values of a particular FBTYPE listed together in a
  single property).

See PROPERTY
See FREE/BUSY-TYPE
See PERIOD")

  (type geographic-location
    "This property describes a geographic location where the component takes place.

Must be a GEO.

From RFC5545:
  This property value specifies latitude and longitude,
  in that order (i.e., LAT LON ordering).  The longitude
  represents the location east or west of the prime meridian as a
  positive or negative real number, respectively.  The longitude and
  latitude values MAY be specified up to six decimal places, which
  will allow for accuracy to within one meter of geographical
  position.  Receiving applications MUST accept values of this
  precision and MAY truncate values of greater precision.

  Values for latitude and longitude shall be expressed as decimal
  fractions of degrees.  Whole degrees of latitude shall be
  represented by a two-digit decimal number ranging from 0 through
  90.  Whole degrees of longitude shall be represented by a decimal
  number ranging from 0 through 180.  When a decimal fraction of a
  degree is specified, it shall be separated from the whole number
  of degrees by a decimal point.

  Latitudes north of the equator shall be specified by a plus sign
  (+), or by the absence of a minus sign (-), preceding the digits
  designating degrees.  Latitudes south of the Equator shall be
  designated by a minus sign (-) preceding the digits designating
  degrees.  A point on the Equator shall be assigned to the Northern
  Hemisphere.

  Longitudes east of the prime meridian shall be specified by a plus
  sign (+), or by the absence of a minus sign (-), preceding the
  digits designating degrees.  Longitudes west of the meridian shall
  be designated by minus sign (-) preceding the digits designating
  degrees.  A point on the prime meridian shall be assigned to the
  Eastern Hemisphere.  A point on the 180th meridian shall be
  assigned to the Western Hemisphere.  One exception to this last
  convention is permitted.  For the special condition of describing
  a band of latitude around the earth, the East Bounding Coordinate
  data element shall be assigned the value +180 (180) degrees.

  Any spatial address with a latitude of +90 (90) or -90 degrees
  will specify the position at the North or South Pole,
  respectively.  The component for longitude may have any legal
  value.

  With the exception of the special condition described above, this
  form is specified in [ANSI INCITS 61-1986].

  The simple formula for converting degrees-minutes-seconds into
  decimal degrees is:

  decimal = degrees + minutes/60 + seconds/3600.

See PROPERTY
See GEO")

  (type last-modification
    "This property specifies the date and time that the information associated with the calendar component was last revised in the calendar store.

Must be a DATE-TIME

See PROPERTY
See DATE-TIME")

  (type location
    "This property describes a physical location where the component takes place.

Must be a TEXT.

From RFC5545:
  Specific venues such as conference or meeting rooms may
  be explicitly specified using this property.  An alternate
  representation may be specified that is a URI that points to
  directory information with more structured specification of the
  location.  For example, the alternate representation may specify
  either an LDAP URL [RFC4516] pointing to an LDAP server entry or a
  CID URL [RFC2392] pointing to a MIME body part containing a
  Virtual-Information Card (vCard) [RFC2426] for the location.

See PROPERTY
See ALTERNATE-REPRESENTATION
See LANGUAGE
See TEXT")

  (type offset-from
    "This property describes where the time zone is offset from.

Must be an UTC-OFFSET.

From RFC5545:
  This property specifies the offset that is in use prior
  to this time observance.  It is used to calculate the absolute
  time at which the transition to a given observance takes place.
  This property MUST only be specified in a VTIMEZONE calendar
  component.  A VTIMEZONE calendar component MUST include this
  property.  The property value is a signed numeric indicating the
  number of hours and possibly minutes from UTC.  Positive numbers
  represent time zones east of the prime meridian, or ahead of UTC.
  Negative numbers represent time zones west of the prime meridian,
  or behind UTC.

See PROPERTY
See UTC-OFFSET")

  (type offset-to
    "This property describes where the time zone is offset to.

From RFC5545:
  This property specifies the offset that is in use in
  this time zone observance.  It is used to calculate the absolute
  time for the new observance.  The property value is a signed
  numeric indicating the number of hours and possibly minutes from
  UTC.  Positive numbers represent time zones east of the prime
  meridian, or ahead of UTC.  Negative numbers represent time zones
  west of the prime meridian, or behind UTC.

See PROPERTY
See UTC-OFFSET")

  (type organizer
    "This property describes the organizer of the component.

Must be an ADDRESS.

From RFC5545:
  This property is specified within the VEVENT,
  VTODO, and VJOURNAL calendar components to specify the
  organizer of a group-scheduled calendar entity.  The property is
  specified within the VFREEBUSY calendar component to specify the
  calendar user requesting the free or busy time.  When publishing a
  VFREEBUSY calendar component, the property is used to specify
  the calendar that the published busy time came from.

  The property has the property parameters CN, for specifying the
  common or display name associated with the Organizer, DIR, for
  specifying a pointer to the directory information associated with
  the Organizer, SENT-BY, for specifying another calendar user
  that is acting on behalf of the Organizer.  The non-standard
  parameters may also be specified on this property.  If the
  LANGUAGE property parameter is specified, the identified
  language applies to the CN parameter value.

See PROPERTY
See LANGUAGE
See COMMON-NAME
See DIRECTORY-ENTRY
See SENT-BY
See ADDRESS")

  (type priority
    "This property describes the priority of the component.

Must be an integer in [0,9].

From RFC5545:
  This priority is specified as an integer in the range 0
  to 9.  A value of 0 specifies an undefined priority.  A value of 1
  is the highest priority.  A value of 2 is the second highest
  priority.  Subsequent numbers specify a decreasing ordinal
  priority.  A value of 9 is the lowest priority.

  A CUA with a three-level priority scheme of HIGH, MEDIUM, and
  LOW is mapped into this property such that a property value in
  the range of 1 to 4 specifies HIGH priority.  A value of 5 is
  the normal or MEDIUM priority.  A value in the range of 6 to 9
  is LOW priority.

  A CUA with a priority schema of A1, A2, A3, B1, B2, ...,
  C3 is mapped into this property such that a property value of 1
  specifies A1, a property value of 2 specifies A2, a property
  value of 3 specifies A3, and so forth up to a property value of
  9 specifies C3.

  Other integer values are reserved for future use.

  Within a VEVENT calendar component, this property specifies a
  priority for the event.  This property may be useful when more
  than one event is scheduled for a given time period.

  Within a VTODO calendar component, this property specified a
  priority for the to-do.  This property is useful in prioritizing
  multiple action items for a given time period.

See PROPERTY")

  (type product
    "This property defines the calendar's producer.

Must be a TEXT.

From RFC5545:
  The vendor of the implementation SHOULD assure that
  this is a globally unique identifier; using some technique such as
  an FPI value, as defined in [ISO.9070.1991].

  This property SHOULD NOT be used to alter the interpretation of an
  iCalendar object beyond the semantics specified in this memo.  For
  example, it is not to be used to further the understanding of non-
  standard properties.

See PROPERTY
See TEXT")

  (type recurrence-id
    "This property allows referencing a specific recurrence date.

Must be a DATE-TIME or a DATE.

From RFC5545:
  The full range of calendar components specified by a
  recurrence set is referenced by referring to just the UID
  property value corresponding to the calendar component.  The
  RECURRENCE-ID property allows the reference to an individual
  instance within the recurrence set.

  If the value of the DTSTART property is a DATE type value, then
  the value MUST be the calendar date for the recurrence instance.

  The DATE-TIME value is set to the time when the original
  recurrence instance would occur; meaning that if the intent is to
  change a Friday meeting to Thursday, the DATE-TIME is still set to
  the original Friday meeting.

  The RECURRENCE-ID property is used in conjunction with the UID
  and SEQUENCE properties to identify a particular instance of a
  recurring event, to-do, or journal.  For a given pair of UID and
  SEQUENCE property values, the RECURRENCE-ID value for a
  recurrence instance is fixed.

  The RANGE parameter is used to specify the effective range of
  recurrence instances from the instance specified by the
  RECURRENCE-ID property value.  The value for the range parameter
  can only be THISANDFUTURE to indicate a range defined by the
  given recurrence instance and all subsequent instances.
  Subsequent instances are determined by their RECURRENCE-ID value
  and not their current scheduled start time.  Subsequent instances
  defined in separate components are not impacted by the given
  recurrence instance.  When the given recurrence instance is
  rescheduled, all subsequent instances are also rescheduled by the
  same time difference.  For instance, if the given recurrence
  instance is rescheduled to start 2 hours later, then all
  subsequent instances are also rescheduled 2 hours later.

  Similarly, if the duration of the given recurrence instance is
  modified, then all subsequence instances are also modified to have
  this same duration.

     Note: The RANGE parameter may not be appropriate to
     reschedule specific subsequent instances of complex recurring
     calendar component.  Assuming an unbounded recurring calendar
     component scheduled to occur on Mondays and Wednesdays, the
     RANGE parameter could not be used to reschedule only the
     future Monday instances to occur on Tuesday instead.  In such
     cases, the calendar application could simply truncate the
     unbounded recurring calendar component (i.e., with the COUNT
     or UNTIL rule parts), and create two new unbounded recurring
     calendar components for the future instances.

See PROPERTY
See VALUE-TYPE
See TIME-ZONE-IDENTIFIER
See RECURRENCE-IDENTIFIER-RANGE
See DATE
See DATE-TIME")

  (type recurrence-date
    "This property describes a specific date on which the component recurs.

Must be a DATE-TIME or DATE.

From RFC5545:
  This property can appear along with the RRULE
  property to define an aggregate set of repeating occurrences.
  When they both appear in a recurring component, the recurrence

  instances are defined by the union of occurrences defined by both
  the RDATE and RRULE.

  The recurrence dates, if specified, are used in computing the
  recurrence set.  The recurrence set is the complete set of
  recurrence instances for a calendar component.  The recurrence set
  is generated by considering the initial DTSTART property along
  with the RRULE, RDATE, and EXDATE properties contained
  within the recurring component.  The DTSTART property defines
  the first instance in the recurrence set.  The DTSTART property
  value SHOULD match the pattern of the recurrence rule, if
  specified.  The recurrence set generated with a DTSTART property
  value that doesn't match the pattern of the rule is undefined.
  The final recurrence set is generated by gathering all of the
  start DATE-TIME values generated by any of the specified RRULE
  and RDATE properties, and then excluding any start DATE-TIME
  values specified by EXDATE properties.  This implies that start
  DATE-TIME values specified by EXDATE properties take precedence
  over those specified by inclusion properties (i.e., RDATE and
  RRULE).  Where duplicate instances are generated by the RRULE
  and RDATE properties, only one recurrence is considered.
  Duplicate instances are ignored.

See PROPERTY
See VALUE-TYPE
See TIME-ZONE-IDENTIFIER
See DATE
See DATE-TIME")

  (type recurrence-rule
    "This property describes a generic rule by which the component recurs.

Must be a RECURRENCE.

From RFC5545:
  The recurrence rule, if specified, is used in computing
  the recurrence set.  The recurrence set is the complete set of
  recurrence instances for a calendar component.  The recurrence set
  is generated by considering the initial DTSTART property along
  with the RRULE, RDATE, and EXDATE properties contained
  within the recurring component.  The DTSTART property defines
  the first instance in the recurrence set.  The DTSTART property
  value SHOULD be synchronized with the recurrence rule, if
  specified.  The recurrence set generated with a DTSTART property
  value not synchronized with the recurrence rule is undefined.  The
  final recurrence set is generated by gathering all of the start
  DATE-TIME values generated by any of the specified RRULE and
  RDATE properties, and then excluding any start DATE-TIME values
  specified by EXDATE properties.  This implies that start DATE-
  TIME values specified by EXDATE properties take precedence over
  those specified by inclusion properties (i.e., RDATE and
  RRULE).  Where duplicate instances are generated by the RRULE
  and RDATE properties, only one recurrence is considered.
  Duplicate instances are ignored.

  The DTSTART property specified within the iCalendar object
  defines the first instance of the recurrence.  In most cases, a
  DTSTART property of DATE-TIME value type used with a recurrence
  rule, should be specified as a date with local time and time zone
  reference to make sure all the recurrence instances start at the
  same local time regardless of time zone changes.

  If the duration of the recurring component is specified with the
  DTEND or DUE property, then the same exact duration will apply
  to all the members of the generated recurrence set.  Else, if the
  duration of the recurring component is specified with the
  DURATION property, then the same nominal duration will apply to
  all the members of the generated recurrence set and the exact
  duration of each recurrence instance will depend on its specific
  start time.  For example, recurrence instances of a nominal
  duration of one day will have an exact duration of more or less
  than 24 hours on a day where a time zone shift occurs.  The
  duration of a specific recurrence may be modified in an exception
  component or simply by using an RDATE property of PERIOD value
  type.

See PROPERTY
See RECURRENCE")

  (type related
    "This property defines a related component.

Must be a TEXT.

From RFC5545:
  The property value consists of the persistent, globally
  unique identifier of another calendar component.  This value would
  be represented in a calendar component by the UID property.

  By default, the property value points to another calendar
  component that has a PARENT relationship to the referencing
  object.  The RELTYPE property parameter is used to either
  explicitly state the default PARENT relationship type to the
  referenced calendar component or to override the default PARENT
  relationship type and specify either a CHILD or SIBLING
  relationship.  The PARENT relationship indicates that the calendar
  component is a subordinate of the referenced calendar component.
  The CHILD relationship indicates that the calendar component is a
  superior of the referenced calendar component.  The SIBLING
  relationship indicates that the calendar component is a peer of
  the referenced calendar component.

  Changes to a calendar component referenced by this property can
  have an implicit impact on the related calendar component.  For
  example, if a group event changes its start or end date or time,
  then the related, dependent events will need to have their start
  and end dates changed in a corresponding way.  Similarly, if a
  PARENT calendar component is cancelled or deleted, then there is
  an implied impact to the related CHILD calendar components.  This
  property is intended only to provide information on the
  relationship of calendar components.  It is up to the target
  calendar system to maintain any property implications of this
  relationship.

See PROPERTY
See RELATIONSHIP-TYPE
See TEXT")

  (type repeat
    "This property defines the number of times the component's action is repeated.

Must be a positive integer.

From RFC5545:
  This property defines the number of times an alarm
  should be repeated after its initial trigger.  If the alarm
  triggers more than once, then this property MUST be specified
  along with the DURATION property.

See PROPERTY")

  (type request-status
    "This property describes the status of the component's requested action.

Must be a TEXT.

See PROPERTY
See TEXT")

  (type resource
    "This property defines the equipment or resources anticipated for an activity specified by a calendar component.

Must be a TEXT.

See PROPERTY
See ALTERNATE-REPRESENTATION
See LANGUAGE
See TEXT")

  (type scale
    "This property defines the calendar's scaling.

Must be a TEXT.

From RFC5545:
  This memo is based on the Gregorian calendar scale.
  The Gregorian calendar scale is assumed if this property is not
  specified in the iCalendar object.  It is expected that other
  calendar scales will be defined in other specifications or by
  future versions of this memo.

See PROPERTY
See TEXT")

  (type sequence-number
    "This property defines the sequence number of the component.

Must be a positive integer.

From RFC5545:
  When a calendar component is created, its sequence
  number is 0.  It is monotonically incremented by the \"Organizer's\"
  CUA each time the \"Organizer\" makes a significant revision to the
  calendar component.

  The \"Organizer\" includes this property in an iCalendar object that
  it sends to an \"Attendee\" to specify the current version of the
  calendar component.

  The \"Attendee\" includes this property in an iCalendar object that
  it sends to the \"Organizer\" to specify the version of the calendar
  component to which the \"Attendee\" is referring.

  A change to the sequence number is not the mechanism that an
  \"Organizer\" uses to request a response from the \"Attendees\".  The
  \"RSVP\" parameter on the \"ATTENDEE\" property is used by the
  \"Organizer\" to indicate that a response from the \"Attendees\" is
  requested.

  Recurrence instances of a recurring component MAY have different
  sequence numbers.

See PROPERTY")

  (type stamp
    "This component describes the creation stamp of the component.

Must be a DATE-TIME

From RFC5545:
  The value MUST be specified in the UTC time format.

  This property is also useful to protocols such as [2447bis] that
  have inherent latency issues with the delivery of content.  This
  property will assist in the proper sequencing of messages
  containing iCalendar objects.

  In the case of an iCalendar object that specifies a METHOD
  property, this property differs from the CREATED and LAST-
  MODIFIED properties.  These two properties are used to specify
  when the particular calendar data in the calendar store was
  created and last modified.  This is different than when the
  iCalendar object representation of the calendar service
  information was created or last modified.

  In the case of an iCalendar object that doesn't specify a METHOD
  property, this property is equivalent to the LAST-MODIFIED
  property.

See PROPERTY
See DATE-TIME")

  (type start
    "This property describes the start time of the component.

Must be a DATE-TIME or a DATE.

From RFC5545:
  Within the VEVENT calendar component, this property
  defines the start date and time for the event.

  Within the VFREEBUSY calendar component, this property defines
  the start date and time for the free or busy time information.
  The time MUST be specified in UTC time.

  Within the STANDARD and DAYLIGHT sub-components, this property
  defines the effective start date and time for a time zone
  specification.  This property is REQUIRED within each STANDARD
  and DAYLIGHT sub-components included in VTIMEZONE calendar
  components and MUST be specified as a date with local time without
  the TZID property parameter.

See PROPERTY
See VALUE-TYPE
See TIME-ZONE-IDENTIFIER
See DATE
See DATE-TIME")

  (type status
    "This property defines the overall status or confirmation for the calendar component.

Must be a TEXT.

See PROPERTY
See TEXT")

  (type summary
    "This property defines a short summary or subject for the calendar component.

Must be a TEXT.

From RFC5545:
  This property is used in the VEVENT, VTODO, and
  VJOURNAL calendar components to capture a short, one-line
  summary about the activity or journal entry.

  This property is used in the VALARM calendar component to
  capture the subject of an EMAIL category of alarm.

See PROPERTY
See ALTERNATE-REPRESENTATION
See LANGUAGE
See TEXT")

  (type transparency
    "This property describes whether the component takes up physical time.

Must be a string or one of :OPAQUE :TRANSPARENT.

From RFC5545:
  Time Transparency is the characteristic of an event
  that determines whether it appears to consume time on a calendar.
  Events that consume actual time for the individual or resource
  associated with the calendar SHOULD be recorded as OPAQUE,
  allowing them to be detected by free/busy time searches.  Other
  events, which do not take up the individual's (or resource's) time
  SHOULD be recorded as TRANSPARENT, making them invisible to free/
  busy time searches.

See PROPERTY")

  (type transport-method
    "This property describes the transport method by which the calendar is sent.

Must be a TEXT.

From RFC5545:
  When used in a MIME message entity, the value of this
  property MUST be the same as the Content-Type method parameter
  value.  If either the METHOD property or the Content-Type
  method parameter is specified, then the other MUST also be
  specified.

  No methods are defined by this specification.  This is the subject
  of other specifications, such as the iCalendar Transport-
  independent Interoperability Protocol (iTIP) defined by [2446bis].

  If this property is not present in the iCalendar object, then a
  scheduling transaction MUST NOT be assumed.  In such cases, the
  iCalendar object is merely being used to transport a snapshot of

See PROPERTY
See TEXT")

  (type trigger
    "This property describes the trigger for an alarm.

Must be a TIME-SPAN or a DATE-TIME.

From RFC5545:
  This property defines when an alarm will trigger.  The
  default value type is DURATION, specifying a relative time for the
  trigger of the alarm.  The default duration is relative to the
  start of an event or to-do with which the alarm is associated.
  The duration can be explicitly set to trigger from either the end
  or the start of the associated event or to-do with the RELATED
  parameter.  A value of START will set the alarm to trigger off the
  start of the associated event or to-do.  A value of END will set
  the alarm to trigger off the end of the associated event or to-do.

  Either a positive or negative duration may be specified for the
  TRIGGER property.  An alarm with a positive duration is
  triggered after the associated start or end of the event or to-do.
  An alarm with a negative duration is triggered before the
  associated start or end of the event or to-do.

  The RELATED property parameter is not valid if the value type of
  the property is set to DATE-TIME (i.e., for an absolute date and
  time alarm trigger).  If a value type of DATE-TIME is specified,
  then the property value MUST be specified in the UTC time format.
  If an absolute trigger is specified on an alarm for a recurring
  event or to-do, then the alarm will only trigger for the specified
  absolute DATE-TIME, along with any specified repeating instances.

  If the trigger is set relative to START, then the DTSTART
  property MUST be present in the associated VEVENT or VTODO
  calendar component.  If an alarm is specified for an event with
  the trigger set relative to the END, then the DTEND property or
  the DTSTART and DURATION  properties MUST be present in the
  associated VEVENT calendar component.  If the alarm is specified
  for a to-do with a trigger set relative to the END, then either
  the DUE property or the DTSTART and DURATION  properties
  MUST be present in the associated VTODO calendar component.

  Alarms specified in an event or to-do that is defined in terms of
  a DATE value type will be triggered relative to 00:00:00 of the
  user's configured time zone on the specified date, or relative to
  00:00:00 UTC on the specified date if no configured time zone can
  be found for the user.  For example, if DTSTART is a DATE value
  set to 19980205 then the duration trigger will be relative to
  19980205T000000 America/New_York for a user configured with the
  America/New_York time zone.

See PROPERTY
See VALUE-TYPE
See TIME-ZONE-IDENTIFIER
See TRIGGER-ON
See TIME-SPAN
See DATE-TIME")

  (type tzid
    "This property assigns a unique ID to the time zone to reference it by.

Must be a TEXT.

From RFC5545:
  This is the label by which a time zone calendar
  component is referenced by any iCalendar properties whose value
  type is either DATE-TIME or TIME and not intended to specify a UTC
  or a floating time.  The presence of the SOLIDUS character as a
  prefix, indicates that this TZID represents an unique ID in a
  globally defined time zone registry (when such registry is
  defined).

     Note: This document does not define a naming convention for
     time zone identifiers.  Implementers may want to use the naming
     conventions defined in existing time zone specifications such
     as the public-domain TZ database [TZDB].  The specification of
     globally unique time zone identifiers is not addressed by this
     document and is left for future study.

See PROPERTY
See TEXT")

  (type tzname
    "This property describes a name for the time zone.

Must be a TEXT.

From RFC5545:
  This property specifies a customary name that can be
  used when displaying dates that occur during the observance
  defined by the time zone sub-component.

See PROPERTY
See LANGUAGE
See TEXT")

  (type tzurl
    "This property describes a network location where the time zone is described.

Must be a URI.

From RFC5545:
  This property provides a means for a VTIMEZONE
  component to point to a network location that can be used to
  retrieve an up-to-date version of itself.  This provides a hook to
  handle changes government bodies impose upon time zone
  definitions.  Retrieval of this resource results in an iCalendar
  object containing a single VTIMEZONE component and a METHOD
  property set to PUBLISH.

See PROPERTY
See URI")

  (type uid
    "This property describes a globally unique identifier for the component.

Must be a TEXT.

From RFC5545:
  The UID itself MUST be a globally unique identifier.
  The generator of the identifier MUST guarantee that the identifier
  is unique.  There are several algorithms that can be used to
  accomplish this.  A good method to assure uniqueness is to put the
  domain name or a domain literal IP address of the host on which
  the identifier was created on the right-hand side of an @, and
  on the left-hand side, put a combination of the current calendar
  date and time of day (i.e., formatted in as a DATE-TIME value)
  along with some other currently unique (perhaps sequential)
  identifier available on the system (for example, a process id
  number).  Using a DATE-TIME value on the left-hand side and a
  domain name or domain literal on the right-hand side makes it
  possible to guarantee uniqueness since no two hosts should be
  using the same domain name or IP address at the same time.  Though
  other algorithms will work, it is RECOMMENDED that the right-hand
  side contain some domain identifier (either of the host itself or
  otherwise) such that the generator of the message identifier can
  guarantee the uniqueness of the left-hand side within the scope of
  that domain.

  This is the method for correlating scheduling messages with the
  referenced VEVENT, VTODO, or VJOURNAL calendar component.

  The full range of calendar components specified by a recurrence
  set is referenced by referring to just the UID property value
  corresponding to the calendar component.  The RECURRENCE-ID
  property allows the reference to an individual instance within the
  recurrence set.

  This property is an important method for group-scheduling
  applications to match requests with later replies, modifications,
  or deletion requests.  Calendaring and scheduling applications
  MUST generate this property in VEVENT, VTODO, and VJOURNAL
  calendar components to assure interoperability with other group-
  scheduling applications.  This identifier is created by the
  calendar system that generates an iCalendar object.

  Implementations MUST be able to receive and persist values of at
  least 255 octets for this property, but they MUST NOT truncate
  values in the middle of a UTF-8 multi-octet sequence.

See PROPERTY
See TEXT")

  (type url
    "This property describes a URL where a rendered representation of the component resides.

Must be a URI.

From RFC5545:
  This property may be used in a calendar component to
  convey a location where a more dynamic rendition of the calendar
  information associated with the calendar component can be found.
  This memo does not attempt to standardize the form of the URI, nor
  the format of the resource pointed to by the property value.  If
  the URL property and Content-Location MIME header are both
  specified, they MUST point to the same resource.

See PROPERTY
See URI")

  (type version
    "This property defines the version of the iCalendar protocol.

Must be a TEXT.

From RFC5545:
  This property specifies the identifier corresponding to the
  highest version number or the minimum and maximum range of the
  iCalendar specification that is required in order to interpret the
  iCalendar object.

See PROPERTY
See TEXT"))

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

  (type attachment-value
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

See MAKE-WEEK-DAY-NUM
See WEEK-DAY-NUM-WEEK
See WEEK-DAY-NUM-WEEK-DAY")

  (function make-week-day-num
    "Create a fresh week-day-num instance.

See WEEK-DAY-NUM")

  (function week-day-num-week
    "The number of weeks to recur by.

May be an integer in [-53,53]

See WEEK-DAY-NUM")

  (function week-day-num-week-day
    "The week day on which to recur.

Must be a WEEK-DAY

See WEEK-DAY-NUM
See WEEK-DAY")

  (type date
    "Describes a specific day on the calendar.

See MAKE-DATE
See DATE-YEAR
See DATE-MONTH
See DATE-DATE")

  (function make-date
    "Create a fresh date instance.

See DATE")

  (function date-year
    "The year of the date.

Must be a positive integer.
Defaults to the current UTC year.

See DATE")

  (function date-month
    "The month of the date.

Must be an integer in [1,12].
Defaults to the current UTC month.

See DATE")

  (function date-date
    "The day of the date.

Must be an integer in [1,31].
Defaults to the current UTC date.

See DATE")

  (type date-time
    "Description of a date and time on the calendar.

From RFC5545:
  If the property permits, multiple DATE-TIME values
  are specified as a COMMA-separated list of values.  No additional
  content value encoding (i.e., BACKSLASH character encoding, see
  Section 3.3.11) is defined for this value type.

  The DATE-TIME value type is used to identify values that contain
  a precise calendar date and time of day.  The format is based on
  the [ISO.8601.2004] complete representation, basic format for a
  calendar date and time of day.  The text format is a concatenation
  of the date, followed by the LATIN CAPITAL LETTER T character,
  the time designator, followed by the time format.

  The DATE-TIME value type expresses time values in three forms:

  The form of date and time with UTC offset MUST NOT be used.  For
  example, the following is not valid for a DATE-TIME value:

   19980119T230000-0800       ;Invalid time format

  FORM #1: DATE WITH LOCAL TIME

  The date with local time form is simply a DATE-TIME value that
  does not contain the UTC designator nor does it reference a time
  zone.  For example, the following represents January 18, 1998, at
  11 PM:

   19980118T230000

  DATE-TIME values of this type are said to be floating and are
  not bound to any time zone in particular.  They are used to
  represent the same hour, minute, and second value regardless of
  which time zone is currently being observed.  For example, an
  event can be defined that indicates that an individual will be
  busy from 11:00 AM to 1:00 PM every day, no matter which time zone
  the person is in.  In these cases, a local time can be specified.
  The recipient of an iCalendar object with a property value
  consisting of a local time, without any relative time zone
  information, SHOULD interpret the value as being fixed to whatever
  time zone the ATTENDEE is in at any given moment.  This means
  that two Attendees, in different time zones, receiving the same
  event definition as a floating time, may be participating in the
  event at different actual times.  Floating time SHOULD only be
  used where that is the reasonable behavior.

  In most cases, a fixed time is desired.  To properly communicate a
  fixed time in a property value, either UTC time or local time with
  time zone reference MUST be specified.

  The use of local time in a DATE-TIME value without the TZID
  property parameter is to be interpreted as floating time,
  regardless of the existence of VTIMEZONE calendar components in
  the iCalendar object.

  FORM #2: DATE WITH UTC TIME

  The date with UTC time, or absolute time, is identified by a LATIN
  CAPITAL LETTER Z suffix character, the UTC designator, appended to
  the time value.  For example, the following represents January 19,
  1998, at 0700 UTC:

   19980119T070000Z

  The TZID property parameter MUST NOT be applied to DATE-TIME
  properties whose time values are specified in UTC.

  FORM #3: DATE WITH LOCAL TIME AND TIME ZONE REFERENCE

  The date and local time with reference to time zone information is
  identified by the use the TZID property parameter to reference
  the appropriate time zone definition.  TZID is discussed in
  detail in Section 3.2.19.  For example, the following represents
  2:00 A.M. in New York on January 19, 1998:

   TZID=America/New_York:19980119T020000

  If, based on the definition of the referenced time zone, the local
  time described occurs more than once (when changing from daylight
  to standard time), the DATE-TIME value refers to the first
  occurrence of the referenced time.  Thus, TZID=America/
  New_York:20071104T013000 indicates November 4, 2007 at 1:30 A.M.
  EDT (UTC-04:00).  If the local time described does not occur (when
  changing from standard to daylight time), the DATE-TIME value is
  interpreted using the UTC offset before the gap in local times.
  Thus, TZID=America/New_York:20070311T023000 indicates March 11,
  2007 at 3:30 A.M. EDT (UTC-04:00), one hour after 1:30 A.M. EST
  (UTC-05:00).
  
  A time value MUST only specify the second 60 when specifying a
  positive leap second.  For example:

   19970630T235960Z

  Implementations that do not support leap seconds SHOULD interpret
  the second 60 as equivalent to the second 59.

See MAKE-DATE-TIME
See DATE-TIME-YEAR
See DATE-TIME-MONTH
See DATE-TIME-DATE
See DATE-TIME-HOUR
See DATE-TIME-MINUTE
See DATE-TIME-SECOND
See DATE-TIME-UTC-P")

  (function make-date-time
    "Create a fresh date-time instance.

See DATE-TIME")

  (function date-time-month
    "The month of the date.

Must be an integer in [1,12].
Defaults to the current UTC month.

See DATE-TIME")

  (function date-time-year
    "The year of the date.

Must be a positive integer.
Defaults to the current UTC year.

See DATE-TIME")

  (function date-time-date
    "The day of the date.

Must be an integer in [1,31].
Defaults to the current UTC date.

See DATE-TIME")

  (function date-time-hour
    "The hour of the time.

Must be an integer in [0,23].
Defaults to the current UTC hour.

See DATE-TIME")

  (function date-time-minute
    "The minute of the time.

Must be an integer in [0,59].
Defaults to the current UTC minute.

See DATE-TIME")

  (function date-time-second
    "The second of the time.

Must be an integer in [0,60].
Defaults to the current UTC second.

60 is allowed to account for leap seconds.

See DATE-TIME")

  (function date-time-utc-p
    "Whether the time is in UTC.

Must be a boolean.
Defaults to T.

See DATE-TIME")

  (type time-span
    "Describes a span of time.

From RFC5545:
  If the property permits, multiple duration values are
  specified by a COMMA-separated list of values.  The format is
  based on the [ISO.8601.2004] complete representation basic format
  with designators for the duration of time.  The format can
  represent nominal durations (weeks and days) and accurate
  durations (hours, minutes, and seconds).  Note that unlike
  [ISO.8601.2004], this value type doesn't support the Y and M
  designators to specify durations in terms of years and months.

  The duration of a week or a day depends on its position in the
  calendar.  In the case of discontinuities in the time scale, such
  as the change from standard time to daylight time and back, the
  computation of the exact duration requires the subtraction or
  addition of the change of duration of the discontinuity.  Leap
  seconds MUST NOT be considered when computing an exact duration.
  When computing an exact duration, the greatest order time
  components MUST be added first, that is, the number of days MUST
  be added first, followed by the number of hours, number of
  minutes, and number of seconds.

  Negative durations are typically used to schedule an alarm to
  trigger before an associated time (see Section 3.8.6.3).

  No additional content value encoding (i.e., BACKSLASH character
  encoding, see Section 3.3.11) are defined for this value type.

See MAKE-TIME-SPAN
See TIME-SPAN-WEEK
See TIME-SPAN-HOUR
See TIME-SPAN-MINUTE
See TIME-SPAN-SECOND
See TIME-SPAN-DAY
See TIME-SPAN-INC-P")

  (function make-time-span
    "Create a fresh time-span instance.

See TIME-SPAN")

  (function time-span-week
    "The number of weeks to span.

May be a positive integer.

See TIME-SPAN")

  (function time-span-hour
    "The number of hours to span.

May be a positive integer.

See TIME-SPAN")

  (function time-span-minute
    "The number of minutes to span.

May be a positive integer.

See TIME-SPAN")

  (function time-span-second
    "The number of seconds to span.

May be a positive integer.

See TIME-SPAN")

  (function time-span-day
    "The number of days to span.

May be a positive integer.

See TIME-SPAN")

  (function time-span-inc-p
    "Whether the span is positive or negative.

Must be a boolean. Defaults to T.

See TIME-SPAN")

  (type period
    "Describes a period of time.

From RFC5545:
  If the property permits, multiple period values are
  specified by a COMMA-separated list of values.  There are two
  forms of a period of time.  First, a period of time is identified
  by its start and its end.  This format is based on the
  [ISO.8601.2004] complete representation, basic format for DATE-
  TIME start of the period, followed by a SOLIDUS character
  followed by the DATE-TIME of the end of the period.  The start
  of the period MUST be before the end of the period.  Second, a
  period of time can also be defined by a start and a positive
  duration of time.  The format is based on the [ISO.8601.2004]
  complete representation, basic format for the DATE-TIME start of
  the period, followed by a SOLIDUS character, followed by the
  [ISO.8601.2004] basic format for DURATION of the period.

See MAKE-PERIOD
See PERIOD-START
See PERIOD-LIMIT")

  (function make-period
    "Create a fresh period instance.

See PERIOD")

  (function period-start
    "The start date of the period.

Must be a DATE-TIME.

See PERIOD
See DATE-TIME")

  (function period-limit
    "The limit of the period.

Must be either a DATE-TIME for a specific ending date,
or a TIME-SPAN for a relative duration.

See PERIOD
See DATE-TIME
See TIME-SPAN")

  (type recurrence
    "Describes the rules for a recurring component.

From RFC5545:
  This value type is a structured value consisting of a
  list of one or more recurrence grammar parts.  Each rule part is
  defined by a NAME=VALUE pair.  The rule parts are separated from
  each other by the SEMICOLON character.  The rule parts are not
  ordered in any particular sequence.  Individual rule parts MUST
  only be specified once.  Compliant applications MUST accept rule
  parts ordered in any sequence, but to ensure backward
  compatibility with applications that pre-date this revision of
  iCalendar the FREQ rule part MUST be the first rule part specified
  in a RECUR value.

  The FREQ rule part identifies the type of recurrence rule.  This
  rule part MUST be specified in the recurrence rule.  Valid values
  include SECONDLY, to specify repeating events based on an interval
  of a second or more; MINUTELY, to specify repeating events based
  on an interval of a minute or more; HOURLY, to specify repeating
  events based on an interval of an hour or more; DAILY, to specify
  repeating events based on an interval of a day or more; WEEKLY, to
  specify repeating events based on an interval of a week or more;
  MONTHLY, to specify repeating events based on an interval of a
  month or more; and YEARLY, to specify repeating events based on an
  interval of a year or more.

  The INTERVAL rule part contains a positive integer representing at
  which intervals the recurrence rule repeats.  The default value is
  1, meaning every second for a SECONDLY rule, every minute for a
  MINUTELY rule, every hour for an HOURLY rule, every day for a
  DAILY rule, every week for a WEEKLY rule, every month for a
  MONTHLY rule, and every year for a YEARLY rule.  For example,
  within a DAILY rule, a value of 8 means every eight days.

  The UNTIL rule part defines a DATE or DATE-TIME value that bounds
  the recurrence rule in an inclusive manner.  If the value
  specified by UNTIL is synchronized with the specified recurrence,
  this DATE or DATE-TIME becomes the last instance of the
  recurrence.  The value of the UNTIL rule part MUST have the same
  value type as the DTSTART property.  Furthermore, if the
  DTSTART property is specified as a date with local time, then
  the UNTIL rule part MUST also be specified as a date with local
  time.  If the DTSTART property is specified as a date with UTC
  time or a date with local time and time zone reference, then the
  UNTIL rule part MUST be specified as a date with UTC time.  In the
  case of the STANDARD and DAYLIGHT sub-components the UNTIL
  rule part MUST always be specified as a date with UTC time.  If
  specified as a DATE-TIME value, then it MUST be specified in a UTC
  time format.  If not present, and the COUNT rule part is also not
  present, the RRULE is considered to repeat forever.

  The COUNT rule part defines the number of occurrences at which to
  range-bound the recurrence.  The DTSTART property value always
  counts as the first occurrence.

  The BYSECOND rule part specifies a COMMA-separated list of seconds
  within a minute.  Valid values are 0 to 60.  The BYMINUTE rule
  part specifies a COMMA-separated list of minutes within an hour.
  Valid values are 0 to 59.  The BYHOUR rule part specifies a COMMA-
  separated list of hours of the day.  Valid values are 0 to 23.
  The BYSECOND, BYMINUTE and BYHOUR rule parts MUST NOT be specified
  when the associated DTSTART property has a DATE value type.
  These rule parts MUST be ignored in RECUR value that violate the
  above requirement (e.g., generated by applications that pre-date
  this revision of iCalendar).

  The BYDAY rule part specifies a COMMA-separated list of days of
  the week; SU indicates Sunday; MO indicates Monday; TU indicates
  Tuesday; WE indicates Wednesday; TH indicates Thursday; FR
  indicates Friday; and SA indicates Saturday.

  Each BYDAY value can also be preceded by a positive (+n) or
  negative (-n) integer.  If present, this indicates the nth
  occurrence of a specific day within the MONTHLY or YEARLY RRULE.

  For example, within a MONTHLY rule, +1MO (or simply 1MO)
  represents the first Monday within the month, whereas -1MO
  represents the last Monday of the month.  The numeric value in a
  BYDAY rule part with the FREQ rule part set to YEARLY corresponds
  to an offset within the month when the BYMONTH rule part is
  present, and corresponds to an offset within the year when the
  BYWEEKNO or BYMONTH rule parts are present.  If an integer
  modifier is not present, it means all days of this type within the
  specified frequency.  For example, within a MONTHLY rule, MO
  represents all Mondays within the month.  The BYDAY rule part MUST
  NOT be specified with a numeric value when the FREQ rule part is
  not set to MONTHLY or YEARLY.  Furthermore, the BYDAY rule part
  MUST NOT be specified with a numeric value with the FREQ rule part
  set to YEARLY when the BYWEEKNO rule part is specified.

  The BYMONTHDAY rule part specifies a COMMA-separated list of days
  of the month.  Valid values are 1 to 31 or -31 to -1.  For
  example, -10 represents the tenth to the last day of the month.
  The BYMONTHDAY rule part MUST NOT be specified when the FREQ rule
  part is set to WEEKLY.

  The BYYEARDAY rule part specifies a COMMA-separated list of days
  of the year.  Valid values are 1 to 366 or -366 to -1.  For
  example, -1 represents the last day of the year (December 31st)
  and -306 represents the 306th to the last day of the year (March
  1st).  The BYYEARDAY rule part MUST NOT be specified when the FREQ
  rule part is set to DAILY, WEEKLY, or MONTHLY.

  The BYWEEKNO rule part specifies a COMMA-separated list of
  ordinals specifying weeks of the year.  Valid values are 1 to 53
  or -53 to -1.  This corresponds to weeks according to week
  numbering as defined in [ISO.8601.2004].  A week is defined as a
  seven day period, starting on the day of the week defined to be
  the week start (see WKST).  Week number one of the calendar year
  is the first week that contains at least four (4) days in that
  calendar year.  This rule part MUST NOT be used when the FREQ rule
  part is set to anything other than YEARLY.  For example, 3
  represents the third week of the year.

     Note: Assuming a Monday week start, week 53 can only occur when
     Thursday is January 1 or if it is a leap year and Wednesday is
     January 1.

  The BYMONTH rule part specifies a COMMA-separated list of months
  of the year.  Valid values are 1 to 12.

  The WKST rule part specifies the day on which the workweek starts.
  Valid values are MO, TU, WE, TH, FR, SA, and SU.  This is

  significant when a WEEKLY RRULE has an interval greater than 1,
  and a BYDAY rule part is specified.  This is also significant when
  in a YEARLY RRULE when a BYWEEKNO rule part is specified.  The
  default value is MO.

  The BYSETPOS rule part specifies a COMMA-separated list of values
  that corresponds to the nth occurrence within the set of
  recurrence instances specified by the rule.  BYSETPOS operates on
  a set of recurrence instances in one interval of the recurrence
  rule.  For example, in a WEEKLY rule, the interval would be one
  week A set of recurrence instances starts at the beginning of the
  interval defined by the FREQ rule part.  Valid values are 1 to 366
  or -366 to -1.  It MUST only be used in conjunction with another
  BYxxx rule part.  For example the last work day of the month
  could be represented as:

   FREQ=MONTHLY;BYDAY=MO,TU,WE,TH,FR;BYSETPOS=-1

  Each BYSETPOS value can include a positive (+n) or negative (-n)
  integer.  If present, this indicates the nth occurrence of the
  specific occurrence within the set of occurrences specified by the
  rule.

  Recurrence rules may generate recurrence instances with an invalid
  date (e.g., February 30) or nonexistent local time (e.g., 1:30 AM
  on a day where the local time is moved forward by an hour at 1:00
  AM).  Such recurrence instances MUST be ignored and MUST NOT be
  counted as part of the recurrence set.

  Information, not contained in the rule, necessary to determine the
  various recurrence instance start time and dates are derived from
  the Start Time (DTSTART) component attribute.  For example,
  FREQ=YEARLY;BYMONTH=1 doesn't specify a specific day within the
  month or a time.  This information would be the same as what is
  specified for DTSTART.
  
  BYxxx rule parts modify the recurrence in some manner.  BYxxx rule
  parts for a period of time that is the same or greater than the
  frequency generally reduce or limit the number of occurrences of
  the recurrence generated.  For example, FREQ=DAILY;BYMONTH=1
  reduces the number of recurrence instances from all days (if
  BYMONTH rule part is not present) to all days in January.  BYxxx
  rule parts for a period of time less than the frequency generally
  increase or expand the number of occurrences of the recurrence.
  For example, FREQ=YEARLY;BYMONTH=1,2 increases the number of
  days within the yearly recurrence set from 1 (if BYMONTH rule part
  is not present) to 2.

  If multiple BYxxx rule parts are specified, then after evaluating
  the specified FREQ and INTERVAL rule parts, the BYxxx rule parts
  are applied to the current set of evaluated occurrences in the
  following order: BYMONTH, BYWEEKNO, BYYEARDAY, BYMONTHDAY, BYDAY,
  BYHOUR, BYMINUTE, BYSECOND and BYSETPOS; then COUNT and UNTIL are
  evaluated.

  The table below summarizes the dependency of BYxxx rule part
  expand or limit behavior on the FREQ rule part value.

  The term N/A means that the corresponding BYxxx rule part MUST
  NOT be used with the corresponding FREQ value.

  BYDAY has some special behavior depending on the FREQ value and
  this is described in separate notes below the table.

   +----------+--------+--------+-------+-------+------+-------+------+
   |          |SECONDLY|MINUTELY|HOURLY |DAILY  |WEEKLY|MONTHLY|YEARLY|
   +----------+--------+--------+-------+-------+------+-------+------+
   |BYMONTH   |Limit   |Limit   |Limit  |Limit  |Limit |Limit  |Expand|
   +----------+--------+--------+-------+-------+------+-------+------+
   |BYWEEKNO  |N/A     |N/A     |N/A    |N/A    |N/A   |N/A    |Expand|
   +----------+--------+--------+-------+-------+------+-------+------+
   |BYYEARDAY |Limit   |Limit   |Limit  |N/A    |N/A   |N/A    |Expand|
   +----------+--------+--------+-------+-------+------+-------+------+
   |BYMONTHDAY|Limit   |Limit   |Limit  |Limit  |N/A   |Expand |Expand|
   +----------+--------+--------+-------+-------+------+-------+------+
   |BYDAY     |Limit   |Limit   |Limit  |Limit  |Expand|Note 1 |Note 2|
   +----------+--------+--------+-------+-------+------+-------+------+
   |BYHOUR    |Limit   |Limit   |Limit  |Expand |Expand|Expand |Expand|
   +----------+--------+--------+-------+-------+------+-------+------+
   |BYMINUTE  |Limit   |Limit   |Expand |Expand |Expand|Expand |Expand|
   +----------+--------+--------+-------+-------+------+-------+------+
   |BYSECOND  |Limit   |Expand  |Expand |Expand |Expand|Expand |Expand|
   +----------+--------+--------+-------+-------+------+-------+------+
   |BYSETPOS  |Limit   |Limit   |Limit  |Limit  |Limit |Limit  |Limit |
   +----------+--------+--------+-------+-------+------+-------+------+

  Note 1:  Limit if BYMONTHDAY is present; otherwise, special expand
           for MONTHLY.

  Note 2:  Limit if BYYEARDAY or BYMONTHDAY is present; otherwise,
           special expand for WEEKLY if BYWEEKNO present; otherwise,
           special expand for MONTHLY if BYMONTH present; otherwise,
           special expand for YEARLY.


  Here is an example of evaluating multiple BYxxx rule parts.

   DTSTART;TZID=America/New_York:19970105T083000
   RRULE:FREQ=YEARLY;INTERVAL=2;BYMONTH=1;BYDAY=SU;BYHOUR=8,9;
    BYMINUTE=30

  First, the INTERVAL=2 would be applied to FREQ=YEARLY to
  arrive at every other year.  Then, BYMONTH=1 would be applied
  to arrive at every January, every other year.  Then, BYDAY=SU
  would be applied to arrive at every Sunday in January, every
  other year.  Then, BYHOUR=8,9 would be applied to arrive at
  every Sunday in January at 8 AM and 9 AM, every other year.
  Then, BYMINUTE=30 would be applied to arrive at every Sunday in
  January at 8:30 AM and 9:30 AM, every other year.  Then, lacking
  information from RRULE, the second is derived from DTSTART, to
  end up in every Sunday in January at 8:30:00 AM and 9:30:00 AM,
  every other year.  Similarly, if the BYMINUTE, BYHOUR, BYDAY,
  BYMONTHDAY, or BYMONTH rule part were missing, the appropriate
  minute, hour, day, or month would have been retrieved from the
  DTSTART property.

  If the computed local start time of a recurrence instance does not
  exist, or occurs more than once, for the specified time zone, the
  time of the recurrence instance is interpreted in the same manner
  as an explicit DATE-TIME value describing that date and time, as
  specified in Section 3.3.5.

  No additional content value encoding (i.e., BACKSLASH character
  encoding, see Section 3.3.11) is defined for this value type.

See MAKE-RECURRENCE
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

  (function make-recurrence
    "Create a fresh recurrence instance.

See RECURRENCE")

  (function recurrence-frequency
    "The frequency by which the recurrence happens.

Must be one of :SECONDLY :MINUTELY :HOURLY :DAILY :WEEKLY :MONTHLY :YEARLY.

See RECURRENCE")

  (function recurrence-end-date
    "The end date on which the recurrence is stopped.

May be a DATE or a DATE-TIME.

See RECURRENCE
See DATE
See DATE-TIME")

  (function recurrence-count
    "The number of times the recurrence happens.

May be a positive integer.

See RECURRENCE")

  (function recurrence-interval
    "The interval in which the recurrence happens.

May be a positive integer.

See RECURRENCE")

  (function recurrence-by-seconds
    "A list of seconds by which the recurrence happens.

May be a SECOND-LIST

See RECURRENCE
See SECOND-LIST")

  (function recurrence-by-minutes
    "A list of minutes by which the recurrence happens.

May be a MINUTE-LIST

See RECURRENCE
See MINUTE-LIST")

  (function recurrence-by-hours
    "A list of hours by which the recurrence happens.

May be an HOUR-LIST

See RECURRENCE
See HOUR-LIST")

  (function recurrence-by-days
    "A list of days by which the recurrence happens.

May be a WEEK-DAY-LIST.

See RECURRENCE
See WEEK-DAY-LIST")

  (function recurrence-by-month-days
    "A list of month days by which the recurrence happens.

May be a MONTH-DAY-LIST

See RECURRENCE
See MONTH-DAY-LIST")

  (function recurrence-by-year-days
    "A list of year days by which the recurrence happens.

May be a YEAR-DAY-LIST

See RECURRENCE
See YEAR-DAY-LIST")

  (function recurrence-by-weeks
    "A list of weeks by which the recurrence happens.

May be a WEEK-LIST

See RECURRENCE
See WEEK-LIST")

  (function recurrence-by-months
    "A list of months by which the recurrence happens.

May be a MONTH-LIST

See RECURRENCE
See MONTH-LIST")

  (function recurrence-by-set-pos
    "A list of specific positions by which the recurrence happens.

May be a YEAR-DAY-LIST

See RECURRENCE
See YEAR-DAY-LIST")

  (function recurrence-week-start
    "The week day by which the recurrence starts.

May be a WEEK-DAY

See RECURRENCE
See WEEK-DAY")

  (type uri
    "Type for unified resource identifiers.

This is just a string. No actual checks beyond this are done.

From RFC5545:
  This value type might be used to reference binary
  information, for values that are large, or otherwise undesirable
  to include directly in the iCalendar object.

  Property values with this value type MUST follow the generic URI
  syntax defined in [RFC3986].

  When a property parameter value is a URI value type, the URI MUST
  be specified as a quoted-string value.

  No additional content value encoding (i.e., BACKSLASH character
  encoding, see Section 3.3.11) is defined for this value type.")

  (type utc-offset
    "Representation of a time-zone offset from UTC.

From RFC5545:
  The PLUS SIGN character MUST be specified for positive
  UTC offsets (i.e., ahead of UTC).  The HYPHEN-MINUS character MUST
  be specified for negative UTC offsets (i.e., behind of UTC).  The
  value of -0000 and -000000 are not allowed.  The time-second,
  if present, MUST NOT be 60; if absent, it defaults to zero.

  No additional content value encoding (i.e., BACKSLASH character
  encoding, see Section 3.3.11) is defined for this value type.

See MAKE-UTC-OFFSET
See UTC-OFFSET-HOUR
See UTC-OFFSET-MINUTE
See UTC-OFFSET-SECOND
See UTC-OFFSET-INC-P")

  (function make-utc-offset
    "Create a fresh utc-offset instance.

See UTC-OFFSET")

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

See MAKE-GEO
See GEO-LAT
See GEO-LNG")

  (function make-geo
    "Create a fresh geo instance.

See GEO")

  (function geo-lat
    "Accesses the latitude of the location as a float.

Must be a FLOAT.

See GEO")

  (function geo-lng
    "Accesses the longitude of the location as a float.

Must be a FLOAT.

See GEO")

  (type text
    "Type for text values.

This is just a string."))
