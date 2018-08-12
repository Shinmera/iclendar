## About iClendar
This is a library implementing the iCalendar file format specification according to [RFC5545](https://tools.ietf.org/html/rfc5545).

No, the name is not a typo.

## How To
After loading the library, you can assemble calendar components like you would create any other class instance:

    (make-instance 'iclendar:calendar
      :product "iClendar"
      :components (list (make-instance 'iclendar:event
                          :start (iclendar:make-date :year 2018 :month 8 :date 12)
                          :description "Today iClendar is released.")))

If you forget a required property, or use an invalid value, it will signal an error informing you of what's wrong. iClendar is very strict in its data constraints, which should mean that it is not possible to generate invalid data using it.

Once you have your objects assembled, you can serialise them to stream, string, or file using `serialize`:

    (iclendar:serialize *)
    ; => BEGIN:VCALENDAR^M
    ;    PRODID:iClendar^M
    ;    VERSION:2.0^M
    ;    BEGIN:VEVENT^M
    ;    DTSTAMP:20180812T154447Z^M
    ;    DTSTART:20180812^M
    ;    UID:CCDF2E5C-D741-AD74-1F3B-A9DC01397D8D^M
    ;    DESCRIPTION:Today iClendar is released.^M
    ;    END:VEVENT^M
    ;    END:VCALENDAR^M

The Return/CR characters have been visualised as `^M` here.

The iCalendar format allows you to associate metadata parameters with various properties. iClendar supports this by wrapping each property value of a component in an appropriate `property` instance. This instance holds both the actual value and the metadata parameters. Thus, adding metadata to a property merely requires setting the property's value, and then accessing it like so:

    (iclendar:description (first (iclendar:components *)))

This will return an instance of `description`, one of the mentioned containers. You can now set the metadata slots on it:

    (setf (iclendar:language *) "DE")

Once this is done, the serialisation will include the metadata parameter in its output:

    (iclendar:serialize ***)
    ; => BEGIN:VCALENDAR^M
    ;    PRODID:iClendar^M
    ;    VERSION:2.0^M
    ;    BEGIN:VEVENT^M
    ;    DTSTAMP:20180812T161148Z^M
    ;    DTSTART:20180812^M
    ;    UID:EA6DA4AE-DAC4-ADAC-8807-1969E0E0D4AD^M
    ;    DESCRIPTION;LANGUAGE="DE":Today iClendar is released.^M
    ;    END:VEVENT^M
    ;    END:VCALENDAR^M

Parameters are, just like property values, strictly type checked.

A special note must be made for properties that contain multiple values. In that case, the slot on the component will return a list of `property` instances. Pushing to that slot, or setting its value in any way, will automatically ensure that it contains a list of the appropriate `property` instances. For example, you can easily add comments like this:

    (push "Ha ha what a story, Mark" (iclendar:comments (first (iclendar:components *))))

You can also remove multiple-value property values in the same way you would handle a list. Properties that are not multiple-value can be unset as normal by using `slot-makunbound`.

## Parsing iCalendar
Currently iClendar does not support parsing `ics` files.

## Extension / Internal Organisation
### Serializable-Class
Each of the following classes is a subclass of `serializable-class`, which merely keeps an `identifier` class slot that corresponds to the iCalendar format's identifier. For instance, the `calendar`'s identifier is `VCALENDAR`.

### Parameters
Parameters are modelled as subclasses of a `parameter-direct-slot` class and thus describe custom slot types. These slot types are then used in `property` definitions, where they ensure that the parameter metadata has the correct type. You can define new parameters using `define-parameter`, or by simply individually adding them to the `x-parameters` slot on a `property` instance.

### Properties
Properties are modelled as classes that have a `value` slot, and a slot for each of the supported parameters. They also contain a slot called `x-parameters` with a table for custom, non-standard parameters. You can define new property types with `define-property`, which takes `:type` and `:parameters` extra options to define that behaviour.

You can get an alist of all parameters of a property using `parameters`.

### Components
A component contains a number of properties, potential sub-components, and a slot called `x-properties` for extraneous, non-standard properties. Each property is modelled as a slot that maintains relational constraints of the property within the component. You can define additional components with `define-component`. A slot in a component definition is turned into a property-slot if the initarg `:property` is present. If it is, the initarg's value must be the name of a `property` subclass. A property-slot also takes the `:constraint` initarg, which describes whether it is `:required`, `:optional`, or `:multiple` value. The constraint may also be one of `(not NAME)` or `(and NAME)` if the property may not be or must be set alongside another.

You can get a list of all properties of a component using `properties`.

## Serialisation
Serialisation of components, properties, and their types is handled by the generic function `serialize-object`. The stream that is passed to it should automatically ensure the proper line-folding behaviour iCalendar expects, and make sure `terpri` calls produce the proper `CRLF` output. When writing to this stream, Returns are ignored, and Linefeeds are turned into "content new lines", meaning the literal characters `\` and `n`. 

When writing textual values to the stream, you should output the string using `serialize-object` as well, so that it can properly escape backslashes, semi-colons, and commas.

Typically, if you merely create new parameters, properties, and components, you will not need to touch any of these functions as the standard behaviour is generic enough to cover your bases. However, if you add a new value type, you will most definitely need to add an appropriate `serialize-object` method for it.
