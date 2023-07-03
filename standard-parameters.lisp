(in-package #:org.shirakumo.iclendar)

(define-parameter (alternate-representation "ALTREP"))

(define-parameter (common-name "CN"))

(define-parameter (calendar-user-type "CUTYPE")
  :type (or string (member :individual :group :resource :room :unknown)))

(define-parameter (delegator "DELEGATED-FROM")
  :type address-list)

(define-parameter (delegatee "DELEGATED-TO")
  :type address-list)

(define-parameter (directory-entry "DIR")
  :type uri)

(define-parameter (encoding "ENCODING")
  :type (member :8bit :base64))

(define-parameter (format-type "FMTTYPE")
  :type string)

(define-parameter (free/busy-type "FBTYPE")
  :type (or string (member :free :busy :busy-unavailable :busy-tentative)))

(define-parameter (language "LANGUAGE")
  :type text)

(define-parameter (membership "MEMBER")
  :type address-list)

(define-parameter (participation-status "PARTSTAT")
  :type (or string (member :needs-action :accepted :declined :tentative :delegated :completed :in-process)))

(define-parameter (recurrence-identifier-range "RANGE")
  :type (member :this-and-future))

(define-parameter (trigger-on "RELATED")
  :type (member :start :end))

(define-parameter (relationship-type "RELTYPE")
  :type (or string (member :parent :child :sibling)))

(define-parameter (role "ROLE")
  :type (or string (member :chair :req-participant :opt-participant :non-participant)))

(define-parameter (reply-requested "RSVP")
  :type boolean)

(define-parameter (sent-by "SENT-BY")
  :type address)

(define-parameter (time-zone-identifier "TZID"))

(define-parameter (value-type "VALUE")
  :type (or string (member :binary :boolean :cal-address :date :date-time :duration :float :integer :period :recur :text :time :uri :utc-offset)))
