#|
 This file is a part of iclendar
 (c) 2018 Shirakumo http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package #:org.shirakumo.iclendar)

(define-parameter alternate-representation ()
  (:identifier "ALTREP"))

(define-parameter common-name ()
  (:identifier "CN"))

(define-parameter calendar-user-type ()
  (:type (or string (member :individual :group :resource :room :unknown)))
  (:identifier "CUTYPE"))

(define-parameter delegator ()
  (:type address-list)
  (:identifier "DELEGATED-FROM"))

(define-parameter delegatee ()
  (:type address-list)
  (:identifier "DELEGATED-TO"))

(define-parameter directory-entry ()
  (:type uri)
  (:identifier "DIR"))

(define-parameter encoding ()
  (:type (member :8bit :base64))
  (:identifier "ENCODING"))

(define-parameter format-type ()
  (:type cons)
  (:identifier "FMTTYPE"))

(define-parameter free/busy-type ()
  (:type (or string (member :free :busy :busy-unavailable :busy-tentative)))
  (:identifier "FBTYPE"))

(define-parameter language ()
  (:type language)
  (:identifier "LANGUAGE"))

(define-parameter membership ()
  (:type address-list)
  (:identifier "MEMBER"))

(define-parameter participation-status ()
  (:identifier "PARTSTAT"))

(define-parameter participation-status-event (participation-status)
  (:type (or string (member :needs-action :accepted :declined :tentative :delegated))))

(define-parameter participation-status-todo (participation-status)
  (:type (or string (member :needs-action :accepted :declined :tentative :delegated :completed :in-process))))

(define-parameter participation-status-journal (participation-status)
  (:type (or string (member :needs-action :accepted :declined))))

(define-parameter recurrence-identifier-range ()
  (:type (member :this-and-future))
  (:identifier "RANGE"))

(define-parameter trigger-on ()
  (:type (member :start :end))
  (:identifier "RELATED"))

(define-parameter relationship-type ()
  (:type (or string (member :parent :child :sibling)))
  (:identifier "RELTYPE"))

(define-parameter role ()
  (:type (or string (member :chair :req-participant :opt-participant :non-participant)))
  (:identifier "ROLE"))

(define-parameter reply-requested ()
  (:type boolean)
  (:identifier "RSVP"))

(define-parameter sent-by ()
  (:type address)
  (:identifier "SENT-BY"))

(define-parameter time-zone-identifier ()
  (:identifier "TZID"))

(define-parameter value-type ()
  (:type (or string (member :binary :boolean :cal-address :date :date-time :duration :float :integer :period :recur :text :time :uri :utc-offset)))
  (:identifier "VALUE"))
