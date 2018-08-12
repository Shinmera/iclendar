#|
 This file is a part of iclendar
 (c) 2018 Shirakumo http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package #:org.shirakumo.iclendar)

(defun make-uid ()
  (format NIL "~8,'0x-~4,'0x-~4,'0x-~4,'0x-~12,'0x"
          (random #xFFFFFFFF)
          (logand #xFFFF (get-universal-time))
          (logand #xFFFF (ash (get-universal-time) -4))
          (logand #xFFFF (random #xFFFFFFFF))
          (random #xFFFFFFFFFFFF)))

(defun c-day () (nth-value 6 (decode-universal-time (get-universal-time) 0)))
(defun c-year () (nth-value 5 (decode-universal-time (get-universal-time) 0)))
(defun c-month () (nth-value 4 (decode-universal-time (get-universal-time) 0)))
(defun c-date () (nth-value 3 (decode-universal-time (get-universal-time) 0)))
(defun c-hour () (nth-value 2 (decode-universal-time (get-universal-time) 0)))
(defun c-minute () (nth-value 1 (decode-universal-time (get-universal-time) 0)))
(defun c-second () (nth-value 0 (decode-universal-time (get-universal-time) 0)))

(defmacro define-list-type (name inner)
  (let ((predicate (intern (let ((*print-case* (readtable-case *readtable*)))
                             (format NIL "~a-~a" name 'p)))))
    `(progn (defun ,predicate (list)
              (loop for entry in list
                    always (typep entry ',inner)))
            (deftype ,name ()
              '(and list (satisfies ,predicate))))))

(defmacro define-print-object (class identity format-string &rest args)
  (let ((stream (gensym "STREAM")))
    `(defmethod print-object ((,class ,class) ,stream)
       (print-unreadable-object (,class ,stream :type T :identity ,identity)
         (symbol-macrolet ,(loop for arg in args
                                 when (symbolp arg)
                                 collect `(,arg (if (typep ,class 'structure-object)
                                                    (,arg ,class)
                                                    (if (slot-boundp ,class ',arg)
                                                        (,arg ,class)
                                                        '<unbound>))))
           (let ((*print-property-value-only* T))
             (format ,stream ,format-string ,@args)))))))

(defun ensure-finalized (class)
  (let ((class (etypecase class
                 (class class)
                 (symbol (find-class class)))))
    (unless (c2mop:class-finalized-p class)
      (c2mop:finalize-inheritance class))
    class))

(defun find-direct-slot (name class)
  (find name (c2mop:class-direct-slots class) :key #'c2mop:slot-definition-name))

(defun find-superclass-slot (name class)
  (loop for super in (c2mop:class-direct-superclasses class)
        for found = (find name (c2mop:class-slots (ensure-finalized super))
                          :key #'c2mop:slot-definition-name)
        when found return found))
