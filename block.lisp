#|
 This file is a part of iclendar
 (c) 2018 Shirakumo http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package #:org.shirakumo.iclendar)

(defclass block-class (standard-class)
  ((identifier :initarg :identifier :reader identifier)))

(defmethod c2mop:validate-superclass ((class block-class) (super standard-class)) T)
(defmethod c2mop:validate-superclass ((class block-class) (super block-class)) T)
(defmethod c2mop:validate-superclass ((class standard-class) (super block-class)) NIL)

(defclass property-definition (c2mop:standard-slot-definition)
  ((identifier :initarg :identifier :reader identifier)
   (requirement :initarg :requirement :reader requirement)))

(defclass direct-property-definition (c2mop:standard-direct-slot-definition property-definition)
  ())

(defmethod initialize-instance :after ((slot direct-property-definition) &key (identifier NIL i-p) requirement)
  (declare (ignore identifier))
  (unless i-p (error "IDENTIFIER required for ~s" (c2mop:slot-definition-name slot)))
  (unless requirement (setf (slot-value slot 'requirement) :optional)))

(defclass effective-property-definition (c2mop:standard-effective-slot-definition property-definition)
  ())

(defmethod c2mop:direct-slot-definition-class ((class block-class) &rest args)
  (declare (ignore args))
  (find-class 'direct-property-definition))

(defmethod c2mop:effective-slot-definition-class ((class block-class) &rest args)
  (declare (ignore args))
  (find-class 'effective-property-definition))

(defmethod c2mop:compute-effective-slot-definition ((class block-class) identifier direct-slots)
  (let ((effective-slot (call-next-method)))
    (dolist (direct-slot direct-slots)
      (flet ((copy-slot (identifier)
               (setf (slot-value effective-slot identifier)
                     (slot-value direct-slot identifier))))
        (when (and (typep direct-slot 'direct-property-definition)
                   (eql (c2mop:slot-definition-name direct-slot)
                        (c2mop:slot-definition-name effective-slot)))
          (copy-slot 'identifier)
          (copy-slot 'requirement)
          (return))))
    effective-slot))

(defclass block ()
  ()
  (:metaclass block-class))

(defmethod check-properties-valid ((block block))
  (dolist (slot (c2mop:class-slots (class-of block)))
    (when (typep slot 'effective-property-definition)
      (let ((value (slot-value block (c2mop:slot-definition-name slot)))
            (initarg (first (c2mop:slot-definition-initargs slot))))
        (flet ((check-slot-value (value default)
                 (let ((type (or (c2mop:slot-definition-type slot) default)))
                   (unless (typep value type)
                     (error "The value ~s for ~s is not of type ~s." value initarg type)))))
          (etypecase (requirement slot)
            ((eql :required)
             (check-slot-value value '(not null)))
            ((eql :optional)
             (when value
               (check-slot-value value '(not cons))))
            ((eql :multiple)
             (dolist (item value)
               (check-slot-value item T)))
            (symbol
             (when value
               (unless (slot-value block (requirement slot))
                 (error "The parameter ~s requires ~s to be set as well."
                        initarg (requirement slot)))
               (check-slot-value value T)))
            (cons
             (when value
               (when (slot-value block (second (requirement slot)))
                 (error "The parameter ~s does not allow ~s to be set as well."
                        initarg (second (requirement slot))))
               (check-slot-value value T)))))))))

(defmethod shared-initialize :after ((block block) slots &key)
  (declare (ignore slots))
  (check-properties-valid block))

(defmacro define-block (name direct-superclasses direct-slots &rest options)
  `(defclass ,name (,@direct-superclasses block)
     ,(loop for (slot . initargs) in direct-slots
            collect (list* slot :initarg (intern (string slot) :keyword)
                                :initform NIL
                                initargs))
     (:metaclass block-class)
     ,@options))
