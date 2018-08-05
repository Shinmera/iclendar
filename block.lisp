#|
 This file is a part of iclendar
 (c) 2018 Shirakumo http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package #:org.shirakumo.iclendar)

(defclass block-class (standard-class)
  ((name :initarg :name :reader name)))

(defmethod c2mop:validate-superclass ((class block-class) (super standard-class)) T)
(defmethod c2mop:validate-superclass ((class block-class) (super block-class)) T)
(defmethod c2mop:validate-superclass ((class standard-class) (super block-class)) NIL)

(defclass property-definition (c2mop:standard-slot-definition)
  ((name :initarg :name :reader name)
   (requirement :initarg :requirement :reader requirement)))

(defclass direct-property-definition (c2mop:standard-direct-slot-definition property-definition)
  ())

(defmethod initialize-instance :after ((slot direct-property-definition) &key (name NIL name-p) requirement)
  (declare (ignore name))
  (unless name-p (error "NAME required."))
  (unless requirement (setf (slot-value slot 'requirement) :optional)))

(defclass effective-property-definition (c2mop:standard-effective-slot-definition property-definition)
  ())

(defmethod c2mop:direct-slot-definition-class ((class block-class) &rest args)
  (declare (ignore args))
  (find-class 'direct-property-definition))

(defmethod c2mop:effective-slot-definition-class ((class block-class) &rest args)
  (declare (ignore args))
  (find-class 'effective-property-definition))

(defmethod c2mop:compute-effective-slot-definition ((class block-class) name direct-slots)
  (let ((effective-slot (call-next-method)))
    (dolist (direct-slot direct-slots)
      (flet ((copy-slot (name)
               (setf (slot-value effective-slot name)
                     (slot-value direct-slot name))))
        (when (and (typep direct-slot 'direct-property-definition)
                   (eql (c2mop:slot-definition-name direct-slot)
                        (c2mop:slot-definition-name effective-slot)))
          (copy-slot 'name)
          (copy-slot 'requirement)
          (return))))
    effective-slot))

(defclass block ()
  ()
  (:metaclass block-class))

(defmethod check-properties-valid ((block block))
  (dolist (slot (c2mop:class-slots (class-of block)))
    (when (typep slot 'effective-property-definition)
      (let ((value (slot-value block (c2mop:slot-definition-name slot))))
        (etypecase (requirement slot)
          ((eql :required) (check-type value (and (not null) (not cons))))
          ((eql :optional) (check-type value (or null (not cons))))
          ((eql :multiple) (check-type value list))
          (symbol (when value
                    (assert (slot-value block (requirement slot)))))
          (cons (when value
                  (assert (null (slot-value block (second (requirement slot))))))))))))

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
