#|
 This file is a part of iclendar
 (c) 2018 Shirakumo http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package #:org.shirakumo.iclendar)

(defun intern* (&rest parts)
  (let ((*print-case* (readtable-case *readtable*)))
    (intern (format NIL "~{~a~^-~}" parts)
            #.*package*)))

(defun props-p (initargs &rest choices)
  (loop with null = '#:null
        for choice in choices
        for value = (getf initargs choice null)
        do (unless (eql value null) (return T))))

(defun ensure-finalized (class)
  (unless (c2mop:class-finalized-p class)
    (c2mop:finalize-inheritance class))
  class)

(defun find-direct-slot (name class)
  (find name (c2mop:class-direct-slots class) :key #'c2mop:slot-definition-name))

(defun find-superclass-slot (name class)
  (loop for super in (c2mop:class-direct-superclasses class)
        for found = (find name (c2mop:class-slots (ensure-finalized super))
                          :key #'c2mop:slot-definition-name)
        when found return found))

(defclass serializable-class (c2mop:metaobject)
  ((identifier :reader identifier)))

(defmethod shared-initialize ((class serializable-class) slot-names &key (identifier NIL given))
  (declare (ignore slot-names))
  (call-next-method)
  (when given
    (setf (slot-value class 'identifier) (if (consp identifier) (car identifier) identifier))))

(defmethod c2mop:finalize-inheritance :after ((class serializable-class))
  (unless (slot-boundp class 'identifier)
    (setf (slot-value class 'identifier)
          (loop for super in (c2mop:class-direct-superclasses class)
                do (when (c2mop:subclassp super 'serializable-class)
                     (unless (c2mop:class-finalized-p super)
                       (c2mop:finalize-inheritance super))
                     (return (slot-value class 'identifier)))))))

(defclass parameter-class (standard-class serializable-class)
  ((type :reader parameter-type))
  (:default-initargs :type 'text))

(defmethod initialize-instance :after ((class parameter-class) &key (type NIL given))
  (when given
    (setf (slot-value class 'type) (if (consp type) (car type) type))))

(defmethod c2mop:finalize-inheritance :after ((class parameter-class))
  (unless (slot-boundp class 'type)
    (setf (slot-value class 'type)
          (loop for super in (c2mop:class-direct-superclasses class)
                do (when (c2mop:subclassp super 'parameter-class)
                     (return (slot-value (ensure-finalized super) 'type)))))))

(defmethod c2mop:validate-superclass ((class parameter-class) (super standard-class)) T)
(defmethod c2mop:validate-superclass ((class parameter-class) (super parameter-class)) T)
(defmethod c2mop:validate-superclass ((class standard-class) (super parameter-class)) NIL)

(defclass parameter ()
  ((value :initarg :value :accessor value))
  (:metaclass parameter-class))

(defmethod (setf c2mop:slot-value-using-class) :before (value (class parameter-class) (parameter parameter) slot-definition)
  (when (eql 'value (c2mop:slot-definition-name slot-definition))
    (unless (typep value (parameter-type class))
      (error "The value ~s of property parameter ~s is not of type ~s."
             value (class-name class) (parameter-type class)))))

(defmacro define-parameter (name direct-superclasses &rest options)
  `(defclass ,name (,@direct-superclasses parameter)
     ()
     (:metaclass parameter-class)
     ,@options))

(defclass property-slot-class (standard-class serializable-class)
  ())

(defmethod c2mop:validate-superclass ((class property-slot-class) (super standard-class)) T)
(defmethod c2mop:validate-superclass ((class property-slot-class) (super property-slot-class)) T)
(defmethod c2mop:validate-superclass ((class standard-class) (super property-slot-class)) NIL)

(defclass property-definition (c2mop:standard-slot-definition serializable-class)
  ((requirement :initarg :requirement :reader requirement)
   (parameters :initarg :parameters :reader parameters))
  (:default-initargs
   :type 'text
   :requirement :optional
   :parameters ()))

(defclass direct-property-definition (c2mop:standard-direct-slot-definition property-definition)
  ())

(defclass effective-property-definition (c2mop:standard-effective-slot-definition property-definition)
  ())

(defmethod c2mop:direct-slot-definition-class ((class property-slot-class) &rest args)
  (if (props-p args :identifier :requirement)
      (find-class 'direct-property-definition)
      (call-next-method)))

(defmethod c2mop:effective-slot-definition-class ((class property-slot-class) &rest args)
  (if (or (typep (find-direct-slot (getf args :name) class) 'direct-property-definition)
          ;; Make sure property definition slots propagate.
          (typep (find-superclass-slot (getf args :name) class) 'effective-property-definition))
      (find-class 'effective-property-definition)
      (call-next-method)))

(defmethod c2mop:compute-effective-slot-definition ((class property-slot-class) identifier direct-slots)
  (let ((effective-slot (call-next-method)))
    (when (typep effective-slot 'effective-property-definition)
      (let ((super-slot (find-superclass-slot (c2mop:slot-definition-name effective-slot) class))
            (direct-slot (find-direct-slot (c2mop:slot-definition-name effective-slot) class)))
        (labels ((copy-slot (from identifier)
                   (when (slot-boundp from identifier)
                     (setf (slot-value effective-slot identifier) (slot-value from identifier))))
                 (copy-slots (from)
                   (copy-slot from 'identifier)
                   (copy-slot from 'requirement)
                   (setf (slot-value effective-slot 'parameters)
                         (loop for parameter in (slot-value from 'parameters)
                               for class = (find-class parameter)
                               if (c2mop:subclassp class (find-class 'parameter))
                               collect class
                               else do (error "~s is not a parameter class." parameter)))))
          ;; Inherit first
          (when super-slot (copy-slots super-slot))
          ;; Then override
          (when direct-slot (copy-slots direct-slot)))))
    effective-slot))

(defclass property ()
  ()
  (:metaclass property-slot-class))

(defmacro define-property ((name identifier) &body options)
  `(defclass ,(intern* name 'property) (property)
     ((,name ,@options
             :identifier ,identifier
             :initarg ,(intern (string name) :keyword)
             :reader ,name))
     (:metaclass property-slot-class)))

(defclass block ()
  ((parameters :initform (make-hash-table :test 'eql) :reader parameters))
  (:metaclass property-slot-class))

(defmethod check-properties-valid ((block block))
  (let ((slots (c2mop:class-slots (class-of block))))
    (dolist (slot slots)
      (when (typep slot 'effective-property-definition)
        (let ((name (c2mop:slot-definition-name slot)))
          (labels ((initarg (&optional (name name))
                     (first (c2mop:slot-definition-initargs (find name slots :key #'c2mop:slot-definition-name))))
                   (check-slot-value (&key (value (slot-value block name)) (default T))
                     (let ((type (or (c2mop:slot-definition-type slot) default)))
                       (unless (typep value type)
                         (error "The value ~s for ~s is not of type ~s." value (initarg) type)))))
            (etypecase (requirement slot)
              ((eql :required)
               (unless (slot-boundp block name)
                 (error "The property ~s is required." (initarg)))
               (check-slot-value))
              ((eql :optional)
               (when (slot-boundp block name)
                 (check-slot-value :type '(not cons))))
              ((eql :multiple)
               (when (slot-boundp block name)
                 (dolist (value (slot-value block name))
                   (check-slot-value :value value))))
              (symbol
               (when (slot-boundp block name)
                 (unless (slot-boundp block (requirement slot))
                   (error "The property ~s requires ~s to be set as well."
                          (initarg) (initarg (requirement slot))))
                 (check-slot-value)))
              (cons
               (when (slot-boundp block name)
                 (when (slot-boundp block (second (requirement slot)))
                   (error "The property ~s does not allow ~s to be set as well."
                          (initarg) (initarg (second (requirement slot)))))
                 (check-slot-value))))))))))

(defmethod shared-initialize :after ((block block) slots &key)
  (declare (ignore slots))
  (check-properties-valid block)
  (dolist (slot (c2mop:class-slots (class-of block)))
    (when (typep slot 'effective-property-definition)
      (setf (gethash (c2mop:slot-definition-name slot) (parameters block))
            (mapcar #'make-instance (parameters slot))))))

(defmethod identifier ((block block))
  (identifier (class-of block)))

(defmethod property-parameters ((block block) (property symbol))
  (gethash property (parameters block)))

(defmethod property-parameter ((block block) (property symbol) (parameter symbol))
  (let ((parameters (property-parameters block property)))
    (value (or (find parameter parameters :key (lambda (parameter) (class-name (class-of parameter))))
               (error "The property ~s has no parameter ~s." property parameter)))))

(defmethod (setf property-parameter) (value (block block) (property symbol) (parameter symbol))
  (let ((parameters (property-parameters block property)))
    (setf (value (or (find parameter parameters :key (lambda (parameter) (class-name (class-of parameter))))
                     (error "The property ~s has no parameter ~s." property parameter)))
          value)))

(defmacro define-block (name direct-superclasses properties direct-slots &rest options)
  `(defclass ,name (,@direct-superclasses block
                    ,@(loop for property in properties
                            collect (intern* property 'property)))
     ,direct-slots
     (:metaclass property-slot-class)
     ,@options))
