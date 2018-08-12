#|
 This file is a part of iclendar
 (c) 2018 Shirakumo http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package #:org.shirakumo.iclendar)

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

(defclass parameter-slot (c2mop:standard-slot-definition serializable-class)
  ())

(defclass direct-parameter-slot (c2mop:standard-direct-slot-definition parameter-slot)
  ())

(defmethod initialize-instance :after ((slot direct-parameter-slot) &key)
  ;; Push default initarg and accessor.
  (let ((name (c2mop:slot-definition-name slot)))
    (unless (c2mop:slot-definition-initargs slot)
      (setf (c2mop:slot-definition-initargs slot) (list (intern (string name) :keyword))))
    (unless (or (c2mop:slot-definition-readers slot)
                (c2mop:slot-definition-writers slot))
      (setf (c2mop:slot-definition-readers slot) (list name))
      (setf (c2mop:slot-definition-writers slot) (list `(setf ,name))))))

(defmacro define-parameter ((name identifier) &body body)
  (destructuring-bind (&key (type 'text))
      body
    `(defclass ,name (direct-parameter-slot)
       ()
       (:default-initargs
        :type ',type
        :identifier ,identifier))))

(defclass effective-parameter-slot (c2mop:standard-effective-slot-definition parameter-slot)
  ())

(defclass property-class (c2mop:standard-class serializable-class)
  ())

(defmethod c2mop:validate-superclass ((class property-class) (super standard-class)) T)
(defmethod c2mop:validate-superclass ((class property-class) (super property-class)) T)
(defmethod c2mop:validate-superclass ((class standard-class) (super property-class)) NIL)

(defmethod c2mop:direct-slot-definition-class ((class property-class) &key name)
  (or (find-class name NIL)
      (call-next-method)))

(defmethod c2mop:effective-slot-definition-class ((class property-class) &key name)
  (if (find-class name NIL)
      (find-class 'effective-parameter-slot)
      (call-next-method)))

(defmethod c2mop:compute-effective-slot-definition ((class property-class) id direct-slots)
  (let ((effective-slot (call-next-method)))
    (when (typep effective-slot 'effective-parameter-slot)
      (let ((super-slot (find-superclass-slot (c2mop:slot-definition-name effective-slot) class))
            (direct-slot (find-direct-slot (c2mop:slot-definition-name effective-slot) class)))
        (labels ((copy-slot (from identifier)
                   (when (slot-boundp from identifier)
                     (setf (slot-value effective-slot identifier) (slot-value from identifier)))))
          ;; Inherit first
          (when super-slot
            (copy-slot super-slot 'identifier))
          ;; Then override
          (when direct-slot
            (copy-slot direct-slot 'identifier)))))
    effective-slot))

(defclass property ()
  ((value :initarg :value :accessor value :type text)
   (x-parameters :initarg :x-parameters :initform (make-hash-table :test 'equalp) :accessor x-parameters))
  (:metaclass property-class))

(defvar *print-property-value-only* NIL)

(defmethod print-object ((property property) stream)
  (cond ((not *print-property-value-only*)
         (print-unreadable-object (property stream :type T :identity T)
           (format stream "~s" (value property))))
        (*print-readably*
         (format stream "~s" (value property)))
        (T
         (format stream "~a" (value property)))))

(defmethod parameters ((property property))
  (append (loop for name being the hash-keys of (x-parameters property)
                for value being the hash-values of (x-parameters property)
                collect (list name value))
          (loop for slot in (c2mop:class-slots (class-of property))
                when (and (typep slot 'parameter-slot)
                          (slot-boundp property (c2mop:slot-definition-name slot)))
                collect (list (identifier slot)
                              (slot-value property (c2mop:slot-definition-name slot))))))

(defmethod identifier ((property property))
  (identifier (class-of property)))

(defmacro define-property ((name identifier) &body body)
  (destructuring-bind (&key (type 'text) parameters) body
    `(defclass ,name (property)
       ((value :type ,type)
        ,@(loop for parameter in parameters
                collect (if (listp parameter) parameter (list parameter))))
       (:metaclass property-class)
       (:identifier ,identifier))))

(defclass property-slot (c2mop:standard-slot-definition)
  ((constraint :initarg :constraint :reader constraint)
   (property-type :initarg :property :reader property-type))
  (:default-initargs
   :constraint :optional))

(defmethod property-slot-value-type ((slot property-slot))
  (dolist (slot (append (c2mop:class-direct-slots (find-class (property-type slot)))
                        (c2mop:class-slots (find-class (property-type slot)))))
    (when (and (eql 'value (c2mop:slot-definition-name slot))
               (c2mop:slot-definition-type slot))
      (return (c2mop:slot-definition-type slot)))))

(defmethod check-slot-value ((slot property-slot) value)
  (unless (or (typep value (property-type slot))
              (typep value (property-slot-value-type slot)))
    (error "The value ~s for ~s is not a ~s or ~s."
           value (c2mop:slot-definition-name slot) (property-type slot) (property-slot-value-type slot))))

(defmethod check-slot-constraint ((slot property-slot) instance)
  (let ((name (c2mop:slot-definition-name slot)))
    (etypecase (constraint slot)
      ((eql :required)
       (unless (slot-boundp instance name)
         (error "The property ~s is required." name)))
      ((eql :optional)
       (when (slot-boundp instance name)
         (unless (typep (slot-value instance name) 'property)
           (error "When the property ~s is set, it must have a property for a value."
                  name))))
      ((eql :multiple)
       (when (slot-boundp instance name)
         (unless (listp (slot-value instance name))
           (error "When the property ~s is set, it must have a list for a value."
                  name))))
      (cons
       (when (slot-boundp instance name)
         (ecase (first (constraint slot))
           (not
            (when (slot-boundp instance (second (constraint slot)))
              (error "The property ~s does not allow ~s to be set as well."
                     name (second (constraint slot)))))
           (and
            (unless (slot-boundp instance (second (constraint slot)))
              (error "The property ~s requires ~s to be set as well."
                     name (constraint slot))))))))))

(defmethod (setf c2mop:slot-value-using-class) :before (value (class standard-class) (object standard-object) (slot property-slot))
  (case (constraint slot)
    (:multiple
     (dolist (item value)
       (check-slot-value slot item)))
    (T
     (check-slot-value slot value))))

(defmethod (setf c2mop:slot-value-using-class) (value (class standard-class) (object standard-object) (slot property-slot))
  (cond ((typep value 'property)
         (call-next-method))
        ((eql :multiple (constraint slot))
         (loop for cons on value
               do (unless (typep (car cons) 'property)
                    (setf (car cons) (make-instance (property-type slot) :value (car cons)))))
         (call-next-method))
        (T
         (setf (c2mop:slot-value-using-class class object slot)
               (make-instance (property-type slot) :value value)))))

(defmethod c2mop:slot-makunbound-using-class :before ((class standard-class) (object standard-object) (slot property-slot))
  (when (eql :required (constraint slot))
    (error "The property ~s is required and cannot be unbound." (c2mop:slot-definition-name slot))))

(defclass direct-property-slot (c2mop:standard-direct-slot-definition property-slot)
  ())

(defmethod initialize-instance :after ((slot direct-property-slot) &key)
  ;; Push default initarg and accessor.
  (let ((name (c2mop:slot-definition-name slot)))
    (unless (c2mop:slot-definition-initargs slot)
      (setf (c2mop:slot-definition-initargs slot) (list (intern (string name) :keyword))))
    (unless (or (c2mop:slot-definition-readers slot)
                (c2mop:slot-definition-writers slot))
      (setf (c2mop:slot-definition-readers slot) (list name))
      (setf (c2mop:slot-definition-writers slot) (list `(setf ,name))))))

(defclass effective-property-slot (c2mop:standard-effective-slot-definition property-slot)
  ())

(defclass component-class (c2mop:standard-class serializable-class)
  ())

(defmethod c2mop:validate-superclass ((class component-class) (super standard-class)) T)
(defmethod c2mop:validate-superclass ((class component-class) (super component-class)) T)
(defmethod c2mop:validate-superclass ((class standard-class) (super component-class)) NIL)

(defmethod c2mop:direct-slot-definition-class ((class component-class) &key property name)
  (if (or property
          (typep (find-superclass-slot name class) 'property-slot))
      (find-class 'direct-property-slot)
      (call-next-method)))

(defmethod c2mop:effective-slot-definition-class ((class component-class) &key name)
  (if (typep (or (find-direct-slot name class)
                 (find-superclass-slot name class))
             'property-slot)
      (find-class 'effective-property-slot)
      (call-next-method)))

(defmethod c2mop:compute-effective-slot-definition ((class component-class) id direct-slots)
  (let ((effective-slot (call-next-method)))
    (when (typep effective-slot 'effective-property-slot)
      (let* ((name (c2mop:slot-definition-name effective-slot))
             (super-slot (find-superclass-slot name class))
             (direct-slot (find-direct-slot name class)))
        (labels ((copy-slot (from identifier)
                   (when (slot-boundp from identifier)
                     (setf (slot-value effective-slot identifier) (slot-value from identifier)))))
          ;; Inherit first
          (when super-slot
            (copy-slot super-slot 'constraint)
            (copy-slot super-slot 'property-type))
          ;; Then override
          (when direct-slot
            (copy-slot direct-slot 'constraint)
            (copy-slot direct-slot 'property-type)))
        ;; Ensure we are valid.
        (unless (find-class (property-type effective-slot) NIL)
          (error "The property spec ~s for slot ~s does not denote a class."
                 (property-type effective-slot) name))
        (unless (c2mop:subclassp (ensure-finalized (property-type effective-slot)) (find-class 'property))
          (error "The property spec ~s for slot ~s does not denote a subclass of ~s."
                 (property-type effective-slot) name 'property))))
    effective-slot))

(defclass component ()
  ((x-properties :initarg :x-properties :initform () :accessor x-properties))
  (:metaclass component-class))

(defmethod shared-initialize :after ((component component) slots &key)
  (declare (ignore slots))
  (dolist (slot (c2mop:class-slots (class-of component)))
    (when (typep slot 'property-slot)
      (check-slot-constraint slot component))))

(defmethod identifier ((component component))
  (identifier (class-of component)))

(defmethod properties ((component component))
  (append (x-properties component)
          (loop for slot in (c2mop:class-slots (class-of component))
                when (and (typep slot 'property-slot)
                          (slot-boundp component (c2mop:slot-definition-name slot)))
                append (let ((value (slot-value component (c2mop:slot-definition-name slot))))
                         (if (listp value) value (list value))))))

(defmacro define-component (name direct-superclasses direct-slots &rest options)
  (destructuring-bind (name identifier) (if (listp name) name (list name NIL))
    `(defclass ,name (,@direct-superclasses component)
       ,direct-slots
       (:metaclass component-class)
       ,@(when identifier `((:identifier ,identifier)))
       ,@options)))
