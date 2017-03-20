#|
 This file is a part of Clip
 (c) 2014 Shirakumo http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package #:org.tymoonnext.clip)

(define-condition clip-condition ()
  ()
  (:documentation "Superclass for all conditions related to Clip."))

(define-condition node-condition (clip-condition)
  ((node :initarg :node))
  (:documentation "Superclass for all conditions related to problems with a node.

See CLIP-CONDITION"))

(define-condition attribute-condition (node-condition)
  ((attribute :initarg :attribute))
  (:documentation "Superclass for all conditions related to problems with a node's attribute.

See NODE-CONDITION"))

(define-condition missing-attribute (error attribute-condition)
  ()
  (:report (lambda (c s) (format s "The attribute ~s is required on the node ~a."
                                 (slot-value c 'attribute) (slot-value c 'node))))
  (:documentation "Condition signalled when a required attribute is missing.

See ATTRIBUTE-CONDITION"))

(define-condition unknown-attribute (warning attribute-condition)
  ()
  (:report (lambda (c s) (format s "The attribute ~s is present on the node ~a~%but does not have a specified effect."
                                 (slot-value c 'attribute) (slot-value c 'node))))
  (:documentation "Condition signalled when an unknown attribute is present.

See ATTRIBUTE-CONDITION"))

(defun check-attribute (node attribute)
  "Checks whether the given attribute is present on the node.

If it is, the attribute's value is returned.
Otherwise, an error of type MISSING-ATTRIBUTE is signalled.

See MISSING-ATTRIBUTE"
  (or (plump:attribute node attribute)
      (error 'missing-attribute :attribute attribute :node node)))

(defun check-no-unknown-attributes (node &rest known-attributes)
  "Checks whether there are any unknown attributes present on the node.

If an unknown attribute is present, a warning of type
UNKNOWN-ATTRIBUTE is signalled. Otherwise, NIL is returned.

See UNKNOWN-ATTRIBUTE"
  (loop for attribute being the hash-keys of (plump:attributes node)
        do (unless (find attribute known-attributes :test #'string-equal)
             (warn 'unknown-attribute :attribute attribute :node node))))

(defun check-sole-attribute (node attribute)
  "Checks whether the given attribute is the only attribute on the node.

If it is not present, or not the only one, an error is signalled.
Otherwise, the attribute's value is returned.

See CHECK-NO-UNKNOWN-ATTRIBUTES
See CHECK-ATTRIBUTE"
  (check-no-unknown-attributes node attribute)
  (check-attribute node attribute))
