#|
 This file is a part of Clip
 (c) 2014 TymoonNET/NexT http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package #:org.tymoonnext.clip)

(defvar *clipboard* NIL
  "Template storage object.")

(defun make-clipboard (&rest fields)
  "Creates a new clipboard using the specified fields (like a plist)."
  (let ((table (make-hash-table)))
    (loop for (key val) on fields by #'cddr
          do (setf (gethash key table) val))
    table))

(defgeneric clip (object field)
  (:documentation "Generic object accessor.
If you want to get special treatment of objects or types, define your own methods on this."))

(defgeneric (setf clip) (value object field)
  (:documentation "Generic object setter.
If you want to get special treatment of objects or types, define your own methods on this."))

(defun clipboard (field)
  "Shorthand for (CLIP *CLIPBOARD* FIELD)"
  (clip *clipboard* field))

(defun (setf clipboard) (value field)
  "Shorthand for (SETF (CLIP *CLIPBOARD* FIELD) VALUE)"
  (setf (clip *clipboard* field) value))

(defmethod clip ((table hash-table) field)
  "Generic hash-table accessor."
  (gethash field table))

(defmethod (setf clip) (value (table hash-table) field)
  (setf (gethash field table) value))

(defmethod clip ((model list) field)
  "Generic alist or plist accessor."
  (cond
    ((keywordp (first model))
     (getf model (make-keyword field)))
    ((listp (first model))
     (cdr (assoc field model :test #'equalp)))
    (T
     (error "Model is of type LIST, but is neither an ALIST or PLIST."))))

(defmethod (setf clip) (value (model list) field)
  (cond
    ((keywordp (first model))
     (setf (getf model (make-keyword field)) value))
    ((listp (first model))
     (setf (cdr (assoc field model :test #'equalp)) value))
    (T
     (error "Model is of type LIST, but is neither an ALIST or PLIST."))))

(defmethod clip ((model standard-object) field)
  "Generic slot accessor."
  (let ((field (find-symbol (string field)
                            (symbol-package (class-name (class-of model))))))
    (if field (slot-value model field))))

(defmethod (setf clip) (value (model standard-object) field)
  (let ((field (find-symbol (string field)
                            (symbol-package (class-name (class-of model))))))
    (if field (setf (slot-value model field) value))))

(defgeneric resolve-value (object)
  (:documentation "Attempts to resolve the object to a specific value.
This is usually used in combination with READ-FROM-STRING of an attribute value."))

(defmethod resolve-value (object)
  "Default fallback for unrecognized objects; simply returns it."
  object)

(defmethod resolve-value ((symbol symbol))
  "Handler for symbols.
If the symbol is EQL to '* the *CLIPBOARD* is returned,
otherwise the value of (CLIPBOARD SYMBOL) is returned."
  (if (eql symbol '*)
      *clipboard*
      (clipboard symbol)))

(defmethod resolve-value ((list list))
  "Handler for lists, aka function calls.

The function call is decided upon the CAR of the list.
The following cases are handled:

QUOTE     Returns the first argument
FUNCTION  Returns the symbol-function of the first argument
OR        Simulated version of the OR macro.
AND       Simulated version of the AND macro.

Otherwise the symbol is looked for in the :CLIP package
and then the current *PACKAGE*. If found, the function is
applied with all arguments of the list (which are first
all individually passed to RESOLVE-VALUE too)."
  (let ((func (car list))
        (args (cdr list)))
    (case func
      (quote (first args))
      (function (symbol-function (first args)))
      (or (loop for arg in args
                thereis (resolve-value arg)))
      (and (loop for arg in args
                 for val = (resolve-value arg)
                 when (not val)
                   do (return)
                 finally (return val)))
      (T (apply (or (find-symbol (string func) :clip) func)
                (mapcar #'resolve-value args))))))

(defun resolve-attribute (node attr)
  "Shorthand to resolve the value of an attibute.
See RESOLVE-VALUE."
  (resolve-value (read-from-string (plump:attribute node attr))))
