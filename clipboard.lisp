#|
 This file is a part of Clip
 (c) 2014 TymoonNET/NexT http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package #:org.tymoonnext.clip)

(defvar *clipboard-stack* NIL
  "Template storage stack. When new clipboards are bound, they are pushed onto the stack.
Once the binding is left, they are popped off the stack again.")

(defclass clipboard ()
  ((%clipboard-env :initarg :env :initform (make-hash-table :test 'equalp) :accessor clipboard-env))
  (:documentation "Special class for clipboard environments. Use CLIPBOARD or CLIP to access and set values within.
Field names are automatically transformed into strings as per STRING. Access is case-insensitive."))

(defun make-clipboard (&rest fields)
  "Creates a new clipboard using the specified fields (like a plist)."
  (let ((table (make-hash-table :test 'equalp)))
    (loop for (key val) on fields by #'cddr
          do (setf (gethash (string key) table) val))
    (make-instance 'clipboard :env table)))

(defmacro with-clipboard-bound ((new-clipboard &rest fields) &body body)
  "Executes the body with the new clipboard on the *CLIPBOARD-STACK*.

If fields are provided, they are set on the NEW-CLIPBOARD in plist fashion as per consecutive SETF.
This means that side-effects of an early field set affect later fields. The fields are evaluated
before the NEW-CLIPBOARD is pushed onto the *CLIPBOARD-STACK*."
  (let ((clipboard (gensym "CLIPBOARD")))
    `(let ((,clipboard ,new-clipboard))
       ,@(loop for (field value) on fields by #'cddr collect `(setf (clip ,clipboard ,field) ,value))
       (let ((*clipboard-stack* (cons ,clipboard *clipboard-stack*)))
         ,@body))))

(defgeneric clip (object field)
  (:documentation "Generic object accessor.
If you want to get special treatment of objects or types, define your own methods on this."))

(defgeneric (setf clip) (value object field)
  (:documentation "Generic object setter.
If you want to get special treatment of objects or types, define your own methods on this."))

(defun clipboard (field)
  "Shorthand for (CLIP (FIRST *CLIPBOARD-STACK*) FIELD)"
  (clip (first *clipboard-stack*) field))

(defun (setf clipboard) (value field)
  "Shorthand for (SETF (CLIP (FIRST *CLIPBOARD-STACK*) FIELD) VALUE)"
  (setf (clip (first *clipboard-stack*) field) value))

(defmethod clip ((board clipboard) field)
  "Accessor for the clipboard object."
  (gethash (string field) (clipboard-env board)))

(defmethod (setf clip) (value (board clipboard) field)
  (setf (gethash (string field) (clipboard-env board)) value))

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
If the symbol is a keyword the symbol itself is returned,
otherwise the value of (CLIPBOARD SYMBOL) is returned."
  (cond ((loop for char across (symbol-name symbol)
               always (char= char #\*))
         (nth (1- (length (symbol-name symbol))) *clipboard-stack*))
        ((keywordp symbol)
         symbol)
        ((eq symbol T)
         T)
        ((eq symbol NIL)
         NIL)
        (T (clipboard symbol))))

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
    (cond ((and (loop for char across (symbol-name func)
                      always (char= char #\*))
                (<= 2 (length (symbol-name func))))
           (clip (nth (1- (length (symbol-name func))) *clipboard-stack*)
                 (resolve-value (first args))))
          (T
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
             (if (if (resolve-value (first args))
                     (resolve-value (second args))
                     (resolve-value (third args))))
             (when (when (resolve-value (first args))
                     (loop for form in (rest args)
                           for val = (resolve-value form)
                           finally (return val))))
             (unless (unless (resolve-value (first args))
                       (loop for form in (rest args)
                             for val = (resolve-value form)
                             finally (return val))))
             (T (apply (or (multiple-value-bind (s e) (find-symbol (string func) :clip)
                             (when (and s (eql e :external) (fboundp s)) s)) func)
                       (mapcar #'resolve-value args))))))))

(defun resolve-attribute (node attr)
  "Shorthand to resolve the value of an attibute.
See RESOLVE-VALUE."
  (resolve-value (read-from-string (plump:attribute node attr))))

(defun parse-and-resolve (value)
  "If the passed value is a STRING it is parsed using READ-FROM-STRING and subsequently passed to RESOLVE-VALUE.
If it is not a string, the value itself is returned."
  (if (stringp value)
      (resolve-value (read-from-string value))
      value))
