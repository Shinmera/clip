#|
 This file is a part of Clip
 (c) 2014 TymoonNET/NexT http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package #:org.tymoonnext.clip)

(defvar *clipboard*)

(defun make-clipboard (fields)
  (let ((table (make-hash-table)))
    (loop for (key val) on fields by #'cddr
          do (setf (gethash key table) val))
    table))

(defgeneric clip (object field))
(defgeneric (setf clip) (value object field))

(defun clipboard (field)
  (clip *clipboard* field))

(defun (setf clipboard) (value field)
  (setf (clip *clipboard* field) value))

(defmethod clip ((table hash-table) field)
  (gethash field table))

(defmethod (setf clip) (value (table hash-table) field)
  (setf (gethash field table) value))

(defmethod clip ((model list) field)
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
  (let ((field (find-symbol (string field)
                            (symbol-package (class-name (class-of model))))))
    (if field (slot-value model field))))

(defmethod (setf clip) (value (model standard-object) field)
  (let ((field (find-symbol (string field)
                            (symbol-package (class-name (class-of model))))))
    (if field (setf (slot-value model field) value))))

(defgeneric resolve-value (object))

(defmethod resolve-value (object) object)

(defmethod resolve-value ((symbol symbol))
  (if (eql symbol '*)
      *clipboard*
      (clipboard symbol)))

(defmethod resolve-value ((list list))
  (let ((func (car list))
        (args (cdr list)))
    (case func
      (quote (first args))
      (function (symbol-function (first args)))
      (T (apply (or (find-symbol (string func) :clip) func)
                (mapcar #'resolve-value args))))))

(defun resolve-attribute (node attr)
  (resolve-value (read-from-string (plump:attribute node attr))))
