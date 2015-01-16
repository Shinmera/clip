#|
 This file is a part of Clip
 (c) 2014 Shirakumo http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package #:org.tymoonnext.clip)

(defvar *attribute-processors* (make-hash-table :test 'equalp)
  "Global registry of attribute processors.

This has to be an EQUALP hash-table with the attribute name as keys
and functions that accept two arguments (node attribute-value) as
values. Binding this variable can be useful to establish local
attributes.")

(defvar *target* NIL
  "This variable is bound to whatever node is currently being processed.")

(defvar *target-counter* 0
  "This counter is upped whenever process-node is called.")

(defun attribute-processor (attribute)
  "Returns the processor function for the requested attribute if one is registered.
Otherwise returns NIL. See *ATTRIBUTE-PROCESSORS*."
  (gethash attribute *attribute-processors*))

(defun (setf attribute-processor) (func attribute)
  "Sets the attribute-processor bound to the given attribute to the specified function.
See *ATTRIBUTE-PROCESSORS*."
  (setf (gethash attribute *attribute-processors*) func))

(defun process-attribute (node attribute value)
  "Processes the specified attribute using the given value.
If no attribute processor can be found, nothing is done.
See *ATTRIBUTE-PROCESSORS*."
  (let ((func (attribute-processor attribute)))
    (when func (funcall func node value))))

(defmacro define-attribute-processor (attribute (node value) &body body)
  "Defines a new attribute processor.

ATTRIBTUE --- A symbol or string that matches the attribute to process (case-insensitive)
NODE      --- The current node is bound to this symbol.
VALUE     --- The value of the attribute is bound to this symbol.
BODY      ::= form*"
  `(setf (attribute-processor ,(string attribute))
         #'(lambda (,node ,value) ,@body)))

(defun process-attributes (node)
  "Processes all attributes on the node.
See PROCESS-ATTRIBUTE."
  (maphash #'(lambda (attr val) (process-attribute node attr val))
           (plump:attributes node)))

(define-attribute-processor lquery (node value)
  (let ((actions (typecase value
                   (list value)
                   (string (read-from-string (format NIL "(~a)" value))))))
    (plump:remove-attribute node "lquery")
    (loop with node = (make-proper-vector :size 1 :initial-element node :fill-pointer T)
          for (func . args) in actions
          do (apply (or (find-symbol (string func) :lquery-funcs) func)
                    node (mapcar #'resolve-value args)))))

(define-attribute-processor eval (node value)
  (eval (read-from-string value))
  (plump:remove-attribute node "eval"))

(define-attribute-processor iterate (node value)
  (let ((val (parse-and-resolve value))
        (new-children (plump:make-child-array))
        (target (plump:first-element node)))
    (flet ((process (item)
             (with-clipboard-bound (item)
               (vector-push-extend
                (process-node (plump:clone-node target))
                new-children))))
      (etypecase val
        (list (loop for item in val do (process item)))
        (vector (loop for item across val do (process item)))))
    (setf (plump:children node) new-children)
    (plump:remove-attribute node "iterate")))

(define-attribute-processor as (node value)
  (setf (plump:tag-name node) value)
  (plump:remove-attribute node "as"))

(define-attribute-processor count (node value)
  (declare (ignore value))
  (setf (plump:attribute node "count") (princ-to-string *target-counter*)))


(defun replace-region (string start end replacer)
  (with-output-to-string (output)
    (loop with filler = (make-string-output-stream)
          with target = output
          for char across string
          do (cond
               ((char= char start)
                (setf target filler))
               
               ((char= char end)
                (unless (eql target filler)
                  (error "Unmatched closing."))
                (princ (funcall replacer (get-output-stream-string filler)) output)
                (close filler)
                (setf target output)
                (setf filler (make-string-output-stream)))
               
               (T (write-char char target)))
          finally (if (eql target filler)
                      (error "Unmatched opening.")
                      (close filler)))))

(define-attribute-processor fill (node value)
  (loop for (attribute object) on (read-from-string (format NIL "(~a)" value))
        for attr = (string attribute)
        do (let ((value (plump:attribute node attr)))
             (when value
               (setf (plump:attribute node attr)
                     (replace-region
                      value #\{ #\}
                      #'(lambda (value)
                          (clip (resolve-value object) value)))))))
  (plump:remove-attribute node "fill"))
