#|
 This file is a part of Clip
 (c) 2014 TymoonNET/NexT http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package #:org.tymoonnext.clip)

(defvar *target*)
(defvar *target-counter* 0)

(defgeneric process-attribute (attribute value))

(defmethod process-attribute (attribute value)
  ;; NOOP
  )

(defmacro define-attribute-processor (attribute (node value) &body body)
  `(defmethod process-attribute ((,(gensym "ATTR") (eql ,(make-keyword attribute))) ,value)
     (let ((,node *target*))
       ,@body)))

(defun process-attributes (node)
  (maphash #'(lambda (key val)
               (process-attribute (make-keyword (string-upcase key)) val))
           (plump:attributes node)))

(defun %resolve-lquery-arg (arg)
  (typecase arg
    (keyword arg)
    (T (resolve-value arg))))

(define-attribute-processor lquery (node value)
  (let ((actions (typecase value
                   (list (list value))
                   (string (read-from-string (format NIL "(~a)" value))))))
    (plump:remove-attribute node "lquery")
    (loop with node = (make-proper-vector :size 1 :initial-element node :fill-pointer T)
          for (func . args) in actions
          do (apply (or (find-symbol (string func) :lquery-funcs) func)
                    node (mapcar #'%resolve-lquery-arg args)))))

(define-attribute-processor eval (node value)
  (eval (read-from-string value))
  (plump:remove-attribute node "eval"))

(define-attribute-processor iterate (node value)
  (let ((val (resolve-value (read-from-string value)))
        (new-children (plump:make-child-array))
        (target (plump:first-element node)))
    (flet ((process (item)
             (let ((*clipboard* item))
               (vector-push-extend
                (process-node (plump:clone-node target))
                new-children))))
      (etypecase val
        (list (loop for item in val do (process item)))
        (vector (loop for item across val do (process item)))))
    (setf (plump:children node) new-children)
    (plump:remove-attribute node "iterate")))

(define-attribute-processor do (node value)
  (let ((val (read-from-string (format NIL "(~a)" value)))
        (new-children (plump:make-child-array))
        (target (plump:first-element node)))
    (loop for item in val
          do (let ((*clipboard* item))
               (vector-push-extend
                (process-node (plump:clone-node target))
                new-children)))
    (setf (plump:children node) new-children)
    (plump:remove-attribute node "do")))

(define-attribute-processor as (node value)
  (setf (plump:tag-name node) value)
  (plump:remove-attribute node "as"))

(define-attribute-processor count (node value)
  (setf (plump:attribute node "count") (princ-to-string *target-counter*)))
