#|
 This file is a part of Clip
 (c) 2014 TymoonNET/NexT http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package #:org.tymoonnext.clip)

(defgeneric process-tag (tag node))

(defmethod process-tag (tag node)
  ;; NOOP
  )

(defmacro define-tag-processor (tag &rest args)
  (flet ((make-body ()
           `(((,(gensym "TAG") (eql ,(make-keyword tag))) ,@(first args)) ,@(rest args))))
    `(defmethod process-tag ,@(if (listp (first args))
                                  (make-body)
                                  (cons (pop args) (make-body))))))

(defun process-children (node)
  (loop for child across (plump:children node)
        do (process-node child))
  node)

(defmethod process-tag (tag (node plump:element))
  (process-attributes node)
  (process-children node)
  node)

(defun process-node (node)
  (let ((*target* node))
    (etypecase node
      (plump:element (process-tag (make-keyword (string-upcase (plump:tag-name node))) node))
      (plump:nesting-node (process-children node))
      (plump:node))
    node))

(define-tag-processor noop (node))

(define-tag-processor let (node)
  (maphash #'(lambda (key val)
               (setf (clipboard key) (resolve-value (read-from-string val))))
           (plump:attributes node))
  (process-children node))

(define-tag-processor iterate (node)
  (let ((var (plump:attribute node "over")))
    (plump:remove-attribute node var)
    (process-attribute :iterate var)
    (process-attributes node)))

(define-tag-processor expand (node)
  (process-attributes node)
  (setf (plump:tag-name node)
        (or (plump:attribute node "to") "expand"))
  (plump:remove-attribute node "to")
  (process-node node))

;; Todo: Tag splicing, conditionals
