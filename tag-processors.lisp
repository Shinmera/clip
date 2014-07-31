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

(defmethod process-tag (tag (node plump:element))
  (process-attributes node)
  (process-children node))

(defmacro define-tag-processor (tag &rest args)
  (flet ((make-body ()
           `(((,(gensym "TAG") (eql ,(make-keyword tag))) ,@(first args)) ,@(rest args))))
    `(defmethod process-tag ,@(if (listp (first args))
                                  (make-body)
                                  (cons (pop args) (make-body))))))

(defun process-children (node)
  (loop for child across (plump:children node)
        do (let ((*target* child))
             (process-node child))))

(defun process-node (node)
  (etypecase node
    (plump:element (process-tag (make-keyword (string-upcase (plump:tag-name node))) node))
    (plump:nesting-node (process-children node))
    (plump:node)))

(define-tag-processor noop (node))

(define-tag-processor let (node)
  (maphash #'(lambda (key val)
               (setf (clipboard key) (resolve-value (read-from-string val))))
           (plump:attributes node))
  (process-children node))

(define-tag-processor iterate (node)
  (let* ((var (loop for key being the hash-keys of (plump:attributes node)
                    for val being the hash-values of (plump:attributes node)
                    when (string= val "") do (return key)))
         (val (resolve-value (read-from-string var)))
         (new-children (plump:make-child-array))
         (target (plump:first-element node)))
    (flet ((process (item)
             (let ((*target* (plump:clone-node target))
                   (*clipboard* item))
               (process-node *target*)
               (vector-push-extend *target* new-children))))
      (etypecase val
        (list (loop for item in val do (process item)))
        (vector (loop for item across val do (process item)))))
    (setf (plump:children node) new-children)
    (plump:remove-attribute node var)))
