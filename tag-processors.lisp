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
  (loop for i from 0 ;; We do this manually to allow growing size of the array.
        while (< i (length (plump:children node)))
        for child = (aref (plump:children node) i)
        do (process-node child))
  node)

(defmethod process-tag (tag (node plump:element))
  (process-attributes node)
  (process-children node)
  node)

(defun process-node (node)
  (incf *target-counter*)
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

(defun splice-into (node pos new-children)
  (let ((family (plump:children node))
        (childcount (length new-children)))
    (when (< 0 childcount)
      (plump::array-shift family :n (1- childcount) :from pos)
      (loop for i from 0 below childcount
            for child = (aref new-children i)
            do (setf (plump:parent child) node
                     (aref family (+ pos i)) child)))))

(define-tag-processor splice (node)
  (process-attributes node)
  (let* ((first (plump:first-element node))
         (parent (plump:parent node))
         (family (plump:children parent))
         (pos (position node family))
         (childcount (length (plump:children node))))
    (when first (process-node first))
    (cond
      ((< 0 childcount)
       (splice-into parent pos (plump:children node))
       (setf (plump:parent node) NIL))
      (T (plump:remove-child node)))))

(define-tag-processor when (node)
  (process-attributes node)
  (let ((test (resolve-value (read-from-string (plump:attribute node "test")))))
    (if test
        (process-tag :splice node)
        (plump:remove-child node))))

(define-tag-processor unless (node)
  (process-attributes node)
  (let ((test (resolve-value (read-from-string (plump:attribute node "test")))))
    (if test
        (plump:remove-child node)
        (process-tag :splice node))))

(define-tag-processor if (node)
  (process-attributes node)
  (let* ((test (resolve-value (read-from-string (plump:attribute node "test"))))
         (parent (plump:parent node))
         (pos (position node (plump:children parent)))
         (then) (else))
    (loop for child across (plump:children node)
          when (plump:element-p child)
            do (when (and (not then) (string-equal (plump:tag-name child) "then"))
                 (setf then child))
               (when (and (not else) (string-equal (plump:tag-name child) "else"))
                 (setf else child)))
    (flet ((splice (children)
             (when (< 0 (length children))
               (splice-into parent pos children)
               (process-node (aref children 0)))))
      (cond
        ((and then test)
         (when else (plump:remove-child else))
         (splice (plump:children then)))
        (else
         (when then (plump:remove-child then))
         (splice (plump:children else)))
        (T (plump:remove-child node))))))
