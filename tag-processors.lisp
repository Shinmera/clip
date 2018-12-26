#|
 This file is a part of Clip
 (c) 2014 Shirakumo http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package #:org.tymoonnext.clip)

(defvar *tag-processors* (make-hash-table :test 'equalp)
  "Global registry of tag processors.

This has to be an EQUALP hash-table with the tag name as keys
and functions that accept one argument (the node) as
values. Binding this variable can be useful to establish local
tags.")

(defun tag-processor (tag)
  "Returns the processor function for the requested tag if one is registered.
Otherwise returns NIL. See *TAG-PROCESSORS*."
  (gethash tag *tag-processors*))

(defun (setf tag-processor) (func tag)
  "Sets the tag-processor bound to the given tag-name to the specified function.
See *TAG-PROCESSORS*."
  (setf (gethash tag *tag-processors*) func))

(defun process-tag (tag node)
  "Processes the specified node as the given tag.
If no tag processor can be found, PROCESS-ATTRIBUTES and PROCESS-CHILDREN is called.
See *TAG-PROCESSORS*."
  (let ((func (tag-processor tag)))
    (cond
      (func (funcall func node))
      (T (process-attributes node)
         (process-children node)))))

(defmacro define-tag-processor (tag (node) &body body)
  "Defines a new attribute processor.

TAG    --- A symbol or string that matches the tag name to process (case-insensitive)
NODE   --- The node to process is bound to this symbol
BODY   ::= form*"
  `(setf (tag-processor ,(format NIL "C:~a" tag))
    (setf (tag-processor ,(string tag))
          #'(lambda (,node) ,@body))))

(defun process-children (node)
  "Calls PROCESS-NODE on all childrens of the passed node.

This takes some care to make sure that splicing into the childrens array
of the node is possible. However, note that inserting children before the
node that is currently being processed will most likely lead to horrors.
If such functionality is indeed ever needed (I hope not), this system
needs to be rewritten to somehow be able to cope with such scenarios. "
  (loop for i from 0 ;; We do this manually to allow growing size of the array.
        while (< i (length (plump:children node)))
        do (process-node (aref (plump:children node) i)))
  node)

(defun process-node (node)
  "Processes the passed node.

Depending on type the following is done:
PLUMP:ELEMENT       PROCESS-TAG is called.
PLUMP:NESTING-NODE  PROCESS-CHILDREN is called.
PLUMP:NODE          Nothing is done.
T                   An error is signalled.
Any call to this also increases the *TARGET-COUNTER* regardless of what
is done."
  (let ((*target* node))
    (etypecase node
      (plump:element (process-tag (plump:tag-name node) node))
      (plump:nesting-node (process-children node))
      (plump:node))
    (incf *target-counter*)
    node))

(define-tag-processor noop (node)
  (process-attributes node))

(define-tag-processor let (node)
  (let ((table (make-hash-table :test 'equalp)))
    (maphash #'(lambda (key val)
                 (setf (clip table key)
                       (parse-and-resolve val)))
             (plump:attributes node))
    (with-clipboard-bound ((make-instance 'clipboard :env table))
      (process-children node))
    (clrhash (plump:attributes node))
    (process-tag "splice" node)))

(define-tag-processor iterate (node)
  (process-attributes node)
  (let ((var (check-sole-attribute node "over")))
    (plump:remove-attribute node "over")
    (process-attribute node "iterate" var)))

(define-tag-processor expand (node)
  (process-attributes node)
  (process-node node))

(defun splice-into (node pos new-children)
  (let ((family (plump:children node))
        (childcount (length new-children)))
    (when (< 0 childcount)
      (array-utils:array-shift family :n (1- childcount) :from pos)
      (loop for i from 0 below childcount
            for child = (aref new-children i)
            do (setf (plump:parent child) node
                     (aref family (+ pos i)) child)))))

(define-tag-processor splice (node)
  (process-attributes node)
  (check-no-unknown-attributes node)
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

(define-tag-processor splice-inner (node)
  (process-attributes node)
  (check-no-unknown-attributes node)
  (let* ((first (plump:first-element node))
         (parent (plump:parent node))
         (family (plump:children parent))
         (pos (position node family))
         (childcount (length (plump:children node))))
    (when first (process-node first))
    (cond
      ((< 0 childcount)
       (splice-into parent pos (loop with totals = (make-array 0 :adjustable T :fill-pointer T)
                                     for child across (plump:children node)
                                     do (loop for inner across (plump:children child)
                                              do (vector-push-extend inner totals))
                                     finally (return totals)))
       (setf (plump:parent node) NIL))
      (T (plump:remove-child node)))))

(define-tag-processor when (node)
  (process-attributes node)
  (let ((test (parse-and-resolve (check-sole-attribute node "test"))))
    (plump:remove-attribute node "test")
    (if test
        (process-tag "splice" node)
        (plump:remove-child node))))

(define-tag-processor unless (node)
  (process-attributes node)
  (let ((test (parse-and-resolve (check-sole-attribute node "test"))))
    (plump:remove-attribute node "test")
    (if test
        (plump:remove-child node)
        (process-tag "splice" node))))

(define-tag-processor if (node)
  (process-attributes node)
  (check-no-unknown-attributes node "test")
  (let* ((parent (plump:parent node))
         (pos (position node (plump:children parent)))
         (then) (else) (test (plump:attribute node "test")))
    (plump:remove-attribute node "test")
    ;; Gather elements
    (loop for child across (plump:children node)
          when (plump:element-p child)
            do (when (or (string-equal (plump:tag-name child) "test")
                         (string-equal (plump:tag-name child) "c:test"))
                 (setf test (plump:text child)))
               (when (and (not then) (or (string-equal (plump:tag-name child) "then")
                                         (string-equal (plump:tag-name child) "c:then")))
                 (setf then child))
               (when (and (not else) (or (string-equal (plump:tag-name child) "else")
                                         (string-equal (plump:tag-name child) "c:else")))
                 (setf else child)))
    (unless test
      (error 'missing-attribute :attribute "test" :node node))
    ;; Parse test
    (when (stringp test)
      (setf test (resolve-value (read-from-string test))))
    ;; Perform splice
    (flet ((splice (node)
             (process-attributes node)
             (let ((children (plump:children node)))
               (when (< 0 (length children))
                 (splice-into parent pos children)
                 ;; We need to splice the first since it is in-place of the if.
                 (process-node (aref children 0))))))
      (cond
        ((and then test)
         (when else (plump:remove-child else))
         (splice then))
        (else
         (when then (plump:remove-child then))
         (splice else))
        (T (plump:remove-child node))))))

(define-tag-processor using (node)
  (process-attributes node)
  (with-clipboard-bound ((parse-and-resolve (check-sole-attribute node "value")))
    (plump:remove-attribute node "value")
    (process-children node)
    (process-tag "splice" node)))

(define-tag-processor import (node)
  (let ((path (merge-pathnames (resolve-attribute node "file"))))
    (plump:remove-attribute node "file")
    (plump:parse path :root node)
    (process-tag "splice" node)))
