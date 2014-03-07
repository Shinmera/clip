#|
 This file is a part of Clip
 (c) 2014 TymoonNET/NexT http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package #:org.tymoonnext.clip)

(define-block-processor repeat (element times)
  (let ((times (parse-integer times))
        (clone (elt (remove-if #'dom:text-node-p (dom:child-nodes element)) 0)))
    (dotimes (i (1- times))
      (dom:append-child element (dom:clone-node clone T)))))

(define-block-processor do-strings (element strings)
  (let ((strings (split-sequence #\Space strings))
        (clone (elt (remove-if #'dom:text-node-p (dom:child-nodes element)) 0)))
    (dom:remove-child element clone)
    (dolist (string strings)
      (let ((clone (dom:clone-node clone T))
            (*fill-object* string))
        (scan-element clone)
        (fill-element clone)
        (dom:append-child element clone)))))

(define-standard-fill-function text (element object)
  (vector-push-extend
   (dom:create-text-node (slot-value element 'rune-dom::owner) (princ-to-string object))
   (setf (slot-value element 'rune-dom::children)
         (delete-if #'dom:text-node-p (slot-value element 'rune-dom::children)))))

(define-standard-fill-function html (element object)
  (flet ((resolver (pubid sysid)
           (declare (ignore pubid sysid))
           (flexi-streams:make-in-memory-input-stream nil)))
    (let ((node (cxml:parse (format NIL "<div>~a</div>" object) (cxml-dom:make-dom-builder)
                            :entity-resolver #'resolver :root (slot-value element 'rune-dom::owner))))
      (setf (slot-value element 'rune-dom::children)
            (rune-dom::make-node-list))
      (loop for child across (dom:child-nodes node)
            do (dom:append-child element child)))))

(define-standard-fill-function attr (element object attribute)
  (dom:set-attribute-ns element NIL (string-downcase attribute) (princ-to-string object)))

(define-standard-fill-function object (element object)
  (declare (ignore element))
  object)

(define-standard-fill-function list (element object &rest args)
  (declare (ignore element object))
  args)

(define-standard-fill-function concat (element object &rest args)
  (declare (ignore element object))
  (format NIL "~{~a~}" args))
