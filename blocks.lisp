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

(define-standard-form list (&rest args)
  args)

(define-standard-form concat (&rest args)
  (format NIL "~{~a~}" args))

(define-standard-form slot (object slot)
  (slot-value object slot))

(define-standard-form funcall (function &rest args)
  (apply function args))
