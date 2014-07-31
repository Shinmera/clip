#|
 This file is a part of Clip
 (c) 2014 TymoonNET/NexT http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package #:cl-user)
(defpackage #:clip
  (:nicknames #:org.tymoonnext.clip)
  (:use #:cl #:lquery)
  ;; attr-processors.lisp
  (:export
   #:process-attribute
   #:define-attribute-processor
   #:process-attributes)
  ;; clipboard.lisp
  (:export
   #:*clipboard*
   #:clip
   #:clipboard
   #:make-clipboard
   #:resolve-value
   #:resolve-attribute)
  ;; processor.lisp
  (:export
   #:*target*
   #:process)
  ;; tag-processors.lisp
  (:export
   #:process-tag
   #:define-tag-processor
   #:process-children
   #:process-node)
  ;; toolkit.lisp
  (:export))
