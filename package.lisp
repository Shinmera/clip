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
   #:*attribute-processors*
   #:*target*
   #:*target-count*
   #:attribute-processor
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
   #:resolve-attribute
   #:parse-and-resolve)
  ;; processor.lisp
  (:export
   #:process)
  ;; tag-processors.lisp
  (:export
   #:*tag-processors*
   #:tag-processor
   #:process-tag
   #:define-tag-processor
   #:process-children
   #:process-node)
  ;; toolkit.lisp
  (:export))
