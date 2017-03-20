#|
 This file is a part of Clip
 (c) 2014 Shirakumo http://tymoon.eu (shinmera@tymoon.eu)
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
   #:*clipboard-stack*
   #:clipboard
   #:make-clipboard
   #:with-clipboard-bound
   #:clip
   #:clipboard
   #:resolve-value
   #:resolve-attribute
   #:parse-and-resolve)
  ;; conditions.lisp
  (:export
   #:clip-condition
   #:node-condition
   #:attribute-condition
   #:missing-attribute
   #:unknown-attribute
   #:check-attribute
   #:check-no-unknown-attributes
   #:check-sole-attribute)
  ;; processor.lisp
  (:export
   #:process
   #:process-to-string)
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
