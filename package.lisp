#|
 This file is a part of Clip
 (c) 2014 TymoonNET/NexT http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package #:cl-user)
(defpackage #:clip
  (:nicknames #:org.tymoonnext.clip)
  (:use #:cl #:alexandria #:split-sequence)
  ;; processor.lisp
  (:export
   #:*block-table*
   #:*form-table*
   #:*form-element*
   
   #:no-such-block-error
   #:no-such-form-error
   #:name
   
   #:define-block-processor
   #:scan-element
   #:invoke-block
   #:define-transforming-processor
   #:define-binding-processor
   #:define-form-processor
   #:eval-element
   #:invoke-form
   #:define-standard-form)
  ;; blocks.lisp
  (:export)
  ;; fill.lisp
  (:export
   #:*fill-table*
   #:*fill-object*
   
   #:define-fill-processor
   #:define-iterating-fill-processor
   #:define-standard-fill-form)
  )

(defpackage #:clip-user)
