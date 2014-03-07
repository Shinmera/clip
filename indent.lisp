#|
 This file is a part of Chirp
 (c) 2014 TymoonNET/NexT http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package #:org.tymoonnext.clip)

(defvar *indentation-hints* (make-hash-table :test #'eq))

(defun define-indentation (symbol rule-form)
  (setf (gethash symbol *indentation-hints*)
        rule-form))

(defun initialize-slime ()
  (when (member "SWANK-INDENTATION" *modules* :test #'string=)
    (let* ((swank (find-package :swank))
           (tables (when swank (find-symbol (string '#:*application-hints-tables*) swank))))
      (when tables
        (set tables (cons *indentation-hints* (remove *indentation-hints* (symbol-value tables))))
        t))))

(initialize-slime)

(define-indentation 'define-iterating-fill-processor '(2 2 4 &rest (&whole 2 &lambda 4 &body)))
