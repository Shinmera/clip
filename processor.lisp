#|
 This file is a part of Clip
 (c) 2014 Shirakumo http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package #:org.tymoonnext.clip)

(defun process (target &rest fields)
  "Processes all clip markup on the target with the given FIELDS used to initialise the clipboard."
  (let ((*target-counter* 0)
        (*target* (etypecase target
                    (plump:node target)
                    (pathname (plump:parse target))
                    (string (plump:parse target)))))
    (with-clipboard-bound ((apply #'make-clipboard fields))
      (process-node *target*))
    *target*))

(defun process-to-string (target &rest fields)
  "Same as PROCESS, but automatically performs PLUMP:SERIALIZE on the result to a string."
  (with-output-to-string (stream)
    (plump:serialize (apply #'process target fields) stream)))
