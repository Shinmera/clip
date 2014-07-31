#|
 This file is a part of Clip
 (c) 2014 TymoonNET/NexT http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package #:org.tymoonnext.clip)

(defun process (target &rest fields)
  (let ((*clipboard* (make-clipboard fields))
        (*target* (etypecase target
                    (plump:node target)
                    (pathname (plump:parse target))
                    (string (plump:parse target)))))
    (process-node *target*)
    *target*))
