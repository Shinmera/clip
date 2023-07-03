(in-package #:org.tymoonnext.clip)

(defun concat (list)
  "Returns a space concatenated string of the passed list."
  (format NIL "~{~a~^ ~}" list))

(defun make-keyword (name)
  "Returns a keyword of the passed name."
  (let ((name (string name)))
    (or (find-symbol name "KEYWORD")
        (intern name "KEYWORD"))))
