#|
 This file is a part of Clip
 (c) 2014 TymoonNET/NexT http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package #:org.tymoonnext.clip)

(defvar *block-table* ())
(defvar *fill-table* ())
(defvar *fill-object* NIL)

(define-condition no-such-block-error (error)
  ((%name :initarg :name :accessor name))
  (:report (lambda (c s) (format s "No block ~s found in block table." (name c)))))

(define-condition no-such-fill-function-error (error)
  ((%name :initarg :name :accessor name))
  (:report (lambda (c s) (format s "No fill function ~s found in fill table." (name c)))))

(defmacro setf-alist (alist key val)
  (with-gensyms (g-find g-key g-val)
    `(let* ((,g-key ,key)
            (,g-val ,val)
            (,g-find (find ,g-key ,alist :key #'car)))
       (if ,g-find
           (setf (cdr ,g-find) ,g-val)
           (push (cons ,g-key ,g-val) ,alist)))))

(defmacro define-block-processor (name (elementvar &rest attributes) &body body)
  `(setf-alist *block-table* ,(intern (string-upcase name) "KEYWORD")
               #'(lambda (,elementvar)
                   (let ,(loop for attr in attributes
                               collect `(,attr (pop-attribute ,elementvar ,(string-downcase attr))))
                     ,@body))))

(defun pop-attribute (element attribute)
  (let* ((attribute (string-downcase attribute))
         (return (dom:get-attribute element attribute)))
    (if (string= "" return)
        NIL
        (progn
          (dom:remove-attribute element attribute)
          return))))

(defun scan-element (element)
  (loop for child across (dom:child-nodes element)
        unless (dom:text-node-p child)
          do (block current-recurse
               (when-let ((attr (pop-attribute child "block")))
                 (invoke-block attr child)
                 (return-from current-recurse))
               (scan-element child))))

(defun invoke-block (block element)
  (let ((name (find-symbol (string-upcase block) "KEYWORD")))
    (unless name
      (error 'no-such-block-error :name block))
    (let ((processor (cdr (assoc name *block-table*))))
      (unless processor
        (error 'no-such-block-error :name block))
      (funcall processor element))))

(defmacro define-transforming-processor (name (elementvar) &body transforms)
  `(define-block-processor ,name (,elementvar)
     ,@transforms
     (scan-element ,elementvar)))

(defmacro define-binding-processor (name (elementvar) &body bindings)
  `(define-block-processor ,name (,elementvar)
     (let* ,bindings
       (scan-element ,elementvar))))

(defmacro define-fill-processor (name (elementvar &rest args) fill-object-form &body fill-functions)
  `(define-block-processor ,name (,elementvar ,@args)
     (let ((*fill-object* ,fill-object-form)
           (*fill-table* (append *fill-table* ,fill-functions)))
       (scan-element ,elementvar)
       (fill-element ,elementvar))))

(defun fill-element (element)
  (when-let ((attr (pop-attribute element "fill")))
    ;; FIXME: Symbol pollution?
    (dolist (call (read-from-string (format NIL "(~a)" attr)))
      (invoke-fill call element)))
  (loop for child across (dom:child-nodes element)
        unless (dom:text-node-p child)
          do (fill-element child)))

(defun invoke-fill (fill element)
  (typecase fill
    ((or number string null) fill)
    (symbol (invoke-fill (list fill) element))
    (list (destructuring-bind (name &rest args) fill
            (let ((function (cdr (assoc name *fill-table*))))
              (unless function
                (error 'no-such-fill-function-error :name name))
              (let ((args (mapcar #'(lambda (a) (invoke-fill a element)) args)))
                (apply function element *fill-object* args)))))))

(defmacro define-standard-fill-function (name (elementvar objectvar &rest args) &body body)
  `(setf-alist *fill-table* ',name
               #'(lambda (,elementvar ,objectvar ,@args)
                   ,@body)))
