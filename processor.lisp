#|
 This file is a part of Clip
 (c) 2014 TymoonNET/NexT http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package #:org.tymoonnext.clip)

(defvar *block-table* ())
(defvar *form-table* ())
(defvar *form-element* NIL)

(define-condition no-such-block-error (error)
  ((%name :initarg :name :accessor name))
  (:report (lambda (c s) (format s "No block ~s found in block table." (name c)))))

(define-condition no-such-form-error (error)
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

(defmacro define-transforming-processor (name (elementvar &rest args) &body transforms)
  `(define-block-processor ,name (,elementvar ,@args)
     ,@transforms
     (scan-element ,elementvar)))

(defmacro define-binding-processor (name (elementvar &rest args) &body bindings)
  `(define-block-processor ,name (,elementvar ,@args)
     (let* ,bindings
       (scan-element ,elementvar))))

(defmacro define-form-processor (name (elementvar &rest args) &rest form-functions)
  `(define-block-processor ,name (,elementvar ,@args)
     (let ((*form-table* (append ,form-functions *form-table*)))
       (scan-element ,elementvar)
       (eval-element ,elementvar))))

(defun eval-element (element)
  (when-let ((attr (pop-attribute element "form")))
    ;; FIXME: Symbol pollution?
    (let ((*form-element* element))
      (invoke-form (read-from-string (format NIL "(~a)" attr)) element)))
  (loop for child across (dom:child-nodes element)
        unless (dom:text-node-p child)
          do (eval-element child)))

(defun invoke-form (form element)
  (typecase form
    ((or number string null symbol) form)
    (list (destructuring-bind (name &rest args) form
            (let ((function (cdr (assoc name *form-table*))))
              (unless function
                (error 'no-such-form-error :name name))
              (let ((args (mapcar #'(lambda (a) (invoke-form a element)) args)))
                (apply function args)))))))

(defmacro define-standard-form (name (&rest lambda-list) &body body)
  `(setf-alist *form-table* ',name
               #'(lambda ,lambda-list
                   ,@body)))
