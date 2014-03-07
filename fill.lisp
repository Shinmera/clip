#|
 This file is a part of Clip
 (c) 2014 TymoonNET/NexT http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package #:org.tymoonnext.clip)

(defvar *fill-table* NIL)
(defvar *fill-object* NIL)

(defun make-fill-functions (form-definitions)
  `(list ,@(loop for def in form-definitions
                 collect (destructuring-bind (symbol args &rest body) def
                           `(cons ',(intern (string symbol) "CLIP-USER")
                                  #'(lambda ,(cddr args)
                                      (let ((,(first args) *form-element*)
                                            (,(second args) *fill-object*))
                                        (declare (ignorable ,(first args) ,(second args)))
                                        ,@body)))))))

(defmacro define-fill-processor (name (elementvar &rest args) fill-object-form &body fill-functions)
  `(define-block-processor ,name (,elementvar ,@args)
     (let ((*form-table* (append ,(make-fill-functions fill-functions) *fill-table* *form-table*))
           (*fill-object* ,fill-object-form))
       (scan-element ,elementvar)
       (eval-element ,elementvar))))

(defmacro define-iterating-fill-processor (name (elementvar &rest args) fill-list-form &body fill-functions)
  (with-gensyms ((template "TEMPLATE") (item "ITEM") (clone "CLONE"))
    `(define-block-processor ,name (,elementvar ,@args)
       (let ((,template (elt (remove-if #'dom:text-node-p (dom:child-nodes ,elementvar)) 0)))
         (dom:remove-child ,elementvar ,template)
         (let ((*form-table* (append ,(make-fill-functions fill-functions) *fill-table* *form-table*)))
           (dolist (,item ,fill-list-form)
             (let ((,clone (dom:clone-node ,template T))
                   (*fill-object* ,item))
               (scan-element ,clone)
               (eval-element ,clone)
               (dom:append-child ,elementvar ,clone))))))))

(defmacro define-standard-fill-form (name (element object &rest lambda-list) &body body)
  `(setf-alist *fill-table* ',(intern (string name) "CLIP-USER")
               #'(lambda ,lambda-list
                   (let ((,element *form-element*)
                         (,object *fill-object*))
                     ,@body))))

(define-iterating-fill-processor do-strings (element strings) (split-sequence #\Space strings))

(define-standard-fill-form text (element object &optional (text NIL t-s-p))
  (vector-push-extend
   (dom:create-text-node (slot-value element 'rune-dom::owner) (princ-to-string (if t-s-p text object)))
   (setf (slot-value element 'rune-dom::children)
         (delete-if #'dom:text-node-p (slot-value element 'rune-dom::children)))))

(define-standard-fill-form html (element object &optional (html NIL h-s-p))
  (flet ((resolver (pubid sysid)
           (declare (ignore pubid sysid))
           (flexi-streams:make-in-memory-input-stream nil)))
    (let ((node (cxml:parse (format NIL "<div>~a</div>" (if h-s-p html object)) (cxml-dom:make-dom-builder)
                            :entity-resolver #'resolver :root (slot-value element 'rune-dom::owner))))
      (setf (slot-value element 'rune-dom::children)
            (rune-dom::make-node-list))
      (loop for child across (dom:child-nodes node)
            do (dom:append-child element child)))))

(define-standard-fill-form attr (element object attribute &optional (value NIL v-s-p))
  (dom:set-attribute-ns element NIL (string-downcase attribute) (princ-to-string (if v-s-p value object))))

(define-standard-fill-form object (element object)
  (declare (ignore element))
  object)
