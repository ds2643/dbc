(in-package :dbc)

(defmacro defcnt (name lambda-list contract &body body)
  "Defines a function with optional pre and post conditions.
   Presently, only single predicates are permitted, arguments
   are out of scope for post, and post is applied over a single
   return values (multiple return values not tested).
   e.g.,
    (defcnt doublee (xs)
        (:pre (listp xs)
        :post t)
      (mapcar #'(lambda (x) (* x 2)) xs)) "
  (destructuring-bind (&key (pre nil pre-prov)
                            (post nil post-prov)) contract
    `(defun ,name ,lambda-list
       ,@(when pre-prov (list `(assert ,pre)))
       ,(alexandria:with-gensyms (res)
          `(let ((,res (progn ,@body)))
             ,res)))))
