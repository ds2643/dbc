(in-package :dbc)

(defmacro return-exception (expr)
  "Returns the type of exception
   associated with the evaluation
   of expression expr; Returns
   result of evaluation of expr
   in no exceptions occur."
  `(handler-case (eval ,expr)
    (error (c)
      (type-of c))))

(defun create-test-env ()
  (progn
    (defun double (xs)
      (mapcar #'(lambda (x) (* x 2)) xs))
    (defcnt doublee (xs)
        (:pre (listp xs)
         :post t)
      (double xs))))

(create-test-env)

(lisp-unit:define-test pre-conditions
  (let ((observed-exception (return-exception '(doublee 'invalid)))
        (expected-exception 'simple-error))
    (assert-equal observed-exception expected-exception)))

(lisp-unit:run-tests)
