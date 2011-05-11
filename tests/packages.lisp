
(defpackage :cl-authorize-tests
  (:documentation "Test Libarary for talking with Authorize.net")
  (:use :common-lisp :cl-creditcard :cl-authorize-net :iterate :lisp-unit)
  (:export ))

(with-package-iterator (sym '(:cl-authorize-net) :internal)
  (iter (multiple-value-bind (more? symbol accessibility pkg) (sym)
	  (declare (ignore accessibility))
	  (when (eql (find-package :cl-authorize-net)
		     pkg)
	    (ignore-errors (import (list symbol) :cl-authorize-tests)))
	  (while more?))))
