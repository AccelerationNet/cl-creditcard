
(defpackage :cl-authorize-tests
  (:documentation "Test Libarary for talking with Authorize.net")
  (:use :common-lisp :cl-creditcard :cl-authorize-net :iterate :lisp-unit)
  (:shadowing-import-from #:alexandria #:set-equal)
  (:export ))

(with-package-iterator (sym '(:cl-authorize-net) :internal :external)
  (iter (multiple-value-bind (more? symbol accessibility pkg) (sym)
	  (declare (ignore accessibility))
	  (when (eql (find-package :cl-authorize-net)
		     pkg)
	    (ignore-errors
	      (unintern symbol :cl-authorize-tests)
	      (import (list symbol) :cl-authorize-tests)))
	  (while more?))))
