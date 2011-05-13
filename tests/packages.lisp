
(defpackage :cl-authorize-tests
  (:documentation "Test Libarary for talking with Authorize.net")
  (:use :common-lisp :cl-creditcard :cl-authorize-net :iterate :lisp-unit)
  (:shadowing-import-from #:alexandria #:set-equal)
  (:export ))

