(defpackage :cl-creditcard
    (:documentation "Library for charging credit cards.")
  (:use :common-lisp :iterate)
  (:nicknames :clcc)
  (:export
   ;;major functions
   #:sale #:authorize #:preauth-capture #:void

   #:cc-data
   #:account #:expdate #:name #:ccv #:address #:zip

   #:luhn-check-p
   #:enough-data-p
   
   ;;exceptions
   #:deny-exception 
   ))

#|
(defpackage :cl-monetra
  (:documentation "Libarary for talking with Monetra payment processor")
  (:use :common-lisp :cl+ssl )
  )
|#

(defpackage :cl-authorize-net
    (:use :common-lisp :cl-creditcard))
