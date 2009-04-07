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
   #:test-mode
   ))

#|
(defpackage :cl-monetra
  (:documentation "Libarary for talking with Monetra payment processor")
  (:use :common-lisp :cl+ssl )
  )
|#

(defpackage :cl-authorize-net
    (:use :common-lisp :cl-creditcard  :iterate)
  (:export
   ;;major functions
   #:sale #:authorize #:preauth-capture #:void
   
   #:cc-data #:authorize-data :authorize-processor
   #:test-mode
   #:account #:expdate #:name #:ccv #:address #:zip

   #:response-value
   
   #:luhn-check-p
   #:enough-data-p
   
   ;;exceptions
   #:deny-exception

   ;;Extra data
   #:first-name #:last-name #:company #:address #:city #:state #:zip #:country #:phone #:fax #:cust-id #:customer-ip #:recurring-billing #:invoice-num
   ))
