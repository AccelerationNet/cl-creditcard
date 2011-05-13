(defpackage :cl-authorize-net
    (:use :common-lisp :cl-creditcard  :iterate)
  (:shadowing-import-from #:alexandria #:ensure-gethash #:hash-table-alist #:copy-hash-table #:when-let)
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
   #:first-name #:last-name #:company #:address #:city #:email #:state #:zip #:country #:phone #:fax #:cust-id #:customer-ip #:recurring-billing #:invoice-num

   #:*expected-result*
   #:*log-fn*
   ))
