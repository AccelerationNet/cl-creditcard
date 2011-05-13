(defpackage :cl-authorize-net-example
    (:use :cl :cl-user))

(in-package :cl-authorize-net-example)

(defvar *test-mode* T)

(defun authorize-processor (&key (test-mode *test-mode*))
  (make-instance 'cl-authorize-net:authorize-processor
		 :login "your-login" 
		 :trankey "your-transaction-key"
		 :test-mode test-mode))

(defun charge-cc ( amount &key (recurring T) )
  "returns either a creditcard or a memo transaction stating that
   the card was declined for some reason"      
  (let* ((cc-processor (authorize-processor)) 
	 (cc-data (make-instance
		   'cl-authorize-net:authorize-data
		   :account "4111111111111111"
		   :expdate "09/2020"
		   :first-name "First"
		   :last-name "Last"
		   :email "customer@email.com"
		   :company "customer company"
		   :zip "32606"
		   :address "4701 N Main ST."
		   :cust-id 4111
		   :invoice-num 14123
		   :recurring-billing recurring)))
    ;; returns (values transaction-id response)
    ;; you only get a transaction-id if the sale was successful 
    (cl-authorize-net:sale cc-processor cc-data amount)
    ))