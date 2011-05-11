(in-package :cl-authorize-tests)

(with-package-iterator (sym '(:cl-authorize-net) :internal :external)
  (iter (multiple-value-bind (more? symbol accessibility pkg) (sym)
	  (declare (ignore accessibility))
	  (when (eql (find-package :cl-authorize-net)
		     pkg)
	    (ignore-errors
	      (unintern symbol :cl-authorize-tests)
	      (import (list symbol) :cl-authorize-tests)))
	  (while more?))))

(defun test-authorize-processor ()
  (make-instance 'authorize-processor :login "cnpdev4289" :trankey "SR2P8g4jdEn7vFLQ" :test-mode :full))

(defun make-test-cc-date ()                
  (make-instance 'authorize-data :account "4222222222222" :expdate "01/11"))

(defparameter +authorize-test-alist+
  (list
   (cons "x_version" "3.1")
   (cons "x_delim_data" (cl-authorize-net::bool-value T))
   (cons "x_delim_char"  "|")
   
   (cons "x_type" "test")
   (cons "x_login" "testlogin")
   (cons "x_tran_key" "testtrankey")
   (cons "x_amount" "1.00")
   
   (cons "x_card_num" "4111111111111111")
   (cons "x_exp_date" "01/11")
   (cons "x_card_code" "000")
   
   (cons "x_test_request" (cl-authorize-net::bool-value T))))


(define-test log-test
  (let* ((i 0)
	 (*log-fn* #'(lambda (category msg-fn)
		       (assert-eq :debug category)
		       (assert-eq 0 i "should not have evaluated any code yet")
		       (assert-equal "foo 0" (funcall msg-fn))
		       )))
    (log-it :debug "foo 0")
    (log-it :debug "foo ~d" i)
    (log-it :debug (progn (incf i) "foo 0"))))

(define-test test-prepare-args
  (assert-equal "x_version=3.1&x_delim_data=TRUE&x_delim_char=|&x_type=test&x_login=testlogin&x_tran_key=testtrankey&x_amount=1.00&x_card_num=4111111111111111&x_exp_date=01/11&x_card_code=000&x_test_request=TRUE"
		(args-to-query-string +authorize-test-alist+)
		"The fake args should turn into a query string style string"	
		))

(define-test test-authorize
  (let ((processor (test-authorize-processor))
	(data (make-test-cc-date)))
    (multiple-value-bind (tranid pairs) (authorize processor data "1.00")
      (assert-equal "0" tranid
		    "The test should authorize and return a transaction code of 0")
      (assert-equal "1.00"  (response-value :amount pairs)
		    "The test should authorize an amount of 1$ which is what we passed"))
    (multiple-value-bind (tranid pairs) (authorize processor data "2.00")      
      (assert-false tranid "This should be a decline")
      (assert-equal "2.00" (response-value :amount pairs)
		    "The test should decline to authorize an amount of 2$ which is what we passed") 
      )))

(define-test test-sale
  (let ((processor (test-authorize-processor))
	(data (make-test-cc-date)))
    (multiple-value-bind (tranid pairs) (sale processor data "1.00")
      (assert-equal "0" tranid
		    "The test should authorize and return a transaction code of 0")
      (assert-equal "1.00"  (response-value :amount pairs)
		    "The test should authorize an amount of 1$ which is what we passed"))
    ))
