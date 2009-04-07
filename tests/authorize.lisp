(in-package :cl-authorize-tests)

(deftestsuite cl-authorize-tests () ())

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



(addtest (cl-authorize-tests)
  test-prepare-args
  (ensure-same
   (args-to-query-string +authorize-test-alist+)
   "x_version=3.1&x_delim_data=TRUE&x_delim_char=|&x_type=test&x_login=testlogin&x_tran_key=testtrankey&x_amount=1.00&x_card_num=4111111111111111&x_exp_date=01/11&x_card_code=000&x_test_request=TRUE")
  :report "The fake args should turn into a query string style string"
  )


(addtest (cl-authorize-tests)
  test-authorize
  (let* ((processor (test-authorize-processor))
	(data (make-test-cc-date))
	)
    (multiple-value-bind (tranid pairs) (authorize processor data "1.00")
      (ensure-same tranid "0" :Test #'string=
		   :report "The test should authorize and return a transaction code of 0")
      (ensure-same (response-value :amount pairs)  "1.00"  :Test #'string=
		   :report "The test should authorize an amount of 1$ which is what we passed"))
    (multiple-value-bind (tranid pairs) (authorize processor data "2.00")
      ;(break "Decline? ~a ~a" tranid pairs)
      (ensure-same tranid nil 
		   :report "This should be a decline")
      (ensure-same (response-value :amount pairs) "2.00"  :Test #'string=
		   :report "The test should decline to authorize an amount of 2$ which is what we passed") 
    )))

(addtest (cl-authorize-tests)
  test-sale
  (let* ((processor (test-authorize-processor))
	(data (make-test-cc-date))
	)
    (multiple-value-bind (tranid pairs) (sale processor data "1.00")
      (ensure-same tranid "0" :Test #'string=
		   :report "The test should authorize and return a transaction code of 0")
      (ensure-same (response-value :amount pairs)  "1.00"  :Test #'string=
		   :report "The test should authorize an amount of 1$ which is what we passed"))

    
    ))
  

(addtest (cl-authorize-tests)
  test-void
  (let* ((processor (test-authorize-processor))
	 (data (make-test-cc-date)))
    (multiple-value-bind (tranid pairs) (sale processor data "1.00")
      (ensure-same tranid "0" :Test #'string=
		   :report "The test should authorize and return a transaction code of 0")
      (ensure-same (response-value :amount pairs)  "1.00"  :Test #'string=
		   :report "The test should authorize an amount of 1$ which is what we passed"))

    
    ))