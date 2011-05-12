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

(defun make-test-cc-data ()
  (make-instance 'authorize-data :account "4222222222222" :expdate "01/11"))

(defparameter +authorize-test-alist+
  (list
   (cons "x_version" "3.1")
   (cons "x_delim_data" (bool-value T))
   (cons "x_delim_char"  "|")

   (cons "x_type" "test")
   (cons "x_login" "testlogin")
   (cons "x_tran_key" "testtrankey")
   (cons "x_amount" "1.00")

   (cons "x_card_num" "4111111111111111")
   (cons "x_exp_date" "01/11")
   (cons "x_card_code" "000")

   (cons "x_test_request" (bool-value T))))


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

(defmacro with-logging (&body body)
  `(let ((*log-fn* #'(lambda (category msg-fn)
		       (format T "~%~a: ~a" category (funcall msg-fn)))))
     ,@body))

(define-test test-prepare-args
  (assert-equal "x_version=3.1&x_delim_data=TRUE&x_delim_char=|&x_type=test&x_login=testlogin&x_tran_key=testtrankey&x_amount=1.00&x_card_num=4111111111111111&x_exp_date=01/11&x_card_code=000&x_test_request=TRUE"
		(args-to-query-string +authorize-test-alist+)
		"The fake args should turn into a query string style string"))

(define-test test-authorize
  (with-logging
    (let ((processor (test-authorize-processor))
	  (data (make-test-cc-data)))
      (multiple-value-bind (tranid pairs) (authorize processor data "1.00")
	(assert-equal "0" tranid
		      "The test should authorize and return a transaction code of 0")
	(assert-equal "1.00"  (response-value :amount pairs)
		      "The test should authorize an amount of 1$ which is what we passed"))
      (multiple-value-bind (tranid pairs) (authorize processor data "2.00")
	(assert-false tranid "This should be a decline")
	(assert-equal "2.00" (response-value :amount pairs)
		      "The test should decline to authorize an amount of 2$ which is what we passed")))))

(define-test test-sale
  (with-logging 
    (let ((processor (test-authorize-processor))
	  (data (make-test-cc-data)))
      (multiple-value-bind (tranid pairs) (sale processor data "1.00")
	(assert-equal "0" tranid
		      "The test should authorize and return a transaction code of 0")
	(assert-equal "1.00"  (response-value :amount pairs)
		      "The test should authorize an amount of 1$ which is what we passed")))))

(defmacro assert-equal-post (expected form &rest extras)
  `(assert-false (set-difference ,expected ,form :test #'equalp)
		,@extras))

(define-test test-build-post
  (with-logging
    (let ((processor (test-authorize-processor))
	  (data (make-test-cc-data))
	  (ccv-data (make-test-cc-data))
	  (echeck-web-data (make-instance 'echeck-data
					  :bank-aba-code "123456789"
					  :bank-acct-num "123"
					  :bank-name "test"
					  :bank-acct-name "test"
					  :bank-acct-type (first +echeck-bank-acct-types+)))
	  (echeck-ccd-data (make-instance 'echeck-data
					  :bank-aba-code "123456789"
					  :bank-acct-num "123"
					  :bank-name "test"
					  :bank-acct-name "test"
					  :echeck-type "CCD"
					  :bank-acct-type "BUSINESSCHECKING")))
      (setf (ccv ccv-data) "000")
      (assert-equal-post
       '(("x_version" . "3.1") ("x_delim_data" . "TRUE") ("x_delim_char" . "|")
	 ("x_encap_char" . "") ("x_type" . "AUTH_ONLY")
	 ("x_login" . "cnpdev4289") ("x_tran_key" . "SR2P8g4jdEn7vFLQ")
	 ("x_relay_response" . "FALSE") ("x_test_request" . "TRUE")
	 ("x_amount" . "1.00") ("x_card_num" . "4222222222222")
	 ("x_exp_date" . "01/11"))
       (build-post processor :auth :cc-data data :amount 1))
      
      (assert-equal-post 
       '(("x_version" . "3.1") ("x_delim_data" . "TRUE") ("x_delim_char" . "|")
	 ("x_encap_char" . "") ("x_type" . "AUTH_ONLY")
	 ("x_login" . "cnpdev4289") ("x_tran_key" . "SR2P8g4jdEn7vFLQ")
	 ("x_relay_response" . "FALSE") ("x_test_request" . "TRUE")
	 ("x_amount" . "1.00") ("x_card_code" . "000")
	 ("x_card_num" . "4222222222222")
	 ("x_exp_date" . "01/11"))
       (build-post processor :auth :cc-data ccv-data :amount 1))
      
      (assert-equal-post '(("x_version" . "3.1") ("x_delim_data" . "TRUE")
			   ("x_delim_char" . "|") ("x_encap_char" . "") ("x_type" . "AUTH_ONLY")
			   ("x_login" . "cnpdev4289") ("x_tran_key" . "SR2P8g4jdEn7vFLQ")
			   ("x_relay_response" . "FALSE") ("x_test_request" . "TRUE")
			   ("x_amount" . "1.00"))
			 (build-post processor :auth :cc-data data :amount 1 :include-cc nil))
      
      (assert-equal-post '(("x_version" . "3.1") ("x_delim_data" . "TRUE")
			   ("x_delim_char" . "|") ("x_encap_char" . "") ("x_type" . "AUTH_ONLY")
			   ("x_login" . "cnpdev4289") ("x_tran_key" . "SR2P8g4jdEn7vFLQ")
			   ("x_relay_response" . "FALSE") ("x_test_request" . "TRUE")
			   ("x_amount" . "1.00"))
			 (build-post processor :auth :cc-data ccv-data :amount 1
				     :include-cc nil))

      
      (assert-equal-post '(("x_login" . "cnpdev4289") ("x_tran_key" . "SR2P8g4jdEn7vFLQ")
			   ("x_amount" . "1.00") ("x_method" . "ECHECK")
			   ("x_bank_aba_code" . "123456789") ("x_bank_acct_num" . "123")
			   ("x_bank_acct_type" . "CHECKING") ("x_bank_name" . "test")
			   ("x_bank_acct_name" . "test") ("x_echeck_type" . "WEB")
			   ("x_recurring_billing" . "FALSE"))
			 (build-post processor nil
				     :cc-data echeck-web-data
				     :amount 1))
      
      (assert-equal-post '(("x_login" . "cnpdev4289") ("x_tran_key" . "SR2P8g4jdEn7vFLQ")
			   ("x_amount" . "1.00") ("x_method" . "ECHECK")
			   ("x_bank_aba_code" . "123456789") ("x_bank_acct_num" . "123")
			   ("x_bank_acct_type" . "BUSINESSCHECKING") ("x_bank_name" . "test")
			   ("x_bank_acct_name" . "test") ("x_echeck_type" . "CCD"))
			 (build-post processor nil
				     :cc-data echeck-ccd-data
				     :amount 1))
      (assert-equal-post '(("x_login" . "cnpdev4289") ("x_tran_key" . "SR2P8g4jdEn7vFLQ")
			   ("x_amount" . "1.00") ("x_method" . "ECHECK")
			   ("x_bank_acct_type" . "CHECKING") ("x_bank_name" . "test")
			   ("x_bank_acct_name" . "test") ("x_echeck_type" . "WEB")
			   ("x_recurring_billing" . "FALSE"))
			 (build-post processor nil
				     :cc-data echeck-web-data
				     :amount 1 :include-cc nil))
      
      (assert-equal-post '(("x_login" . "cnpdev4289") ("x_tran_key" . "SR2P8g4jdEn7vFLQ")
			   ("x_amount" . "1.00") ("x_method" . "ECHECK")
			   ("x_bank_acct_type" . "BUSINESSCHECKING") ("x_bank_name" . "test")
			   ("x_bank_acct_name" . "test") ("x_echeck_type" . "CCD"))
			 (build-post processor nil
				     :cc-data echeck-ccd-data
				     :amount 1 :include-cc nil)))))

(define-test dont-log-sensitive-data
  (let ((processor (test-authorize-processor))
	(data (make-test-cc-data))
	(ccv-data (make-test-cc-data))
	(echeck-data (make-instance 'echeck-data
				    :bank-aba-code "123456789"
				    :bank-acct-num "123"
				    :bank-name "test"
				    :bank-acct-name "test"
				    :bank-acct-type (first +echeck-bank-acct-types+)))
	(*log-fn* (lambda (category msg-fn)
		    (declare (ignore category))
		    (let ((msg (funcall msg-fn)))
		      (assert-false (search "\"4222222222222\"" msg)
				    "should not find CC number!")
		      (assert-false (search "\"000\"" msg)
				    "should not find CCV number!")
		      (assert-false (search "\"123456789\"" msg)
				    "should not find aba code!")
		      (assert-false (search "\"123\"" msg)
				    "should not find bank acct number!" msg)))))
    (setf (ccv ccv-data) "000")
    (build-post processor :auth :cc-data data :amount 1)
    (build-post processor :auth :cc-data ccv-data :amount 1)
    (build-post processor nil :cc-data echeck-data :amount 1)))


(define-test echeck-constraints
  (let ((valid-data (make-instance 'echeck-data
				   :bank-aba-code "123456789"
				   :bank-acct-num "123"
				   :bank-name "test"
				   :bank-acct-name "test"
				   :bank-acct-type (first +echeck-bank-acct-types+))))
    (assert-true valid-data "all's OK")
    (labels ((assert-invalid (what-were-testing val
						&key (bank-aba-code (bank-aba-code valid-data))
						(bank-acct-type (bank-acct-type valid-data))
						(bank-acct-num (bank-acct-num valid-data))
						(bank-name (bank-name valid-data))
						(bank-acct-name (bank-acct-name valid-data))
						(echeck-type (echeck-type valid-data))
						)
	       (assert-error 'invalid-echeck-data
			     (make-instance 'echeck-data
					    :bank-aba-code bank-aba-code
					    :bank-acct-type bank-acct-type
					    :bank-acct-num bank-acct-num
					    :bank-name bank-name
					    :bank-acct-name bank-acct-name
					    :echeck-type echeck-type)
			     "should only allow valid data" what-were-testing val))
	     (assert-bad-values (initarg &rest bad-values)
	       (dolist (bad bad-values)
		 (apply #'assert-invalid (list initarg bad initarg bad)))))

      (assert-bad-values :bank-acct-type "bad" nil "CHECKING " "")
      (assert-bad-values :bank-aba-code "bad" nil 5 ""
			 "1234567890" ;;too long
			 "12345678"   ;; too short
			 "a12345678"  ;;right length, non-numeric
			 )
      (assert-bad-values :bank-acct-num "bad" nil 5 ""
			 "123456789012345678901" ;; too long
			 "a2345678901234567890" ;; right length, non-numeric
			 )

      (assert-bad-values :bank-name 5 nil ""
			 "testingtestingtestingtestingtestingtestingtesting!!" ;; too long
			 )

      (assert-bad-values :bank-acct-name 5 nil ""
			 "testingtestingtestingtestingtestingtestingtesting!!" ;; too long
			 )
      (assert-bad-values :echeck-type 5 nil "" "asdf" "ARC" "BOC" "TEL"))))
