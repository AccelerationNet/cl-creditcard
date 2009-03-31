(in-package :cl-authorize-tests)

(deftestsuite cl-authorize-tests () ())

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
  test-get-response-vars
  (ensure-same
   (cl-authorize-net::get-response-vars "foo=\"bar\"|back=\"bast\"")
   '(("foo" . "bar") ("back" . "bast")) :test #'equalp )
  :report "The fake args should turn into a query string style string"
  )