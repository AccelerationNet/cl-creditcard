(in-package :cl-authorize-net)

(defparameter +version+ "3.1")
(defparameter +delimiter+ "|")
(defparameter +encapsulater+ "")
(defparameter +test-post-url+ "https://test.authorize.net:443/gateway/transact.dll"
  ;;Might need to be "https://certification.authorize.net/gateway/transact.dll"
  )
(defparameter +live-post-url+ "https://secure.authorize.net:443/gateway/transact.dll")


(defvar *expected-result* :unbound
  "a variable for use in testing that will bypass the process function and
   return the expected result directly

   If a function, returns the results of running that function
   (allows for multiple values)

  ")

(defvar *log-fn* ()
  "A fn to send log messages to, must conform to: (lambda (category msg-fn)).
 * category will be one of: '(:debug, :info, :warn, :error, :fatal)
 * msg-fn is a lambda that generates the actual message")

(defmacro log-it (category format-string &rest args)
  `(when *log-fn*
     (ignore-errors
       (funcall *log-fn* ,category
		#'(lambda () (format nil ,format-string ,@args) )))))

(defvar *processor* nil
  "A variable to be bound to a processor for internal use")

(defclass authorize-processor ()
  ((login :initarg :login :accessor login )
   (trankey :initarg :trankey :accessor trankey )
   (test-mode
    :accessor test-mode :initarg :test-mode :initform nil
    :documentation "Whether or not we are testing, bool, if set to :full we will also use the +test-post-url+ instead of the +live-post-url+")
   ))

(defmethod post-url ((ap authorize-processor))
  (case (test-mode ap)
    (:full +test-post-url+)
    (T +live-post-url+)))

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defparameter +authorize-slots+
    '(first-name last-name company email address city state zip country phone fax cust-id customer-ip recurring-billing invoice-num description)))

(defclass authorize-data (cc-data)
  #.(mapcar (lambda (x)
	      (list x :accessor x :initform nil :initarg (intern (symbol-name x) :keyword)))
	    +authorize-slots+))

(defparameter +echeck-bank-acct-types+ '("CHECKING" "BUSINESSCHECKING" "SAVINGS")
  "The valid values for bank-acct-type")

(defparameter +echeck-types+ '("ARC" "BOC" "CCD" "PPD" "TEL" "WEB")
  "The valid values for echeck-type")
(defparameter +supported-echeck-types+ '("CCD" "PPD" "WEB")
  "The valid values for echeck-type")

(defun length-between-p (string low high)
  (and string (stringp string)
       (<= low (length string) high)))

(defclass echeck-data ()
  ((bank-aba-code :initarg :bank-aba-code :accessor bank-aba-code :type (string 9)
		  :documentation "The valid routing number of the customer's bank.
Must be a string of 9 digits.")
   (bank-acct-num :initarg :bank-acct-num :accessor bank-acct-num :type (string)
		  :documentation "The customer's valid bank account number.
Must be a string of up to 20 digits")
   (bank-acct-type :initarg :bank-acct-type :accessor bank-acct-type :type (string)
		   :documentation "The type of bank account.
Must be one of +echeck-bank-acct-types+")
   (bank-name :initarg :bank-name :accessor bank-name :type (string)
	      :documentation "The name of the bank that holds the customer's account.
Must be a string of up to 50 characters")
   (bank-acct-name :initarg :bank-acct-name :accessor bank-acct-name :type (string)
		   :documentation "The name of the bank that holds the customer's account.
Must be a string of up to 50 characters")
   (echeck-type :initarg :echeck-type :accessor echeck-type :type (string)
		:initform "WEB"
		:documentation "The type of electronic check transaction.
Must be one of +echeck-types+.  Defaults to WEB")
   (recurring-billing :initarg :recurring-billing
		      :accessor recurring-billing :initform nil
		      :documentation "The recurring stats of the transaction.  Required only when echeck-type is WEB.  Defaults to nil")))

(define-condition invalid-echeck-data (error)
  ((echeck-data :initarg :echeck-data :accessor echeck-data
		:documentation "the problematic echeck data")))

(defmethod initialize-instance :after ((self echeck-data) &key &allow-other-keys)
  (unless (and (stringp (bank-acct-type self))
	       (member (bank-acct-type self) +echeck-bank-acct-types+ :test #'string=))
    (signal 'invalid-echeck-data "bad bank-acct-type" :echeck-data self))
  (unless (and (typep (bank-aba-code self) '(string 9))
	       (every #'digit-char-p (bank-aba-code self)))
    (signal 'invalid-echeck-data "bad bank-aba-code" :echeck-data self))
  (unless (and (length-between-p (bank-acct-num self) 1 20)
	       (every #'digit-char-p (bank-acct-num self)))
    (signal 'invalid-echeck-data "bad bank-acct-num" :echeck-data self))
  (unless (length-between-p (bank-name self) 1 50)
    (signal 'invalid-echeck-data "bad bank-name" :echeck-data self))
  (unless (length-between-p (bank-acct-name self) 1 50)
    (signal 'invalid-echeck-data "bad bank-acct-name" :echeck-data self))
  (unless (and (stringp (echeck-type self))
	       (member (echeck-type self) +echeck-types+ :test #'string=))
    (signal 'invalid-echeck-data "bad echeck-type" :echeck-data self))
  (unless (and (stringp (echeck-type self))
	       (member (echeck-type self) +supported-echeck-types+ :test #'string=))
    (signal 'invalid-echeck-data "unsupported echeck-type" :echeck-data self)))

(defun bool-value (x) (if x "TRUE" "FALSE"))

(defun munge-authorize-slot-name (slot-name)
  (let ((name (cl-creditcard::replace-all (string-downcase (string slot-name)) "-" "_")))
    (format nil "x_~a" name)))

(defun slot-to-authorize-cons (data slot)
  (let ((it (ignore-errors (funcall slot data))))
    (when it
      (cons (munge-authorize-slot-name slot)
	    (case slot
	      (recurring-billing (bool-value it))
	      (T it))))))

(defparameter +transaction-type+
  '((:auth .  "AUTH_ONLY")
    (:capture-prior-auth .  "PRIOR_AUTH_CAPTURE")
    (:sale . "AUTH_CAPTURE")
    (:void . "VOID")
    (:credit . "CREDIT")))
(defun transaction-type (key)
  (cdr (assoc key +transaction-type+)))

(defmethod prebuild-post ((ap authorize-processor) cc-data type )
  (list
   (cons "x_version" +version+)
   (cons "x_delim_data" (bool-value T))
   (cons "x_delim_char" +delimiter+)
   (cons "x_encap_char" +encapsulater+)
   (cons "x_type" (typecase type
		    (string type)
		    (symbol (transaction-type type))))
   (cons "x_login" (login ap))
   (cons "x_tran_key" (trankey ap))
   (cons "x_relay_response" (bool-value nil))
   (cons "x_test_request" (bool-value (test-mode ap)))))

(defmethod build-post ((ap authorize-processor) type &key cc-data amount transaction-id (include-cc T) &allow-other-keys)
  (flet ((slot-def (s) (slot-to-authorize-cons cc-data s)))
    (let ((rtn
	   (iter (for (k . v) in (append (prebuild-post ap cc-data type)
					 (when amount
					   (list (cons "x_amount" (etypecase amount
								    (string amount)
								    (number (format nil "~0,2F" amount))))))
					 (when transaction-id
					   (list (cons "x_trans_id" transaction-id)))
					 (when (and include-cc cc-data)
					   (append
					    (when (ccv cc-data)
					      (list (cons "x_card_code" (ccv cc-data))))
					    (list (cons "x_card_num" (account cc-data))
						  (cons "x_exp_date" (expdate cc-data))
						  )))
					 (mapcar #'slot-def +authorize-slots+)
					 ))
		 (when (and k v)
		   (collect (cons (princ-to-string k) (princ-to-string v)))))))
      (log-it :debug
	      "cl-authorize-net:build-post, results: ~s"
	      ;;; dont log sensitive info
	      (let* ((val (copy-alist rtn))
		     (cc (find "x_card_num" val :key #'car :test #'string-equal ))
		     (ccv (find "x_card_code" val :key #'car :test #'string-equal )))
		(when cc
		  (setf (cdr cc) (format nil "#############~a"
					 (subseq (cdr cc) (- (length (cdr cc)) 4)))))
		(when ccv (setf (cdr ccv) "HIDDEN-CCV"))
		val))
      rtn
      )))

(defun args-to-query-string (alist)
  (format nil "~{~A~^&~}"
	  (loop for (k . v) in alist
		collect (format nil "~A=~A" k v))))


(defun get-response-vars (s)
  (when (or (null s) (= 0 (length s)))
    (error 'cc-error
	   :user-message "There was an error with the response from the credit card processor."
	   :format-control "Response from server was 0 length."))
  (let* ((flat-list (split-sequence:split-sequence +delimiter+ s :test #'string=))
	 (len (length flat-list))
	 (response (loop for v in flat-list
			 for k in '(:response-code :response-subcode :response-reason-code
				    :response-reason-text :authorization-code :avs-response
				    :transaction-id :invoice-number :description
				    :amount :method :transaction-type :customer-id )
			 collect (cons k v))))
    (when (>= len 38)
      (push (cons :card-code-response (nth 38 flat-list)) response))
    (log-it :debug
	    "cl-authorize-net:get-response-vars from:~a,~%results: ~s"
	    (post-url *processor*) response)
    response))

(defun response-value (key r)
  (cdr (assoc key r)))

(defmethod process ((ap authorize-processor) params)
  (multiple-value-bind (body status headers uri stream must-close reason-phrase)
      (drakma:http-request (post-url ap)
			   :method :post :force-ssl T :parameters params)
    (declare (ignore uri stream must-close headers))
    (unless (= status 200)
      ;;It looks like the creditcard number isn't sent back, so we should be allright to
      ;; include the body in the error.
      (error 'cc-error
	     :user-message "There was an error with the response from the credit card processor."
	     :format-control "Server responded with a bad status: ~a ~a~%~a"
	     :format-arguments status reason-phrase body))

    (let* ((pairs (get-response-vars body))
	   (response-code (let ((*read-eval* nil)
				(val (response-value :response-code pairs)))
			    (when val (read-from-string val)))))
      ;http://developer.authorize.net/guides/AIM/Transaction_Response/Response_Reason_Codes_and_Response_Reason_Text.htm
      (values
	(when response-code
	  (case response-code
	    (1 ;transaction approved
	       (response-value :transaction-id pairs))
	    (2 ;Transaction declined
	       nil)
	    (3 ;error processing transaction
	       nil)
	    (4 ;held for review
	       nil)))
	pairs))))

(defmacro possibly-return-expected-result (tag)
  `(when (not (eql :unbound *expected-result*))
     (return-from ,tag (typecase *expected-result*
			 (function (funcall *expected-result*))
			 (T *expected-result*)))))

(defmethod authorize ((ap authorize-processor)
		      cc-data amount &key &allow-other-keys)
  (possibly-return-expected-result authorize)
  (let* ((*processor* ap)
	 (post-args (build-post ap :auth :cc-data cc-data :amount amount)))
    (process ap post-args)))

(defmethod preauth-capture ((ap authorize-processor)
			    transaction-id &key amount &allow-other-keys)
  (possibly-return-expected-result preauth-capture)
  (let* ((*processor* ap)
	 (post-args (build-post ap :capture-prior-auth :transaction-id transaction-id :amount amount)))
    (process ap post-args))
  )

(defmethod sale ((ap authorize-processor) cc-data amount &key  &allow-other-keys)
  (possibly-return-expected-result sale)
  (let* ((*processor* ap)
	 (post-args (build-post ap :sale :cc-data cc-data :amount amount)))
    (process ap post-args))
  )

(defmethod void ((ap authorize-processor) transaction-id &key &allow-other-keys)
  (possibly-return-expected-result void)
  (let* ((*processor* ap)
	 (post-args (build-post ap :void :transaction-id transaction-id)))
    (process ap post-args))
  )

