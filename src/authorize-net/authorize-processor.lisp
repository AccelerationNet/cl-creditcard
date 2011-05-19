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

(defmacro possibly-return-expected-result (tag)
  `(when (not (eql :unbound *expected-result*))
     (log-it :info "Skipping normal execution, returning *expected-result*: ~a" *expected-result*)
     (return-from ,tag (typecase *expected-result*
			 (function (funcall *expected-result*))
			 (T *expected-result*)))))

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

(defclass authorize-customer-data ()
  #.(mapcar (lambda (x)
	      (list x :accessor x :initform nil :initarg (intern (symbol-name x) :keyword)))
	    +authorize-slots+)
  (:documentation "customer / invoice information")
  )

(defclass authorize-data (cc-data authorize-customer-data) ())

(defun bool-value (x) (if x "TRUE" "FALSE"))

(defun munge-authorize-slot-name (slot-name)
  (format nil "x_~a" (symbol-munger:lisp->underscores slot-name)))

(defparameter +transaction-type+
  '((:auth .  "AUTH_ONLY")
    (:capture-prior-auth .  "PRIOR_AUTH_CAPTURE")
    (:sale . "AUTH_CAPTURE")
    (:void . "VOID")
    (:credit . "CREDIT")))
(defun transaction-type (key)
  (cdr (assoc key +transaction-type+)))


(defmacro with-param-hash ((param-fn-sym &optional params params-sym ) &body body)
  (let ((params-sym (or params-sym (gensym "params"))))
    `(let ((,params-sym ,(or params `(make-hash-table :test 'equalp))))
       (flet ((,param-fn-sym (k v)
		(if v
		    (ensure-gethash k ,params-sym v)
		    (remhash k ,params-sym))
		,params-sym))
	 ,@body)))
  )

(defmethod prebuild-post ((ap authorize-processor) cc-data type)
  (with-param-hash (param)
    (param "x_version" +version+)
    (param "x_delim_data" (bool-value T))
    (param "x_delim_char" +delimiter+)
    (param "x_encap_char" +encapsulater+)
    (param "x_login" (login ap))
    (param "x_tran_key" (trankey ap))
    (param "x_relay_response" (bool-value nil))
    (param "x_test_request" (bool-value (test-mode ap)))
    (param "x_type" (typecase type
		      (string type)
		      (symbol (transaction-type type))))))

(defmethod add-sensitive-data ((cc-data authorize-data) params)
  "adds senstivee data to the given hashtable"
  (with-param-hash (param params)
    (param "x_card_code" (ccv cc-data))
    (param "x_card_num" (account cc-data))
    (param "x_exp_date" (expdate cc-data))))

(defmethod build-post ((ap authorize-processor) type &key cc-data amount transaction-id (include-cc T) &allow-other-keys)
  "returns an alist of strings"  

  (with-param-hash (param (prebuild-post ap cc-data type) params)		  
    (when amount
      (param "x_amount" (etypecase amount
			  (string amount)
			  (number (format nil "~0,2F" amount)))))	
    (param "x_trans_id" transaction-id)
		    
    (when (and include-cc cc-data)
      (add-sensitive-data cc-data params))
	   
    (dolist (slot +authorize-slots+)
      (when-let ((val (ignore-errors (funcall slot cc-data))))
	(param (munge-authorize-slot-name slot)
	       (case slot
		 (recurring-billing (bool-value val))
		 (T val)))))

    (log-it :debug
	    "cl-authorize-net:build-post, results: ~s" 
	    (let ((val (copy-hash-table params))) ;;; dont log sensitive info
	      (when (gethash "x_card_code" val)
		(setf (gethash "x_card_code" val) "HIDDEN-CCV"))

	      ;;mask these
	      (dolist (key '("x_card_num" "x_bank_aba_code" "x_bank_acct_num"))
		(when-let ((x (gethash key val)))
		  (setf (gethash key val)
			(let* ((l (length x))
			       (unmasked (max 1 (min 4 (truncate (/ l 2)))))
			       (masked (- (length x) unmasked)))
			  (format nil "~a~a"
				  (make-string masked :initial-element #\#)
				  (subseq x (- l unmasked)))))))

	      (hash-table-alist val)))

    ;; caller expects alist of strings
    (iter (for (k . v) in (hash-table-alist params))
	  (collect (cons (princ-to-string k)
			 (princ-to-string v))))))

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
  (possibly-return-expected-result process)
  (multiple-value-bind (body status headers uri stream must-close reason-phrase)
      (drakma:http-request (post-url ap)
			   :method :post :force-ssl T :parameters params)
    (declare (ignore uri stream must-close headers))
    (unless (= status 200)
      ;;It looks like the creditcard number isn't sent back, so we should be allright to
      ;; include the body in the error.
      (log-it :error "Server responded with a bad status: ~a ~a~%~a" status reason-phrase body)
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

(defmethod authorize ((ap authorize-processor)
		      cc-data amount &key &allow-other-keys)
  (let* ((*processor* ap)
	 (post-args (build-post ap :auth :cc-data cc-data :amount amount)))
    (process ap post-args)))

(defmethod preauth-capture ((ap authorize-processor)
			    transaction-id &key amount &allow-other-keys)
  (let* ((*processor* ap)
	 (post-args (build-post ap :capture-prior-auth :transaction-id transaction-id :amount amount)))
    (process ap post-args))
  )

(defmethod sale ((ap authorize-processor) cc-data amount &key  &allow-other-keys)
  (let* ((*processor* ap)
	 (post-args (build-post ap :sale :cc-data cc-data :amount amount)))
    (process ap post-args))
  )

(defmethod void ((ap authorize-processor) transaction-id &key &allow-other-keys)
  (let* ((*processor* ap)
	 (post-args (build-post ap :void :transaction-id transaction-id)))
    (process ap post-args))
  )

