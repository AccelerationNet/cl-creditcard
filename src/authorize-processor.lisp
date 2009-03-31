(in-package :cl-authorize-net)

(defparameter +version+ "3.1")
(defparameter +delimiter+ "|")
(defparameter +encapsulater+ "\"")
(defparameter +test-post-url+ "https://test.authorize.net:443/gateway/transact.dll")
(defparameter +live-post-url+ "https://secure.authorize.net:443/gateway/transact.dll")

(defclass authorize-processor ()
  ((login :initarg :login :accessor login)
   (trankey :initarg :trankey :accessor trankey)
   (test-mode
    :accessor test-mode :initarg :test-mode :initform nil
    :documentation "Whether or not we are testing, bool")
   ))

(defmethod post-url ((ap authorize-processor))
  (if (test-mode ap) +test-post-url+ +live-post-url+))

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defparameter +authorize-slots+
    '(first-name last-name company address city state zip country phone fax cust-id customer-ip)))

(defclass authorize-data (cc-data)
  #.(mapcar (lambda (x)
	      (list x :accessor x :initform nil :initarg (intern (symbol-name x) :keyword)))
	    +authorize-slots+))

(defun bool-value (x) (if x "TRUE" "FALSE"))

(defun munge-authorize-slot-name (slot-name)
  (let ((name (cl-creditcard::replace-all (string-downcase (string slot-name)) "-" "_")))
    (format nil "x_~a" name)))

(defun slot-to-authorize-cons (data slot)
  (let ((it (funcall slot data)))
    (when it
      (cons (munge-authorize-slot-name slot) it))))

(defmethod prebuild-post ((ap authorize-processor) cc-data type )
  (list
   (cons "x_version" +version+)
   (cons "x_delim_data" (bool-value T))
   (cons "x_delim_char" +delimiter+)
   ;;(cons "x_encap_char" +encapsulater+)
   (cons "x_type" type)
   (cons "x_login" (login ap))
   (cons "x_tran_key" (trankey ap))
   (cons "x_relay_response" (bool-value nil))
   (cons "x_test_request" (bool-value (test-mode ap)))))

(defmethod build-charge-post ((ap authorize-processor) cc-data type amount)
  (flet ((slot-def (s) (slot-to-authorize-cons cc-data s)))
    (append (prebuild-post ap cc-data type)
	    (list (cons "x_amount" amount)
		  (cons "x_card_num" (account cc-data))
		  (cons "x_exp_date" (expdate cc-data))
		  (cons "x_card_code" (ccv cc-data)))
	    (mapcar #'slot-def +authorize-slots+)
	    )))

(defun args-to-query-string (alist)
  (format nil "~{~A~^&~}"
	  (loop for (k . v) in alist
		collect (format nil "~A=~A" k v))))



(defmethod sale ((ap authorize-processor)
		 cc-data amount &key &allow-other-keys)
  
  )


(defun get-response-vars (s)
  (let ((state :key) (idx 0) key)
      (iter
	(case state
	  (:key (let ((newi (position #\= s :start idx)))
		  (unless newi (finish))
		  (setf
		   key (subseq s idx newi)
		   idx (+ 1 newi)
		   state :open-encap)))
	  (:open-encap
	     (assert (char-equal (elt s idx) +encapsulater+))
	     (incf idx)
	     (setf state :value))
	  (:value
	     (let* ((newi idx))
	       (iter (for end = (position +encapsulater+ s :start newi))
		     (setf newi end)
		     (while (char-equal #\\ (elt s (- end 1))))
		     (finally (return end)))
	       (collect (cons key (subseq s idx newi)))
	       (setf state :delimiter idx (+ newi 1))))
	  (:delimiter (setf state :key) (incf idx))))))

(defmethod process ((ap authorize-processor) params)
  (let* ((s (drakma:http-request
			 (post-url ap)
			 :method :post :force-ssl T :parameters params
			 ))
	 (pairs (split-sequence:split-sequence )))
    
    
    ))

(defmethod authorize ((ap authorize-processor)
		      cc-data amount &key &allow-other-keys)
  (let* ((post-args (build-charge-post ap cc-data "AUTH_ONLY" amount))
	 (results (process ap post-args)))
    results
    ))

(defmethod preauth-capture ((ap authorize-processor)
			    transaction-id &key amount &allow-other-keys)
  )

(defmethod void ((ap authorize-processor) transaction-id &key &allow-other-keys)
  )

