(in-package :cl-monetra)

(defclass monetra-processor ()
  ((host :initarg :host :accessor host)
   (port :initarg :port :accessor port :initform 8444)
   (username :initarg :user :accessor username)
   (password :initarg :pass :accessor password)
   (avs-auto-void-codes :initarg :avs-auto-void-codes :initform '("BAD" "ZIP")
			:accessor avs-auto-void-codes
			:documentation "From the CIP spec, AVS results codes (A.7)
  This is because many processors will return authorization even when AVS fails,
  in that case we want to then go void the transaction to avoid higher charges
  to the merchant (and incidentally to aid in preventing theft). By default
  only autovoids on complete fails or zip code fails and ignores the street.
  Add \"STREET\" to the list to include that or set to nil to cancel.")))

(defmethod initialize-instance ((monetra-processor monetra-processor)
				&key &allow-other-keys)
  )

(defmethod base-message ((mp monetra-processor) action &optional cc-data)
  (let ((h (make-hash-table :test 'eq)))
    (setf (gethash :username h) (username mp)
	  (gethash :password h) (password mp)
	  (gethash :action h) action)
    (when cc-data
      (check-valid cc-data :error-p t)
      (setf (gethash :account h) (account cc-data)
	    (gethash :expdate h) (expdate cc-data)
	    (gethash :cardholdername h) (name cc-data)
	    (gethash :cv h) (ccv cc-data)
	    (gethash :street h) (address cc-data)
	    (gethash :zip h) (zip cc-data)))
    h))

(defmethod interpret-response ((mp monetra-processor) msg &key error-p)
  "Attempt to figure out the response to the transaction. Returns (or ttid T) if it is valid
and nil if it is not. If error-p is requested then signal a cc-error.

TODO: test this."

  (let* ((code (or (gethash :code msg)
		   (error "Received unrecognizable response from Monetra: ~a" msg)))
	 (code-sym (read-keyword-from-string code))
	 (ttid (gethash :ttid msg))
	 (verbiage (gethash :verbiage msg)))

    (case code-sym
      ;;; Monetra says it is authorized, but that's hardly the end of the story.
      ;;; Check for various verification failures and deal with them.
      (:AUTH (cond
	       ;;;Condition branch for AVS failure. attempt to void the transaction and signal
	       ;;; that this is denied.
	       ((find (gethash :avs msg) (avs-auto-void-codes mp))
		(if ttid
		    (void mp ttid)
		    (error 'cc-error
			   :format-control "Attempted to auto-void bad AVS code: ~a, but lacks ttid."
			   :format-arguments (gethash :avs msg)
			   :verbiage verbiage))
		(when error-p
		  (signal 'deny-exception
			  :user-message "Couldn't authorize due to address verification problem."
			  :format-control "Couldn't authorize due to AVS code: ~a."
			  :format-arguments (gethash :avs msg)
			  :verbiage verbiage)))

	       ;;; CCV returned bad, void and signal.
	       ((string= "BAD" (gethash :cv msg))
		(if ttid
		    (void mp ttid)
		    (error 'cc-error
			   :format-control "Attempted to auto-void bad CV code: ~a, but lacks ttid."
			   :format-arguments (list (gethash :cv msg))
			   :verbiage verbiage))
		(when error-p
		  (signal 'deny-exception
			  :user-message "Couldn't authorize due to CVV problem."
			  :verbiage verbiage)))
	       ;;; Monetra's internal code indicated something wrong
	       ((not (string= "INT_SUCCESS" (gethash :msoft_code msg)))
		;;TODO: log this.
		;;this isn't a fail condition
		(or ttid t))
	       ;;everything is just good.
	       (T (or ttid t))))

      ;;; Monetra says we should retry the transaction.
      (:RETRY
       (signal 'cc-error
	       :verbiage verbiage
	       :user-message "Credit card processing failed, please retry in a minute."
	       :format-control "Monetra requested retrying the transaction,
                 but we don't yet automatically implement it. phard_code: ~a  msoft_code: ~a"
	       :format-arguments (list (gethash :phard_code msg) (gethash :msoft_code msg))))
      (:TIMEOUT
       (signal 'cc-error
	       :verbiage verbiage
	       :user-message "Credit card processing failed, please retry in a minute."
	       :format-control "Monetra reported timeout. phard_code: ~a  msoft_code: ~a"
	       :format-arguments (list (gethash :phard_code msg) (gethash :msoft_code msg))))
      ;;Credit card denied, or should be confiscated (hard to do here)
      ((:DENY :PKUP)
       (signal 'deny-exception
	       :verbiage verbiage
	       :user-message "Credit card denied."
	       :format-control "Denied: phard_code: ~a  msoft_code:a"
	       :format-arguments (list (gethash :phard_code msg) (gethash :msoft_code msg))))
      (t (signal 'cc-error :verbiage verbiage
		 :user-message "Problem processing credit card, please contact us."
		 :format-control "Unrecognized response, code: ~a  phard_code: ~a  msoft_code: ~a"
		 :format-arguments (list code (gethash :phard_code msg)
					 (gethash :msoft_code msg)))))))


(defmethod sale ((mp monetra-processor) cc-data amount &key &allow-other-keys)
  (let ((msghash (base-message mp "sale" cc-data)))
    (setf (gethash :amount msghash) amount)

    ))

(defmethod authorize ((mp monetra-processor) cc-data amount &key &allow-other-keys))

(defmethod preauth-capture ((mp monetra-processor) transaction-id &key amount  ))
