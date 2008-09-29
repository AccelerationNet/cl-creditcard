(in-package :cl-monetra)

(defclass monetra-processor ()
  ((host :initarg :host :accessor host)
   (port :initarg :port :accessor port :initform 8444)
   (username :initarg :user :accessor username)
   (password :initarg :pass :accessor password)))

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

(defmethod interpret-response (msg &key error-p)

  (let* ((code (or (gethash :code msg)
		   (error "Received unrecognizable response from Monetra: ~a" msg)))
	 (code-sym
	  (let (())))
	 (ttid (gethash :ttid msg)))

    (switch (read-from-string code)
      ("AUTH"))))


(defmethod sale ((mp monetra-processor) cc-data amount &key &allow-other-keys)
  (let ((msghash (base-message mp "sale" cc-data)))
    (setf (gethash :amount msghash) amount)

    ))

(defmethod authorize ((mp monetra-processor) cc-data amount &key &allow-other-keys))

(defmethod preauth-capture ((mp monetra-processor) transaction-id &key amount  ))
