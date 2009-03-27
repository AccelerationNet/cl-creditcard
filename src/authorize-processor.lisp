(in-package :cl-authorize-net)

(defclass authorize-processor ()
  ((login :initarg :login :accessor login)
   (trankey :initarg :trankey :accessor trankey))
  )

(defparameter +version+ "3.1")
(defparameter +post-url+ "https://test.authorize.net:443/gateway/transact.dll")

(defmethod build-basepost ((ap authorize-processor) cc-data type)
  (list
   (cons "x_version" +version+)
   (cons "x_type" type)
   (cons "x_login" (login ap))
   (cons "x_tran_key" (trankey ap))

   (cons "x_card_num" (account cc-data))
   (cons "x_exp_date" (expdate cc-data))

   (cons "x_card_code" (ccv cc-data) )
   (cons "" )
   ))

(defmethod sale ((ap authorize-processor)
		 cc-data amount &key &allow-other-keys)
  )

(defmethod authorize ((ap authorize-processor)
		      cc-data amount &key &allow-other-keys)
  )

(defmethod preauth-capture ((ap authorize-processor)
			    transaction-id &key amount &allow-other-keys)
  )

(defmethod void ((ap authorize-processor) transaction-id &key &allow-other-keys)
  )

