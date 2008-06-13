

(in-package :cl-monetra)

(defgeneric sale (processor cc-data amount &key &allow-other-keys))

(defgeneric authorize (processor cc-data amount &key &allow-other-keys))

(defgeneric preauth-capture (processor transaction-id &key amount  ))


(defclass cc-data ()
  ((account :initarg :account :accessor account :initform nil
	    :documentation "The card number.")
   (expdate :initarg :expdate :accessor expdate :initform nil
	    :documentation "The card expiration date")
   (name :initarg :name :accessor name :initform nil
	 :documentation "The cardholder's name as appears on card.")
   (cv :initarg :cv :accessor cv :initform nil
       :documentation "Card Verification value. Usually 3-4 digits on back of card.")
   (address :initarg :address :accessor address :initform nil
	:documentation "Street address for AVS. Typically only numbers need to
be passed in this fields.  Letters and other characters are ignored.")
   (zip :initarg :zip :accessor zip :initform nil
	:documentation "Zipcode for AVS verfication."))
  (:documentation "Class to hold data that normally goes with a credit card charge.
We interact with this class only through the accessors and never actually check
the type, so you can feed it any data structure that responds to these set of accessors."))

(defgeneric luhn-check-p (card-number)
  (:documentation "Check the credit-card number is valid looking based on
the luhn algorithm: http://en.wikipedia.org/wiki/Luhn_algorithm"))

(defgeneric check-valid (cc-data &key
				 require-avs
				 require-cv
				 error-p
				)
  (:documenation "Check that a cc-data like structure has enough data to
complete a transaction.
Can optionally require that AVS verification, Card Verfication data be present.
Can optionally signal an error if data isn't present.

Some merchant accounts will incur extra fees if AVS isn't used."))

