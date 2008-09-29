(in-package :cl-monetra)

;;;; Processor Functions

(defgeneric sale (processor cc-data amount &key &allow-other-keys)
  (:documentation "Do a one time charge on a credit card for an amount using
the given processor. Other provided keys are up to the processor to interpret
or ignore.

Returns the transaction's unique identifier if successful."))

(defgeneric authorize (processor cc-data amount &key &allow-other-keys)
  (:documentation "Do a pre-authorization without capture returning a
transaction identifier that can later be used with preauth-capture."))

(defgeneric preauth-capture (processor transaction-id &key amount &allow-other-keys )
  (:documentation "Capture a charge that was previously authorized with authorize.
Some processors allow capturing a different amount than was originally authorized."))

(defgeneric void (processor transaction-id &key &allow-other-keys)
  (:documentation "Void a previous transaction."))

;;;; Data Container and functions to validate it


(defclass cc-data ()
  ((account :initarg :account :accessor account :initform nil
	    :documentation "The card number.")
   (expdate :initarg :expdate :accessor expdate :initform nil
	    :documentation "The card expiration date")
   (name :initarg :name :accessor name :initform nil
	 :documentation "The cardholder's name as appears on card.")
   (ccv :initarg :ccv :accessor ccv :initform nil
       :documentation "Card [Code] Verification value. Usually 3-4 digits on back of card.")
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

(defun check-valid (cc-data &key
		    require-avs
		    require-ccv
		    error-p
		    )
  (:documenation "Check that a cc-data like structure has enough data to
complete a transaction.
Can optionally require that AVS verification, Card Verfication data be present.
Can optionally signal an error if data isn't present.

Some merchant accounts will incur extra fees if AVS isn't used."))



;;;; Conditions



;;; example
(let ((ccdata (gather-data-from-form))
      (mp (make-instance 'monetra-processor)))
  (sale mp ccdata 43))
