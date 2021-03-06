(in-package :cl-creditcard)

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

(defgeneric credit (processor cc-data &key transaction-id amount &allow-other-keys )
  (:documentation "Credit a charge, if transaction-id is present this is a credit against a precviouly charged authorization"))

;;;; Data Container and functions to validate it


(defclass cc-data ()
  ((account :initarg :account :accessor account :initform nil
	    :documentation "The card number.")
   (expdate :initarg :expdate :accessor expdate :initform nil
	    :documentation "The card expiration date")
;   (name :initarg :name :accessor name :initform nil
;	 :documentation "The cardholder's name as appears on card.")
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

(defgeneric enough-data-p (cc-data &key
		    require-avs
		    require-ccv
		    error-p
		    )
  (:documentation "Check that a cc-data like structure has enough data to
complete a transaction.
Can optionally require that AVS verification, Card Verfication data be present.
Can optionally signal an error if data isn't present.

Some merchant accounts will incur extra fees if AVS isn't used."))



;;;; Conditions

(define-condition cc-error (simple-error)
  ((user-message :initarg :user-message :accessor user-message :initform nil)
   (verbiage :initarg :verbiage
	     :accessor verbiage
	     :documentation "human-interpretable response code
            (meant for system display to clerk)"))
  (:documentation "Requested errors when processing the credit will inherit
from here. user-message, when supplied, should be a safe string to display to
clients informing them of the error."))

(define-condition deny-exception (cc-error)
  ()
  (:documentation "Error signalled when the CC is denied and error signalling
 is requested."))

;;; example
;(let ((ccdata (gather-data-from-form))
;      (mp (make-instance 'monetra-processor)))
;  (sale mp ccdata 43))
