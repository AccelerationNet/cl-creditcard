(in-package :cl-authorize-net)

(defparameter +echeck-bank-acct-types+ '("CHECKING" "BUSINESSCHECKING" "SAVINGS")
  "The valid values for bank-acct-type")

(defparameter +echeck-types+ '("ARC" "BOC" "CCD" "PPD" "TEL" "WEB")
  "The valid values for echeck-type")
(defparameter +supported-echeck-types+ '("CCD" "PPD" "WEB")
  "The valid values for echeck-type")

(defun length-between-p (string low high)
  (and string (stringp string)
       (<= low (length string) high)))

(defclass echeck-data (authorize-customer-data)
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
Must be one of +echeck-types+.  Defaults to WEB")))

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

(defmethod add-sensitive-data ((cc-data echeck-data) params)
  "adds senstivee data to the given hashtable"  
  (with-param-hash (param params)
    (param "x_bank_aba_code" (bank-aba-code cc-data))
    (param "x_bank_acct_num" (bank-acct-num cc-data))))

(defmethod prebuild-post ((ap authorize-processor) (cc-data echeck-data) type )
  "returns a hashtable of post parameters"
  (with-param-hash (param (call-next-method))
    (when (string-equal "WEB" (echeck-type cc-data))
      (param (munge-authorize-slot-name 'recurring-billing)
	     (bool-value (slot-value cc-data 'recurring-billing))))
    (dolist (sym '(bank-name bank-acct-name bank-acct-type echeck-type))
      (param (munge-authorize-slot-name sym)
	     (slot-value cc-data sym)))
    (param "x_type" nil) ;;remove type, it's not valid for echecks
    (param "x_method" "ECHECK")))

(define-condition unsupported-operation (error) ())

(defmethod authorize ((ap authorize-processor)
		      (cc-data echeck-data)
		       amount &key &allow-other-keys)
  (signal 'unsupported-operation))

(defmethod sale ((ap authorize-processor)
		      (cc-data echeck-data)
		       amount &key &allow-other-keys)
  (let* ((*processor* ap)
	 (post-args (build-post ap nil :cc-data cc-data :amount amount)))
    (process ap post-args)))
