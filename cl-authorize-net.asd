(eval-when (:compile-toplevel :load-toplevel :execute)
  (unless (find-package :cl-creditcard.system)
    (defpackage :cl-creditcard.system
      (:use :common-lisp :asdf))))

(in-package :cl-creditcard.system)

(eval-when (:compile-toplevel :load-toplevel :execute)
  (when (find-system 'asdf-system-connections nil)
    (asdf:operate 'asdf:load-op 'asdf-system-connections)))


(defsystem :cl-authorize-net
  :description "Generic interace library for talking with authorize.net credit card processors."
  :author "<programmers@acceleration.net>"
  :licence "LLGPL http://opensource.franz.com/preamble.html (or talk to me)"
  :version "0.1"
  :components
  ((:module :src
	    :serial T
	    :components ((:file "packages")
			 (:file "credit-card-api")
			 (:file "authorize-processor")
			 (:file "authorize-echeck")
			 )))
  :depends-on (:cl-creditcard :drakma :alexandria))

(defsystem :cl-authorize-net-tests
  :description "Talk to the Authorize.net Payment Processing Software. Test Suite."
  :author "<programmers@acceleration.net>"
  :licence "LLGPL http://opensource.franz.com/preamble.html (or talk to me)"
  :version "0.1"
  :components
  ((:module :tests
	    :components ((:file "packages")
			 (:file "authorize" )
			 )))
  :depends-on (:cl-authorize-net :lisp-unit :alexandria))
