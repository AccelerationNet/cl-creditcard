(eval-when (:compile-toplevel :load-toplevel :execute)
  (unless (find-package :cl-creditcard.system)
    (defpackage :cl-creditcard.system
      (:use :common-lisp :asdf))))

(in-package :cl-creditcard.system)

(eval-when (:compile-toplevel :load-toplevel :execute)
  (when (find-system 'asdf-system-connections nil)
    (asdf:operate 'asdf:load-op 'asdf-system-connections)))

(defsystem :cl-creditcard
  :description "Generic interace library for talking with credit card processors."
  :author "<programmers@acceleration.net>"
  :licence "LLGPL http://opensource.franz.com/preamble.html (or talk to me)"
  :version "0.1"
  :components
  ((:module :src
	    :components ((:file "packages")
			 (:file "credit-card-api" :depends-on ("packages"))
			 )))
  :depends-on ())

(defsystem :cl-monetra
    :description "Talk to the Monetra Payment Processing Software."
    :author "<programmers@acceleration.net>"
    :licence "LLGPL http://opensource.franz.com/preamble.html (or talk to me)"
    :version "0.1"
    :components
    ((:module :src
	      :components ((:file "packages")
			   (:file "credit-card-api" :depends-on ("packages"))
			   (:file "socket-messenger" :depends-on ("packages"))

			   )))
    :depends-on (:cl-creditcard :cl+ssl :flexi-streams :usocket))

(defsystem :cl-monetra-tests
  :description "Talk to the Monetra Payment Processing Software. Test Suite."
  :author "<programmers@acceleration.net>"
  :licence "LLGPL http://opensource.franz.com/preamble.html (or talk to me)"
  :version "0.1"
  :components
  ((:module :tests
	    :components ((:file "socket-messenger-tests.lisp" )
			 )))
  :depends-on (:cl-monetra :lift))