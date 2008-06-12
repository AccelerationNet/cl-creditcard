(eval-when (:compile-toplevel :load-toplevel :execute)
  (unless (find-package :cl-monetra.system)
    (defpackage :cl-monetra.system
      (:use :common-lisp :asdf))))

(in-package :cl-monetra.system)

(eval-when (:compile-toplevel :load-toplevel :execute)
  (when (find-system 'asdf-system-connections nil)
    (asdf:operate 'asdf:load-op 'asdf-system-connections)))

(defsystem :adwcodebase
    :description "Talk to the Monetra Payment Processing Software."
    :author "<programmers@acceleration.net>"
    :licence "LLGPL http://opensource.franz.com/preamble.html (or talk to me)"
    :version "0.1"
    :components
    ((:module :src
	      :components ((:file "packages")
			   (:file "credit-card-api")

			   )))
    :depends-on (:asdf-system-connections ))
