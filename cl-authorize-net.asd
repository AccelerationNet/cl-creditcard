(eval-when (:compile-toplevel :load-toplevel :execute)
  (unless (find-package :cl-creditcard.system)
    (defpackage :cl-creditcard.system
      (:use :common-lisp :asdf))))

(in-package :cl-creditcard.system)

(defsystem :cl-authorize-net
  :description "library for talking with authorize.net credit card processors."
  :author "<programmers@acceleration.net>"
  :licence "MIT"
  :version "0.1"
  :components
  ((:module :src
	    :serial T
	    :components ((:module :authorize-net
				  :serial T
				  :components ((:file "packages")
					       (:file "authorize-processor")
					       (:file "authorize-echeck"))
				  ))))
  :depends-on (:cl-creditcard :drakma :alexandria :symbol-munger
               :split-sequence))

(defmethod asdf:perform ((o asdf:test-op) (c (eql (find-system :cl-authorize-net))))
  (asdf:load-system :cl-authorize-net-tests)
  (let ((*package* (find-package :cl-authorize-tests)))
    (eval (read-from-string "(run-tests :all)"))))

(defsystem :cl-authorize-net-tests
  :description "Talk to the Authorize.net Payment Processing Software. Test Suite."
  :author "<programmers@acceleration.net>"
  :licence "MIT"
  :version "0.1"
  :components
  ((:module :tests
            :serial T
	    :components ((:file "packages")
			 (:file "authorize" )
			 )))
  :depends-on (:cl-authorize-net :lisp-unit :alexandria))

;; Copyright (c) 2008 Acceleration.net, Russ Tyndall, Ryan Davis, Nathan Bird

;; Permission is hereby granted, free of charge, to any person obtaining
;; a copy of this software and associated documentation files (the
;; "Software"), to deal in the Software without restriction, including
;; without limitation the rights to use, copy, modify, merge, publish,
;; distribute, sublicense, and/or sell copies of the Software, and to
;; permit persons to whom the Software is furnished to do so, subject to
;; the following conditions:

;; The above copyright notice and this permission notice shall be
;; included in all copies or substantial portions of the Software.

;; THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND,
;; EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF
;; MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND
;; NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS BE
;; LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION
;; OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION
;; WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.
