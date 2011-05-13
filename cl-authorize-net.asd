(eval-when (:compile-toplevel :load-toplevel :execute)
  (unless (find-package :cl-creditcard.system)
    (defpackage :cl-creditcard.system
      (:use :common-lisp :asdf))))

(in-package :cl-creditcard.system)

(eval-when (:compile-toplevel :load-toplevel :execute)
  (when (find-system 'asdf-system-connections nil)
    (asdf:operate 'asdf:load-op 'asdf-system-connections)))


(defsystem :cl-authorize-net
  :description "library for talking with authorize.net credit card processors."
  :author "<programmers@acceleration.net>"
  :licence "MIT"
  :version "0.1"
  :components
  ((:module :src
	    :serial T
	    :components ((:file "packages")
			 (:file "credit-card-api")
			 (:file "authorize-processor")
			 )))
  :depends-on (:cl-creditcard :drakma))

(defsystem :cl-authorize-net-tests
  :description "Talk to the Authorize.net Payment Processing Software. Test Suite."
  :author "<programmers@acceleration.net>"
  :licence "MIT"
  :version "0.1"
  :components
  ((:module :tests
	    :components ((:file "packages")
			 (:file "authorize" )
			 )))
  :depends-on (:cl-authorize-net :lift))

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
