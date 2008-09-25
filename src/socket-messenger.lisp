(in-package :cl-monetra)

(declaim (optimize (safety 3) (speed 0) (debug 3)))

(defconstant +STX+ #x02)
(defconstant +FS+ #x1c)
(defconstant +ETX+ #x03)
(defparameter +monetra-enc+ (flex:make-external-format :utf-8))

(defun write-message (sslstream key message)
  (unless (open-stream-p sslstream)
    (error "Attempting to write message to closed stream. ~a" sslstream))
  (flet ((ws (s)
	   (write-sequence (etypecase s
			     (string (flex:string-to-octets s :external-format +monetra-enc+))
			     (flex:octet s))
			   sslstream))
	 (wb (b) (write-byte b sslstream)))
    (wb +STX+)
    (ws key)
    (wb +FS+)
    (ws message)
    (wb +ETX+))
  (finish-output sslstream))

(defun message-from-stream (sslstream)
  (unless (and (input-stream-p sslstream)
	       (subtypep (stream-element-type sslstream) 'flexi-streams:octet)
	       (open-stream-p sslstream))
    (error "Stream must be an open binary input stream. ~a" sslstream))
  ;;make sure we have a start tx byte.
  (let ((b (read-byte sslstream)))
    (unless (= +STX+ b)
      (error "Expected STX(~a) on stream, found ~a" +STX+ b)))
  ;;read the message id
  (let ((idbuf (make-array 8 :element-type 'flexi-streams:octet :adjustable t :fill-pointer 0 ))
	(keybuf (make-array 16 :element-type 'flexi-streams:octet :adjustable t :fill-pointer 0 ))
	(valbuf (make-array 16 :element-type 'flexi-streams:octet :adjustable t :fill-pointer 0 ))
	(msghash (make-hash-table :test 'eq))
	(buf))			       ;pointer to which one is active
    (flet ((finish-line ()
	     (let ((*package* (find-package :keyword)) ;want to read in as a keyword
		   (*read-eval* nil))	;just want to be safe(r)
	       (let ((k (read-from-string (octets-to-string keybuf :external-format +monetra-enc+)))
		     (v (octets-to-string valbuf :external-format +monetra-enc+)))
		 (setf (gethash k msghash) v
		       (fill-pointer keybuf) 0 ;reset the buffers.
		       (fill-pointer valbuf) 0
		       buf keybuf     ;reset the active buffer pointer
		       )))))
      ;; Gathering up the ID is pretty direct, just read till #\FS
      (loop for b = (read-byte sslstream)
	 until (= b +FS+)
	 do (vector-push-extend b idbuf))

      (setf buf keybuf)			;start pointing at the key
      (loop
	 for b = (read-byte sslstream)
	 until (= b +ETX+)
	 do (case b
	      (13 (finish-line))   ;CR end of line, switch back to keybuf
	      (10)	           ;LF ignore
	      (#.(char-code #\=) (setf buf valbuf)) ; = indicates start of value
	      (t (vector-push-extend b buf))) ;otherwise(data) add to active buf
	 ))

    (values msghash (octets-to-string idbuf :external-format +monetra-enc+))))

(defun test ()
  (let* ((usocket (usocket:socket-connect "transact.merchantprovider.com" 8444
					  :element-type 'flexi-streams:octet))
	 (socket (usocket:socket usocket))
;;	 (f (setf (sb-bsd-sockets:non-blocking-mode socket) t))
	 (sockstream (usocket:socket-stream usocket))
	 (fd (cl+ssl:stream-fd sockstream))
	 (sslstream (cl+ssl:make-ssl-client-stream
		     fd :close-callback (lambda ()
					  (break "closing sockstream: ~a" sockstream)
					  (close sockstream))))

	 (msgbody "username=adwtest
password=testtest
action=chkpwd
"))

    (write-message sslstream "asf324f" msgbody)


    '(let (
	  (buf (make-array 1024 :fill-pointer 0 :element-type 'octet ))
	  ;;(buf (make-array 128 :element-type 'flexi-streams:octet ))
	  )
      (loop for c = (read-byte sslstream)
	 until (= c +etx+)
	 do
	   (vector-push c buf) )

      buf)
    (message-from-stream sslstream)))