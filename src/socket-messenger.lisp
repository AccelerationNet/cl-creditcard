(in-package :cl-monetra)

(declaim (optimize (safety 3) (speed 0) (debug 3)))

(defconstant +STX+ #x02)
(defconstant +FS+ #x1c)
(defconstant +ETX+ #x03)
(defparameter +monetra-enc+ (flex:make-external-format :utf-8))

(defun string-to-octets (s)
  (flex:string-to-octets s :external-format +monetra-enc+))
(defun octets-to-string (o)
  (flex:octets-to-string o :external-format +monetra-enc+))

(defun hash->string (msghash)
  (error "Not yet implemented."))

(defun write-message (sslstream key message)
  (unless (open-stream-p sslstream)
    (error "Attempting to write message to closed stream. ~a" sslstream))
  (flet ((ws (s)
	   (write-sequence (etypecase s
			     (string (string-to-octets s ))
			     (hash-table (string-to-octets (hash->string s)  ))
			     ((array flex:octet) s))
			   sslstream))
	 (wb (b) (write-byte b sslstream)))
    (wb +STX+)
    (ws key)
    (wb +FS+)
    (ws message)
    (wb +ETX+))
  (finish-output sslstream))

(defun read-response (sslstream)
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
	     ;; We have now read one line that is a key=value pair,
	     ;; translate and stick in hash.
	     (let ((k (read-keyword-from-string (octets-to-string keybuf )))
		   (v (octets-to-string valbuf )))
	       (setf (gethash k msghash) v
		     (fill-pointer keybuf) 0 ;reset the buffers.
		     (fill-pointer valbuf) 0
		     buf keybuf	      ;reset the active buffer pointer
		     ))))
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

    (values msghash (octets-to-string idbuf ))))


(defmethod process ((mp monetra-processor) msg
		    &optional (id (arnesi:random-string 8)))
  (let ((usocket (usocket:socket-connect (host mp) (port mp)
					 :element-type 'flexi-streams:octet)))
    (with-open-stream (sslstream (cl+ssl:make-ssl-client-stream
				  (usocket:socket-stream usocket)
				  :close-callback (lambda ()
						    (break "closing sockstream: ~a" usocket)
						    (usocket:socket-close usocket))))
      (write-message sslstream id msg)
      (multiple-value-bind (response-hash response-id) (read-response sslstream)
	(unless (string= response-id id)
	  (error "Received response to a different message(~a) than the one we sent(~a)."
		 response-id id))
	response-hash))))





(defun test ()
  (let* ((usocket (usocket:socket-connect "transact.merchantprovider.com" 8444
					  :element-type 'flexi-streams:octet))
	 (sockstream (usocket:socket-stream usocket))
	 (sslstream (cl+ssl:make-ssl-client-stream
		     sockstream :close-callback (lambda ()
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