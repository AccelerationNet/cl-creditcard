(setf stream (usocket:socket-connect "transact.merchantprovider.com" 8444 :element-type 'flexi-streams:octet))
(setf stream (usocket:socket-stream stream))
(setf stream (cl+ssl:make-ssl-client-stream stream ))
(write-byte #x2 stream)
(setf fstream (flexi-streams:make-flexi-stream stream))
(write-string "asfd2asdf" fstream)
(write-char (code-char #x1c) fstream)
(write-string msgbody fstream)
(write-char (code-char #x03) fstream)

(defun test ()
  (let* ((usocket (usocket:socket-connect "transact.merchantprovider.com" 8444
					  :element-type 'flexi-streams:octet))
	 (socket (usocket:socket usocket))
;	 (f (setf (sb-bsd-sockets:non-blocking-mode socket) t))
	 (sockstream (usocket:socket-stream usocket))
	 (fd (cl+ssl:stream-fd sockstream))
	 (sslstream (cl+ssl:make-ssl-client-stream
		     fd :close-callback (lambda ()
					  (break "closing sockstream: ~a" sockstream)
					  (close sockstream))))
	 (fstream (flexi-streams:make-flexi-stream sslstream
						   :external-format (flex:make-external-format :utf-8)))
	 (msgbody "username=adwtest
password=testtest
action=chkpwd
"))

    (write-message sslstream "asf324f" msgbody)

    (let (
	  (buf (make-array 1024 :fill-pointer 0 :element-type 'character ))
	  ;;(buf (make-array 128 :element-type 'flexi-streams:octet ))
	  )
      (loop for c = (read-char fstream nil :eof)
	 do
	 (vector-push c buf)
	 until (eq c #\Etx))
      (list buf))

    ))

(defconstant +STX+ #x02)
(defconstant +FS+ #x1c)
(defconstant +ETX+ #x03)
(defparameter +monetra-enc+ (flex:make-external-format :utf-8))

(defun write-message (sslsock key message)
  (unless (open-stream-p sslsock)
    (error "Attempting to write message to closed ssl socket. ~a" sslsock))
  (flet ((ws (s)
	   (write-sequence (etypecase s
			     (string (flex:string-to-octets s :external-format +monetra-enc+))
			     (flex:octet s))
			   sslsock))
	 (wb (b) (write-byte b sslsock)))
    (wb +STX+)
    (ws key)
    (wb +FS+)
    (ws message)
    (wb +ETX+))
  (finish-output sslsock))

;(defun read-response (sslsock))
