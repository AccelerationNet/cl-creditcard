(in-package :cl-monetra)

(defclass monetra-processor ()
  ((host)
   (port)
   (user)
   (pass)))

(defmethod initialize-instance ((monetra-processor monetra-processor)
				&key &allow-other-keys)
  )


(defmethod sale ((mp monetra-processor) cc-data amount &key &allow-other-keys))

(defmethod authorize ((mp monetra-processor) cc-data amount &key &allow-other-keys))

(defmethod preauth-capture ((mp monetra-processor) transaction-id &key amount  ))


