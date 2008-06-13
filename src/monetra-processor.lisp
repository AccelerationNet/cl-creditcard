(in-package :cl-monetra)

(defclass monetra-processor ()
  ((host)
   (port)
   (conn)
   (user)
   (pass)))

(defmethod sale ((mp monetra-processor) cc-data amount &key bill-address &allow-other-keys))

(defmethod authorize ((mp monetra-processor) cc-data amount &key bill-address &allow-other-keys))

(defmethod preauth-capture ((mp monetra-processor) transaction-id &key amount  ))