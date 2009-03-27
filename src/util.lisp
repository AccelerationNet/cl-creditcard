(in-package :cl-monetra)

(defun read-keyword-from-string (str)
  "Safely read a string into the keyword package."
  (let ((*package* (find-package :keyword)) ;want to read in as a keyword
	(*read-eval* nil)) ;just want to be safe(r)
    (read-from-string str)))
