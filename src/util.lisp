(in-package :cl-creditcard)

(defun read-keyword-from-string (str)
  "Safely read a string into the keyword package."
  (let ((*package* (find-package :keyword)) ;want to read in as a keyword
	(*read-eval* nil)) ;just want to be safe(r)
    (read-from-string str)))

(defun replace-all (string part replacement &key (test #'char=))
"Returns a new string in which all the occurences of the part 
is replaced with replacement. [FROM http://cl-cookbook.sourceforge.net/strings.html#manip]"
    (with-output-to-string (out)
      (loop with part-length = (length part)
            for old-pos = 0 then (+ pos part-length)
            for pos = (search part string
                              :start2 old-pos
                              :test test)
            do (write-string string out
                             :start old-pos
                             :end (or pos (length string)))
            when pos do (write-string replacement out)
            while pos)))
