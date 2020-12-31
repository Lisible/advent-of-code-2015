(defun day1 (input)
  (print (compute-floor-number input)))

(defun compute-floor-number (input)
  (cond ((eq (length input) 0) 0)
	((char= (char input 0) #\() (+ (compute-floor-number (subseq input 1)) 1))
	((char= (char input 0) #\)) (- (compute-floor-number (subseq input 1)) 1))))


(defun read-file-to-string (filename)
  (with-open-file (stream filename)
    (loop for line = (read-line stream nil)
	  while line
	  collect line)))

(day1 (car (read-file-to-string "input")))
