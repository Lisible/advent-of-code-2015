(defun day1 (input)
  (part1 input)
  (part2 input))

(defun part1 (input)
  (print (compute-floor-number input)))

(defun compute-floor-number (input)
  (cond ((eq (length input) 0) 0)
	((char= (char input 0) #\() (+ (compute-floor-number (subseq input 1)) 1))
	((char= (char input 0) #\)) (- (compute-floor-number (subseq input 1)) 1))))

(defun part2 (input)
  (print (compute-floor-number-until input -1 0 0)))

(defun compute-floor-number-until (input until-floor current-floor current-index)
  (if (eq current-floor until-floor)
      current-index
      (cond ((eq (length input) current-index) 0)
	    ((char= (char input current-index) #\() (compute-floor-number-until input until-floor (+ current-floor 1) (+ current-index 1)))
	    ((char= (char input current-index) #\)) (compute-floor-number-until input until-floor (- current-floor 1) (+ current-index 1))))))

(defun read-file-to-string (filename)
  (with-open-file (stream filename)
    (loop for line = (read-line stream nil)
	  while line
	  collect line)))

(day1 (car (read-file-to-string "input")))
