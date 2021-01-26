(defun read-to-string (filename)
  (car (with-open-file (stream filename)
    (loop for line = (read-line stream nil)
	  while line
	  collect line))))

(defun compute-next-position (last-position character)
  (cond
    ((eql character #\>) (list (1+ (car last-position)) (car (last last-position))))
    ((eql character #\<) (list (1- (car last-position)) (car (last last-position))))
    ((eql character #\^) (list (car last-position) (1+ (car (last last-position)))))
    ((eql character #\v) (list (car last-position) (1- (car (last last-position)))))))

(defun part1 (input-file-name)
   (print (length (remove-duplicates (reduce
    (lambda (acc val)
      (cons
       (compute-next-position (car acc) val) acc))
    (read-to-string input-file-name)
    :initial-value (list (list 0 0))) :test #'equal))))

(part1 "input")
