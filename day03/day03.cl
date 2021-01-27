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

(defun index-filter (l f)
  (loop for a across l
	for i upfrom 1
	if (funcall f i) collect a))

(defun part1 (input-file-name)
  (print
   (count-visited-positions (compute-position-list (read-to-string input-file-name)))))

(defun count-visited-positions (l)
  (length
   (remove-duplicates
    l
    :test #'equal)))

(defun compute-position-list (str)
  (reduce
     (lambda (acc val)
       (cons (compute-next-position (car acc) val) acc))
     str
     :initial-value (list (list 0 0))))

(defun part2 (input-file-name)
  (let ((input (read-to-string input-file-name)))
    (print (count-visited-positions (concatenate 'list (compute-position-list (index-filter input 'oddp)) (compute-position-list (index-filter input 'evenp)))))))

(part1 "input")
(part2 "input")
