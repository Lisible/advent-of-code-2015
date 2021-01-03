(ql:quickload "split-sequence")

(defstruct dimension l w h)

(defun day02 ()
  (let ((input (read-lines "input")))
    (part1 input)))

(defun part1 (lines)
  (print (reduce #'+ (map 'list (lambda (line) (wrapping-paper-surface (parse-dimension line))) lines))))

(defun parse-dimension (line)
  (let ((split-dimension-string (split-sequence:SPLIT-SEQUENCE #\x line)))
    (make-dimension :l (parse-integer (elt split-dimension-string 0)) :w (parse-integer (elt split-dimension-string 1)) :h (parse-integer (elt split-dimension-string 2)))))

(defun wrapping-paper-surface (dim)
  (let ((w (dimension-w dim))
	(l (dimension-l dim))
	(h (dimension-h dim)))
  (+ (* 2 l w) (* 2 w h) (* 2 h l) (min (* l w) (* w h) (* h l)))))

(defun read-lines (filepath)
  (with-open-file (stream filepath)
    (loop for line = (read-line stream nil)
	  while line
	  collect line)))

(day02)
