#|
// quicksort

partition(A, p, r)
	x = A[r]
	i = p - 1
	for j = p to r - 1
		if A[j] <= x
			i = i + 1
			exchange A[i] with A[j]
	exchange A[i + 1] with A[r]
	return i + 1

quicksort(A, p, r)
	if p < r
		q = partition(A, p, r)
		quicksort(A, p, q - 1)
		quicksort(A, q + 1, r)

initial: quicksort(A, 1, A.length)
|#

(defparameter *A-list* (list))

(defun exchange (i j)
  (let ((e (nth i *A-list*)))
	(setf (nth i *A-list*) (nth j *A-list*))
	(setf (nth j *A-list*) e)
	))

(defun partition (p r)
  (let ((x (nth r *A-list*))
		(i (- p 1)))
	(do ((j p (+ j 1)))
	  ((= j r))
	  (if (<= (nth j *A-list*) x)
		(let ()
		  (setf i (+ i 1))
		  (exchange i j)
		  )))
	(exchange (+ i 1) r)
	(+ i 1)))

(defun quicksort-1 (p r)
  (if (< p r)
	(let ((q (partition p r)))
	  (quicksort-1 p (- q 1))
	  (quicksort-1 (+ q 1) r)
	  )))

(defun quicksort (a)
  (setf *A-list* a)
  (quicksort-1 0 (1-(length *A-list*)))
  *A-list*)

(let ((l (list))
	  (time-mark 0))
  (dotimes (k (expt 2 8))
	(push (random (+ 10000 (rem (get-universal-time) 10000))) l))
  (format t "~S~%" l)
  (setf time-mark (get-internal-run-time))
  (setf l (quicksort l))
  (setf time-mark (- (get-internal-run-time) time-mark))
  (format t "~S~%" l)
  (format t "time used: ~S ms~%" (float(/ time-mark 1000)))
  )
(quit)
