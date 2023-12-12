#|

// bubble sort

bubblesort(A)
	for i = 0 to A.length - 2
		for j = A.length - 1 downto i 
			if A[j] < A[j - 1]
				k = A[j]
				A[j] = A[j - 1]
				A[j - 1] = k

|#

(defun bubble-sort (inl)
  (let ((A inl))
	(do ((i 0 (+ i 1)))
	  ((= i (1-(length A))) A)
	  (do ((j (1-(length A)) (- j 1)))
		((= j i))
		(if (< (nth j A) (nth (1- j) A))
		  (let ((k (nth j A)))
			(setf (nth j A) (nth (1- j) A))
			(setf (nth (1- j) A) k)))))))

(let ((a (list))
	  (time-mark 0))
  (dotimes (k (expt 2 8))
	(push (random (+ 10000 (rem (get-universal-time) 100))) a))
  (format t "~S~%" a)
  (setf time-mark (get-internal-run-time))
  (setf a (bubble-sort a))
  (setf time-mark (- (get-internal-run-time) time-mark))
  (format t "~S~%" a)
  (format t "time used: ~S ms~%" (float(/ time-mark 1000)))
  )
(quit)
