#|

Insertion sort
Input: A sequence of n numbers (a1, a2, ..., an)
Output: A permuntation(reordering) (a1', a2', ..., a'n) of then input 
	sequence such that a1'<= a2'<= ...<= a'n

  inserortion_sort(A)
  	for j = 1 to A.length - 1 
  		key = A[j]
  		//insert A[j] into the sorted sequence A[1..j-1]
  		i = j - 1
  		while i < 0 nor A[i] < key
  			A[i + 1] = A[i]
  			i = i - 1
  		A[i + 1] = key

|#

(defun insertion-sort (inl)
  (do*((a inl a)
	   (j 1 (+ j 1))
	   (key (nth j a) (nth j a)))
	((= j (length a)) a)
	(do ((i (- j 1) (- i 1)))
	  ((or (< i 0) (< (nth i a) key))
	   (setf (nth (+ i 1) a) key))
	  (setf (nth (+ i 1) a) (nth i a)))))

(let ((a (list))
	  (time-mark 0))
  (dotimes (k 2000)
	(push (random (+ 10000 (rem (get-universal-time) 100))) a))
  (format t "~S~%" a)
  (setf time-mark (get-internal-run-time))
  (setf a (insertion-sort a))
  (setf time-mark (- (get-internal-run-time) time-mark))
  (format t "~S~%" a)
  (format t "time used: ~S ms~%" (float(/ time-mark 1000)))
  )
(quit)
