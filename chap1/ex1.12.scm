(define (pascals-triangle row index)
  (cond
   ((or (= index 1) (= index row)) 1)
   (else (+ (pascals-triangle (- row 1) (- index 1))
            (pascals-triangle (- row 1) index)))))
