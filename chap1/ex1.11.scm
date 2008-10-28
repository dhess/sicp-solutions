; Recursive process:

(define (f-recursive n)
  (if (< n 3)
      n
      (+ (f-recursive (- n 1))
         (* 2 (f-recursive (- n 2)))
         (* 3 (f-recursive (- n 3))))))

; Iterative process: count up from 3 to n, keep track of f(n-1),
; f(n-2) and f(n-3) as we go.

(define (f-iterative n)
  (define (iter-f n count f1 f2 f3)
    (if (> count n)
        f1
        (iter-f n
                (+ count 1)
                (+ f1 (* 2 f2) (* 3 f3))
                f1
                f2)))
  (if (< n 3)
      n
      (iter-f n 3 2 1 0)))
