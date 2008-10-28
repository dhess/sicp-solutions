;; The answer to the exercise is slightly different than what's asked
;; for in the text. See
;;
;; http://wiki.drewhess.com/wiki/SICP_exercise_1.22
;;
;; for details.

(define runtime current-milliseconds)

(define (square x)
  (* x x))

(define (smallest-divisor n)
  (find-divisor n 2))

(define (find-divisor n test-divisor)
  (cond ((> (square test-divisor) n) n)
        ((divides? test-divisor n) test-divisor)
        (else (find-divisor n (+ test-divisor 1)))))

(define (divides? a b)
  (= (remainder b a) 0))

(define (prime? n)
  (= n (smallest-divisor n)))

(define (timed-prime-test n)
  (start-prime-test n (runtime)))

(define (start-prime-test n start-time)
  (if (prime? n)
      (report-prime n (- (runtime) start-time))
      #f))

(define (report-prime n elapsed-time)
  (display n)
  (display " (")
  (display elapsed-time)
  (display "ms)\n")
  #t)

(define (search-for-primes a n)
  (search-for-primes-helper (next-odd a) 0 n))

(define (search-for-primes-helper a found n)
  (if (= found n)
      0
      (search-for-primes-helper (+ a 2)
                                (if (timed-prime-test a)
                                    (+ found 1)
                                    found)
                                n)))

(define (next-odd n)
  (if (even? n)
      (+ n 1)
      n))

(define (even? n)
  (= 0 (remainder n 2)))
