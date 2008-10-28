(define nil (quote ()))

(define (filter predicate sequence)
  (cond ((null? sequence) nil)
        ((predicate (car sequence))
         (cons (car sequence)
               (filter predicate (cdr sequence))))
        (else (filter predicate (cdr sequence)))))

(define (enumerate-interval low high)
  (if (> low high)
      nil
      (cons low (enumerate-interval (+ low 1) high))))

(define (accumulate op initial sequence)
  (if (null? sequence)
      initial
      (op (car sequence)
          (accumulate op initial (cdr sequence)))))

(define (flatmap proc seq)
  (accumulate append nil (map proc seq)))

(define (queens board-size) 
  (define (queen-cols k)  
    (if (= k 0) 
        (list empty-board) 
        (filter 
         (lambda (positions) (safe? k positions)) 
         (flatmap 
          (lambda (rest-of-queens) 
            (map (lambda (new-row) 
                   (adjoin-position new-row k rest-of-queens)) 
                 (enumerate-interval 1 board-size)))
          (queen-cols (- k 1)))))) 
  (queen-cols board-size))

(define empty-board nil)

(define (adjoin-position new-row column rest-of-queens)
  (cons new-row rest-of-queens))

(define (safe? column positions)
  (define (next-column-safe? new-row positions row-offset)
    (if (null? positions)
        #t
        (let ((this-row (car positions)))
          (if (or (= this-row new-row)
                  (= (+ this-row row-offset) new-row)
                  (= (- this-row row-offset) new-row))
              #f
              (next-column-safe? new-row (cdr positions) (+ 1 row-offset))))))
  (next-column-safe? (car positions) (cdr positions) 1))
