(define nil (quote ()))

(define (deep-reverse x)
  (cond ((null? x) nil)
        ((pair? x)
         (append (deep-reverse (cdr x)) (cons (deep-reverse (car x)) nil)))
        (else x)))
