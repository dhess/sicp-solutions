(define (make-mobile left right)
  (list left right))


(define (make-branch length structure)
  (list length structure))

;; Part A.

(define (left-branch mobile) (car mobile))

(define (right-branch mobile) (car (cdr mobile)))

(define (branch-length branch) (car branch))

(define (branch-structure branch) (car (cdr branch)))

;; Part B

(define (mobile? structure) (pair? structure))

(define (branch-weight branch)
  (let ((structure (branch-structure branch)))
    (if (mobile? structure)
        (total-weight structure)
        structure)))

(define (total-weight mobile)
  (+ (branch-weight (left-branch mobile))
     (branch-weight (right-branch mobile))))

;; Part C

(define (torque branch)
  (* (branch-length branch) (branch-weight branch)))

(define (balanced? mobile)
  (let ((left (left-branch mobile))
        (right (right-branch mobile)))
    (and (= (torque left) (torque right))
         (if (mobile? (branch-structure left)) (balanced? left) #t)
         (if (mobile? (branch-structure right)) (balanced? right) #t))))

;; Part D

(define (make-mobile left right)
  (cons left right))

(define (make-branch length structure)
  (cons length structure))

(define (left-branch mobile) (car mobile))

(define (right-branch mobile) (cdr mobile))

(define (branch-length branch) (car branch))

(define (branch-structure branch) (cdr branch))
