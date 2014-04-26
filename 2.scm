;; 2.1

(define zero
  (lambda () '(0)))

(define zero?
  (lambda (n)
    (and (eqv? 0 (car n))
         (null? (cdr n)))))

(define number-reduce
  (lambda (n)
    (cond
     ((null? n) n)
     ((eqv? 16 (car n)) (cons 0
                              (if (null? (cdr n)) '(1)
                                  (successor (cdr n)))))
     ((eqv? -1 (car n)) (cons 15
                              (if (null? (cdr n)) '(-1)
                                  (predecessor (cdr n)))))
     (else n))))

(define successor
  (lambda (n)
    (number-reduce
     (cons (1+ (car n)) (cdr n)))))

(define predecessor
  (lambda (n)
    (number-reduce
     (cons (- (car n) 1) (cdr n)))))

;; 2.3
(define one
  (lambda () '(one)))

(define diff
  (lambda (t1 t2)
    (list 'diff t1 t2)))

(define diff?
  (lambda (t)
    (eqv? (car t) 'diff)))

(define diff-left
  (lambda (t)
    (cadr t)))

(define diff-right
  (lambda (t)
    (caddr t)))

;; (2)
(define zero
  (lambda ()
    (diff (one) (one))))

(define zero?
  (lambda (t)
    (equal? t '(diff (one) (one)))))

(define successor
  (lambda (t)
    (cond
     ((and (diff? t)
           (equal? (diff-right t) (one)))
      (diff-left t))
     (else
      (diff t (diff (zero) (one)))))))

(define predecessor
  (lambda (t)
    (cond
     ((and (diff? t)
           (equal? (diff-right t) (diff (zero) (one))))
      (diff-left t))
     (else
      (diff t (one))))))

;; (3)
(define diff-tree-plus
  (lambda (t1 t2)
    (diff t1 (diff (zero) t2))))
