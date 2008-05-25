; Vraag 4
;+++++++++++

(define (make-random m a seed)
  (define x seed)
  (λ ()
    (set! x (modulo (* x a) m))
    (exact->inexact (/ x m))))


; Vraag 5
;++++++++++

(define (make-random2 m a seed)
  (define x seed)
  (define (generate)
    (set! x (modulo (* x a) m))
    (exact->inexact (/ x m)))
  (define (reset)
    (set! x seed))
  (λ msg
    (cond ((or (null? msg) (eq? (car msg) 'generate)) (generate))
          ((eq? (car msg) 'reset) (reset))
          (else (error 'make-random2 "Unknown message: ~S" (car msg))))))

(define (make-random3 m a seed)
  (define x seed)
  (lambda ()
    (display x)
    (display m)
    (display a)
    (set! x (modulo (* x a) m))
    (display x)
    (display m)
    (display a)
    (/ x m)
    (display x)
    (display m)
    (display a)))

(define rand (make-random3 (- (expt 2 32) 1) (expt 7 5) 97))

(define a 
    (make-random3 54951563 36519 97))