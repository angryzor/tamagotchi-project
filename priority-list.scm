(load "heap.scm")

(define (priority-list size <?)
  (define hp (heap size (λ (x y) (not (<? x y)))))
  