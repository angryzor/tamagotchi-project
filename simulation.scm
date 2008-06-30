;; Simulation functions

;; Basic read/write operations

(define (write . parms)
  'ok)
(define (read port off)
  (if (and (= port GPIO_0)
           (= off IOxPIN))
      (begin
        (let ((res 0))
          res))
      (begin
        0)))


;; Arithmetic tools

(define ash arithmetic-shift)

(define logior bitwise-ior)
(define logand bitwise-and)
(define lognot bitwise-not)
(define logxor bitwise-xor)

(define (loop proc times)
  (if (> times 0)
      (begin
        (proc)
        (loop proc (- times 1)))))

