;; Simulation functions

;; Basic read/write operations

(define (write . parms)
;  (display "SIMULATION: WRITE: ")
;  (for-each (λ (x)
;              (display x)
;              (display " ")) parms)
;  (newline)
  'ok)
(define (read port off)
  (display "SIMULATION: READ: ")
  (if (and (= port GPIO_0)
           (= off IOxPIN))
      (begin
;        (display "GPIO_0 pin state requested! Please enter a bitmask.")
        (let ((res #b1000000));(realread)))
          (display "GPIO_0 pin read")
          (newline)
          res))
      (begin
        (display port)
        (display " ")
        (display off)
        (newline)
        0)))


;; Arithmetic tools

(define ash arithmetic-shift)

(define logior bitwise-ior)
(define logand bitwise-and)
(define lognot bitwise-not)
(define logxor bitwise-xor)

(define (wait-ms t)
  (sleep (/ t 1000)))

(define (wait-us t)
  (sleep (/ t 1000000)))

(define (loop proc times)
  (if (> times 0)
      (begin
        (proc)
        (loop proc (- times 1)))))