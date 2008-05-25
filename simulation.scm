;; Simulation functions

;; Basic read/write operations

(define (write . parms)
  (display "SIMULATION: WRITE: ")
  (for-each (λ (x)
              (display x)
              (display " ")) parms)
  (newline))

(define (read . parms)
  (display "SIMULATION: READ: ")
  (for-each (λ (x)
              (display x)
              (display " ")) parms)
  (newline))


;; Arithmetic tools

(define ash arithmetic-shift)

(define logior bitwise-ior)
