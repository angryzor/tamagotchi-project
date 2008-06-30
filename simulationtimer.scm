(define (wait-ms t)
  (sleep (/ t 1000)))

(define (wait-us t)
  (sleep (/ t 1000000)))


(define timatreset 0)
(define timatstop 0)
(define timpr 0)
(define timrunning #f)

(define (timer.set-PR timer val)
  (set! timpr (/ (+ val 1) 60000)))

(define (timer.start timer)
  (set! timrunning #t))

(define (timer.stop timer)
  (set! timatstop (current-inexact-milliseconds))
  (set! timrunning #f))

(define (timer.reset timer)
  (set! timatreset (current-inexact-milliseconds)))

(define (timer.value timer)
  (if timrunning
      (/ (- (current-inexact-milliseconds) timatreset)
         timpr)
      (/ (- timatstop timatreset)
         timpr)))

(define lighttogo 0.91)

(define (measure o)
  (case o
    ('ain0 0)
    ('ain1 (let ((lnow lighttogo))
             (set! lighttogo 0.5)
             lnow))))