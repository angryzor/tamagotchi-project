;(define (tone freq)
;  (round (/ 1000000
;            freq)))

(define (buzzer port mpin)
  ;  (define blinkbool #t)
  ;  (define (blinkcb)
  ;    (if blinkbool
  ;        (set-pin GPIO_0 pin)
  ;        (clear-pin GPIO_0 pin))
  ;    (set! blinkbool (not blinkbool)))
  ;  
  ;  
  ;  (define (playtone timer tone)
  ;    (setup-timer tone)
  ;    (start-timer)
  ;    (wait-us (round len))
  ;    (stop-timer))
  
  (define (init)
    (output-pin (pin mpin)))
  
  (define (beep time)
    (define (blink delay)
      (define (iter itime)
        (if (> itime 0)
            (begin (set-pin (pin mpin))
                   (wait-us delay)
                   (clear-pin (pin mpin))
                   (wait-us delay)
                   (iter (- itime 1)))))
      (iter (/ time delay 2)))
    (blink (/ 1000000 440)))
  
  (define (buzzer-object msg . args)
    (let ((my-param (make-param args 'buzzer-object)))
      (case msg
        ('beep (beep (my-param 1)))
        (else (error 'buzzer-object "message \"~S\" unknown" msg)))))
  
  (init)
  
  buzzer-object)