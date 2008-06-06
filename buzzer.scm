(define (buzzer port mpin)
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
    (blink (round (/ 1000000 440))))
  
  (define (buzzer-object msg . args)
    (let ((my-param (make-param args 'buzzer-object)))
      (case msg
        ('beep (beep (my-param 1)))
        (else (error 'buzzer-object "message \"~S\" unknown" msg)))))
  
  (init)
  
  buzzer-object)
