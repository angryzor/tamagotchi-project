(load "input-base.scm")

(define (input-analog adc-in-id trigger-proc)
  
  (define (this-check)
    (trigger-proc (round (* 100 (measure adc-in-id)))))
  
  (define inpt (input-base this-check))
  
  (define (input-analog-object msg . args)
    (let ((my-param (make-param args 'input-analog-object)))
      (case msg
        (else (apply inpt msg args)))))
  
  input-analog-object)
