(load "input-base.scm")

(define (input-analog adc-in-id trigger-proc)
  (define inpt (input-base this-init this-check))
  
  (define (this-init)
    'ok) ;; init PINSEL reg?
  
;;  Old version
;;  (define (this-check)
;;    (let ((scale-intr (- scale-max scale-min)))
;;      (* (- (* 100 (measure adc-in-id)) scale-min)
;;         (/ 100 scale-intr))))
  (define (this-check)
    (trigger-proc (round (* 100 (measure adc-in-id)))))
  
  (define (input-analog-object)
    (let ((my-param (make-param args 'input-analog-object)))
      (case msg
        (else (apply inpt msg args)))))
  
  input-analog-object)
