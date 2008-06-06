(load "input-base.scm")

(define (input-digital gpio pin-id)
  (define (init)
    (input-pin (pin pin-id)))
  
  (define (this-check)
    (read-pin pin-id))
  
  (define inpt (input-base this-check))
  
  (define (input-digital-object msg . args)
    (let ((my-param (make-param args 'input-digital-object)))
      (case msg
        (else (apply inpt msg args)))))
  
  (init)
  
  input-digital-object)
