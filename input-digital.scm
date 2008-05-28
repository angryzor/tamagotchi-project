(load "input-base.scm")

(define (input-digital gpio pin-id)
  (define (this-init)
    (input-pin (pin pin-id)))
  
  (define (this-check)
    (read-pin pin-id))
  
  (define inpt (input-base this-init this-check))
  
  (define (input-digital-object msg . args)
    (let ((my-param (make-param args 'input-digital-object)))
      (case msg
        (else (apply inpt msg args)))))
  
  input-digital-object)
