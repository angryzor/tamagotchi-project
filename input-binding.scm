(define (input-binding inpt proc)
  (define (check)
    (if (inpt 'check)
        (proc)))
  
  (define (input-binding-object msg . args)
    (let ((my-param (make-param args 'input-binding-object)))
      (case msg
        ('check (check))
        (else (error 'input-binding-object "message \"~S\" unknown" msg)))))
  
  input-binding-object)
