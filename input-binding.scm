(define (input-binding inpt proc)
  (define (check)
    (let ((res (inpt 'check)))
      (if res
          (proc))
      res))
  
  (define (input-binding-object msg . args)
    (let ((my-param (make-param args 'input-binding-object)))
      (case msg
        ('check (check))
        (else (error 'input-binding-object "message \"~S\" unknown" msg)))))
  
  input-binding-object)
