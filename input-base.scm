(define (input-base proc-that-gets-input)
  (define (check)
    (proc-that-gets-input))
  
  (define (input-base-object msg . args)
    (let ((my-param (make-param args 'input-base-object)))
      (case msg
        ('check (check))
        (else (error 'input-base-object "message \"~S\" unknown" msg)))))
  input-base-object)
