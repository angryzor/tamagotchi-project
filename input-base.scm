(define (input-base init-extra proc-that-gets-input)
  (define (check)
    (proc-that-gets-input))
  
  (define (input-base msg . args)
    (let ((my-param (make-param args 'input-base-object)))
      (case msg
        ('check (check))
        (else (error 'input-base-object "message \"~S\" unknown" msg)))))
  (init-extra)
  input-base-object)
