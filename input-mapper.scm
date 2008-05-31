(load "input-context.scm")
(load "stack.scm")

(define (input-mapper)
  (define contexts (stack))
  
  (define (push-context! ctxt)
    (contexts 'push! ctxt))
  
  (define (pop-context!)
    (if (contexts 'empty?)
        (error 'input-mapper-object "context stack is empty!")
        (contexts 'pop!)))
  
  (define (check)
    ((contexts 'top) 'check))
  
  (define (input-mapper-object msg . args)
    (let ((my-param (make-param args 'input-mapper-object)))
      (case msg
        ('push-context! (push-context! (my-param 1)))
        ('pop-context! (pop-context!))
        ('check (check))
        (else (error 'input-mapper-object "message \"~S\" unknown" msg)))))
  
  input-mapper-object)
