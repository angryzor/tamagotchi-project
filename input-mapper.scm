(load "input-binding.scm")

(define (input-mapper)
  (define mappings '()) ;; Using an ordinary list here. Could be changed later
  
  (define (map input proc)
    (cons (input-binding input proc) mappings))
  
  (define (check)
    (for-each (λ (x)
                (x 'check)) mappings))
  
  (define (input-mapper-object)
    (let ((my-param (make-param args 'input-mapper-object)))
      (case msg
        ('map (map (param 1) (param 2)))
        ('check (check))
        (else (error 'input-mapper-object "message \"~S\" unknown" msg)))))
  
  input-mapper-object)
