(load "input-binding.scm")
(load "vector-position-list.scm")

(define (input-context size)
  (define mappings (position-list size eq?))
  
  (define (map input proc)
    (mappings 'add-after! (input-binding input proc)))
  
  (define (check)
    ((mappings 'map (lambda (x)
                      (x 'check)) eq?) 'foldl (lambda (x y)
                                                (or x y)) #f))
  
  (define (input-context-object msg . args)
    (let ((my-param (make-param args 'input-context-object)))
      (case msg
        ('map (map (my-param 1) (my-param 2)))
        ('check (check))
        (else (error 'input-context-object "message \"~S\" unknown" msg)))))
  
  input-context-object)
