(define (random-generator timer . dontrun)
  (define (resume)
    'ok)
  (define (pause)
    'ok)
  (define (get min max)
    (+ min (random (+ max 1))))
  
  (define (random-generator-object msg . args)
    (let ((my-param (make-param args 'random-generator-object)))
      (case msg
        ('resume (resume))
        ('pause (pause))
        ('get (get (my-param 1) (my-param 2)))
        (else (error 'random-generator-object "message \"~S\" unknown" msg)))))
  (resume)
  random-generator-object)
