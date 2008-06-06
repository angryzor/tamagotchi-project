(load "globloader.scm")

(define (statustext bundle drawer)
  (define (draw-sleep)
    (if (bundle 'send-to 'sleep 'asleep?)
        (drawer 'write-string "zzzzz" 0 100)))
  
  (define (draw-refuse)
    (if (or (bundle 'send-to 'sleep 'refused?)
            (bundle 'send-to 'mood 'refused?)
            (bundle 'send-to 'hunger 'refused?)
            (bundle 'send-to 'health 'refused?))
        (drawer 'write-string "noooo" 50 100)))
  
  (define (update)
    (drawer 'fill-rectangle 0 100 128 6 colour-white)
    (draw-sleep)
    (draw-refuse))
  
  (define (statustext-object msg . args)
    (let ((my-param (make-param args 'statustext-object)))
      (case msg
        ('update (update))
        (else (error 'statustext-object "message \"~S\" unknown" msg)))))
  
  statustext-object)
