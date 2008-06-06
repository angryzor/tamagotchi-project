(define (lcd-drawer)
  (define (init)
    (init-lcd))
  
  (define (fill-rect args)
    (apply fill-rectangle args))
  
  (define (write-str str x y)
    (write-string str x y))
  
  (define (lcd-drawer-object msg . args)
    (let ((my-param (make-param args 'lcd-drawer-object)))
      (case msg
        ('fill-rectangle (fill-rect args))
        ('write-string (write-str (my-param 1) (my-param 2) (my-param 3)))
        (else (error 'lcd-drawer-object "message \"~S\" unknown" msg)))))
  
  (init)
  
  lcd-drawer-object)
