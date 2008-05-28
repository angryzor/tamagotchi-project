(load "statusbar.scm")

(define (lcd bundle)
  (define sbar (statusbar (bundle 'send-to-all 'level) fill-rectangle write-string))
  
  (define (init)
    (init-lcd)
    (sbar 'totalredraw))
  
  (define (update)
    (sbar 'update))
  
  (define (lcd-object msg . args)
    (let ((my-param (make-param args 'lcd-object)))
      (case msg
        ('update (update))
        (else (error 'lcd-object "message \"~S\" unknown" msg)))))
  
  (init)
  
  lcd-object)
