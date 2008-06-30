(define (lcd-drawer)
  (define (init)
    (init-lcd))
  
  (define (fill-rect args)
    (apply fill-rectangle args))
  
  (define (write-str str x y)
    (write-string str x y))
  
  (define (draw-bitmap vec x y width height)
    (define bitnum 0)
    (define cursor 0)
    
    (define (next-colour)
      (let ((res (logand (ash (vector-ref vec cursor) (- (* (- 2 bitnum) 8)))
                         #xFF)))
        (set! bitnum (remainder (+ bitnum 1) 3))
        (if (= bitnum 0)
            (set! cursor (+ cursor 1)))
        res))
    
    (let ((x1 x)
          (x2 (+ x width -1))
          (y1 (+ y 2))
          (y2 (+ y height 1)))
      (send-command PASET y1 y2)
      (send-command CASET x1 x2)
      (send-command RAMWR))
    (loop (lambda () (send-byte lcd-data (next-colour))) 
          (* width height)))
  
  (define (lcd-drawer-object msg . args)
    (let ((my-param (make-param args 'lcd-drawer-object)))
      (case msg
        ('fill-rectangle (fill-rect args))
        ('write-string (write-str (my-param 1) (my-param 2) (my-param 3)))
        ('draw-bitmap (draw-bitmap (my-param 1) (my-param 2) (my-param 3) (my-param 4) (my-param 5)))
        (else (error 'lcd-drawer-object "message \"~S\" unknown" msg)))))
  
  (init)
  
  lcd-drawer-object)
