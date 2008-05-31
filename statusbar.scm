(define (statusbar levels fill-rectangle draw-string)
  
  (define (update)
    (define (convert-to-RGB332 pct)
      (define (rescale-RG f)
        (round (* f 7)))
      
      (let ((red (rescale-RG (clamp (/ pct 50) 0 1)))
            (green (rescale-RG (/ (- 50 (clamp (- pct 50) 0 50)) 50))))
        (logior (ash red 5) (ash green 2))))
    
    
;    (levels 'for-each-counting (lambda (level i)
;                                 (fill-rectangle (+ (* 21 i) 3) 3 18 18 (convert-to-RGB332 (level 'percent))))))
    (levels 'for-each-counting (lambda (level i)
                                 (fill-rectangle (+ (* 21 i) 10) 10 4 4 (convert-to-RGB332 (level 'percent))))))
  
  (define (totalredraw)
    (draw-string "hgr" 3 23)
    (draw-string "slp" 24 23)
    (draw-string "gem" 45 23)
    (draw-string "reb" 66 23)
    (draw-string "gez" 87 23)
    (draw-string "uit" 108 23)
    (update))
  
  (define (statusbar-object msg . args)
    (let ((my-param (make-param args 'statusbar-object)))
      (case msg
        ('totalredraw (totalredraw))
        ('update (update))
        (else (error 'statusbar-object "message \"~S\" unknown" msg)))))
  
  statusbar-object)
