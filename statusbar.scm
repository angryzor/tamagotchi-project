(define (statusbar levels drawer)
  
  (define (update)
    (define (convert-to-RGB332 pct)
      (define (rescale-RG f)
        (round (* f 7)))
      
      (let ((red (rescale-RG (clamp (/ pct 50) 0 1)))
            (green (rescale-RG (/ (- 50 (clamp (- pct 50) 0 50)) 50))))
        (logior (ash red 5) (ash green 2))))
    
    
    (levels 'for-each-counting (lambda (level i)
                                 (drawer 'fill-rectangle (+ (* 21 i) 3) 3 18 18 (convert-to-RGB332 (level 'percent))))))
  
  (define (totalredraw)
    (drawer 'write-string "hgr" 3 23)
    (drawer 'write-string "slp" 24 23)
    (drawer 'write-string "gem" 45 23)
    (drawer 'write-string "reb" 66 23)
    (drawer 'write-string "gez" 87 23)
    (drawer 'write-string "uit" 108 23)
    (update))
  
  (define (statusbar-object msg . args)
    (let ((my-param (make-param args 'statusbar-object)))
      (case msg
        ('totalredraw (totalredraw))
        ('update (update))
        (else (error 'statusbar-object "message \"~S\" unknown" msg)))))
  
  statusbar-object)


;    (levels 'for-each-counting (lambda (level i)
;                                 (fill-rectangle (+ (* 21 i) 10) 10 4 4 (convert-to-RGB332 (level 'percent))))))
