(load "globloader.scm")
(load "statusbar.scm")
(load "statustext.scm")
(load "bitmap-tamagotchi")
(load "ld-lcd-drawer.scm")

(define (lcd bundle)
  (define drawer (lcd-drawer))
  (define sbar (statusbar (bundle 'send-to-all 'level) drawer))
  (define stext (statustext bundle drawer))
  
  (define (init)
    (drawer 'fill-rectangle 0 0 128 128 colour-white)
    (drawer 'draw-bitmap bitmap-tamagotchi 35 30 60 60)
    (sbar 'totalredraw)
    (stext 'update))
  
  (define (update)
    (sbar 'update)
    (stext 'update))
  
  (define (lcd-object msg . args)
    (let ((my-param (make-param args 'lcd-object)))
      (case msg
        ('update (update))
        (else (error 'lcd-object "message \"~S\" unknown" msg)))))
  
  (init)
  
  lcd-object)
