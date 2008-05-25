(define (tamagotchi-manager)
  (define (mainloop)
    (check-input)
    (transition-all-fsms)
    (disp-output))
  
  