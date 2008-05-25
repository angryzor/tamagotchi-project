(load "tama-fsm-bundler.scm")

(define (tamagotchi-manager)
  (define fsm-bundle (tama-fsm-bundler))
  
  (define (transition-all-fsms)
    (fsm-bundle 'transition))
  
  (define (mainloop)
    (if (fsm-bundle 'one-dead?)
        (begin
          (display "DEAD")
          (newline))
        (begin
          (display "TRANSITIONING")
          (newline)
;         (check-input)
          (transition-all-fsms)
;         (disp-output))
          (mainloop))))
  
  (mainloop)
  )