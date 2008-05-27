(load "input.scm")
(load "tama-fsm-bundler.scm")

(define (tamagotchi-manager)
  (define fsm-bundle (tama-fsm-bundler))
  (define input (input-mapper))
  
  (define (init-input)
    (let ((b0-press (input-digital GPIO_0 6))
          (b1-press (input-digital GPIO_0 7))
          (b2-press (input-digital GPIO_0 8))
          (b3-press (input-digital GPIO_0 9))
          (temp-hot (input-analog 'ain0 (λ (val)
                                      (> val 75))))
          (ldr-dark (input-analog 'ain1 (λ (val)
                                       (> val 70)))))
      (input 'map b0-press (λ ()
                             (fsm-bundle 'send-to 'hunger 'feed)))
      (input 'map b1-press (λ ()
                             (fsm-bundle 'send-to 'mood 'play-a-game)))
      (input 'map b2-press (λ ()
                             (fsm-bundle 'send-to 'docility 'punish)))
      (input 'map b3-press (λ ()
                             (fsm-bundle 'send-to 'waste 'clean)))
      (input 'map temp-hot (λ ()
                             (fsm-bundle 'send-to 'health 'cure)))
      (input 'map ldr-dark (λ ()
                             (fsm-bundle 'send-to 'sleep 'put-in-bed)))))

  
;;==========================================================
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
          (check-input)
          (transition-all-fsms)
;         (disp-output))
          (mainloop))))
  
  (mainloop)
  )