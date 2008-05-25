(load "hunger-fsm.scm")
(load "sleep-fsm.scm")
(load "docility-fsm.scm")
(load "mood-fsm.scm")
(load "health-fsm.scm")
(load "waste-fsm.scm")

(define (tamagotchi-fsmbundler)
  (define fsms (position-list 6 (λ (x y)
                                     (eq? (car x) y))
  
  (define (add-fsms)
    (fsms 'add-after! (cons 'hunger (hunger-fsm)))
    (fsms 'add-after! (cons 'sleep (sleep-fsm)))
    (fsms 'add-after! (cons 'mood (mood-fsm)))
    (fsms 'add-after! (cons 'docility (docility-fsm)))
    (fsms 'add-after! (cons 'health (health-fsm)))
    (fsms 'add-after! (cons 'waste (waste-fsm))))
  
  (define (fsm-external-procedure-call fsm command . args)
    