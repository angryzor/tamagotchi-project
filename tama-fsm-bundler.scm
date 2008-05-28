(load "hunger-fsm.scm")
(load "sleep-fsm.scm")
(load "docility-fsm.scm")
(load "mood-fsm.scm")
(load "health-fsm.scm")
(load "waste-fsm.scm")

(define (tama-fsm-bundler)
  (define fsm-id car)
  (define fsm-obj cdr)
  
  (define fsms (position-list 6 (lambda (x y)
                                  (eq? (fsm-id x) y))))
  
  (define (add-fsms)
    (fsms 'add-after! (cons 'hunger (hunger-fsm fsm-external-procedure-call)))
    (fsms 'add-after! (cons 'sleep (sleep-fsm fsm-external-procedure-call)))
    (fsms 'add-after! (cons 'mood (mood-fsm fsm-external-procedure-call)))
    (fsms 'add-after! (cons 'docility (docility-fsm fsm-external-procedure-call)))
    (fsms 'add-after! (cons 'health (health-fsm fsm-external-procedure-call)))
    (fsms 'add-after! (cons 'waste (waste-fsm fsm-external-procedure-call))))
  
  (define (fsm-external-procedure-call fsm command . args)
    (let ((the-fsm (fsm-obj (fsms 'value (fsms 'find fsm)))))
      (apply the-fsm command args)))
  
  (define (transition-all)
    (fsms 'for-each (lambda (x)
                      ((fsm-obj x) 'transition))))
  
  (define (one-dead?)
    (let ((death-map (fsms 'map (λ (x)                                  ;;;;; TODO: REMOVE DBUG CODE
                                  (let ((isdead ((fsm-obj x) 'dead?)))
                                    (if isdead
                                        (begin
                                          (display "FSM ")
                                          (display (fsm-id x))
                                          (display " REPORTS DEAD!")
                                          (newline)))
                                    isdead)) eq?)))
      (death-map 'foldl (λ (x y)
                          (or x y)) #f)))
  
  (define (send-to-all msg . args)
    (fsms 'map (λ (x)
                 (apply (fsm-obj x) msg args)) eq?))
  
  (define (tama-fsm-bundler-object msg . args)
    (let ((my-param (make-param args 'tama-fsm-bundler-object)))
      (case msg
        ('transition (transition-all))
        ('one-dead? (one-dead?))
        ('send-to-all (apply send-to-all args))
        ('send-to (let ((r (apply fsm-external-procedure-call args)))
                    (display "sending ")
                    (display (car args))
                    (display " ")
                    (display (cadr args))
                    (display ", returns: ")
                    (display r)
                    (newline)
                    r))
        (else (error 'tama-fsm-bundler-object "message \"~S\" unknown" msg)))))
  
  (add-fsms)
  
  tama-fsm-bundler-object)
