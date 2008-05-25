; sleep-fsm.scm
; Desc: Defines the object sleep-fsm.
;*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*

(load "fsm.scm")
(load "need-level.scm")

;***************************************************
; Object sleep-fsm
; Constructor spec: (  -> sleep-fsm )
; Desc: FSM specification to sleep
; Args: /
;***************************************************

(define (sleep-fsm)
  (define tiredness-level (need-level))
  (define put-in-bed #f)
  
  ;===================================================
  ; Method sleep
  ; Spec: (  -> { #<void> } )
  ; Desc: set put-in-bed to #f and calls lower-tiredness-level!
  ; Args: /
  ;===================================================
  (define (sleep)
    (wait SLEEP_TIME_SECONDS)
    (set! put-in-bed #f)
    (tiredness-level 'lower!))

  ;===================================================
  ; Method dont-sleep
  ; Spec: (  -> { #<void> } )
  ; Desc: set put-in-bed to #f and calls lower-tiredness-level!
  ; Args: /
  ;===================================================
  (define (dont-sleep)
    (set! put-in-bed #f))

  ;===================================================
  ; Method true-condition?
  ; Spec: (  -> { #<void> } )
  ; Desc: always returns true
  ; Args: /
  ;===================================================
  (define (true-condition?)
    #t)

  ;===================================================
  ; Method init-transitions
  ; Spec: (  -> { #<void> } )
  ; Desc: adds transitions to all states (must be called after the states are created)
  ; Args: /
  ;===================================================
  (define (init-transitions)
    (state-awake 'add-transition! (fsm-transition (λ () (tiredness-level 'high?)) state-tired))
    (state-awake 'add-transition! (fsm-transition (λ () (and put-in-bed (not (rebels?)))) state-asleep))
    (state-awake 'add-transition! (fsm-transition (λ () (and put-in-bed (rebels?))) state-refused))
    (state-awake 'add-transition! (fsm-transition true-condition? state-awake))
    ;;---
    (state-tired 'add-transition! (fsm-transition (λ () (and put-in-bed (not (rebels?)))) state-asleep))
    (state-tired 'add-transition! (fsm-transition (λ () (and put-in-bed (rebels?))) state-refused))
    (state-tired 'add-transition! (fsm-transition (λ () (tiredness-level 'low?)) state-awake))
    (state-tired 'add-transition! (fsm-transition (λ () (tiredness-level 'deadly?)) state-dead))
    (state-tired 'add-transition! (fsm-transition true-condition? state-tired))
    ;;---
    (state-asleep 'add-transition! (fsm-transition true-condition? state-tired))
    ;;---
    (state-refused 'add-transition! (fsm-transition true-condition? state-tired)))


  (define state-awake (fsm-state (λ () (tiredness-level 'raise!)) '() 4))
  (define state-tired (fsm-state (λ () (tiredness-level 'raise!)) '() 5))
  (define state-asleep (fsm-state sleep '() 1))
  (define state-refused (fsm-state dont-sleep '() 1))
  (define state-dead (fsm-state '() '() 0))
  
  (define my-fsm (fsm state-awake))
  
  (define (sleep-fsm-object msg . args)
    (let ((my-param (make-param args 'sleep-fsm-object)))
      (case msg
        ('tired? (eq? (my-fsm 'get-current-state) state-tired))
        ('awake? (eq? (my-fsm 'get-current-state) state-awake))
        ('sleeping? (eq? (my-fsm 'get-current-state) state-sleeping))
        ('refused? (eq? (my-fsm 'get-current-state) state-refused))
        ('dead? (eq? (my-fsm 'get-current-state) state-dead))
        (else (apply my-fsm msg args)))))
  
  (init-transitions)
  
  sleep-fsm-object)
