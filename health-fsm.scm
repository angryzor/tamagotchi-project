; health-fsm.scm
; Desc: Defines the object sleep-fsm.
;*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*

(load "fsm.scm")
(load "need-level.scm")

;***************************************************
; Object health-fsm
; Constructor spec: (  -> health-fsm )
; Desc: FSM specification to health
; Args: /
;***************************************************

(define (health-fsm)
  (define sickness-level (need-level))
  (define give-medicine #f)
  
  ;===================================================
  ; Method heal
  ; Spec: (  -> { #<void> } )
  ; Desc: set give-medicine to #f and calls lower-sickness-level!
  ; Args: /
  ;===================================================
  (define (heal)
    (set! give-medicine #f)
    (sickness-level 'lower!))

  ;===================================================
  ; Method refuse-medicine
  ; Spec: (  -> { #<void> } )
  ; Desc: set give-medicine to #f
  ; Args: /
  ;===================================================
  (define (refuse-medicine)
    (set! give-medicine #f))

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
    (state-healthy 'add-transition! (fsm-transition (λ () (sickness-level 'high?)) state-sick))
    (state-healthy 'add-transition! (fsm-transition (λ () (and give-medicine (not (rebels?)))) state-healing))
    (state-healthy 'add-transition! (fsm-transition (λ () (and give-medicine (rebels?))) state-refused))
    (state-healthy 'add-transition! (fsm-transition true-condition? state-healthy))
    ;;---
    (state-sick 'add-transition! (fsm-transition (λ () (and give-medicine (not (rebels?)))) state-healing))
    (state-sick 'add-transition! (fsm-transition (λ () (and give-medicine (rebels?))) state-refused))
    (state-sick 'add-transition! (fsm-transition (λ () (sickness-level 'low?)) state-healthy))
    (state-sick 'add-transition! (fsm-transition (λ () (sickness-level 'deadly?)) state-dead))
    (state-sick 'add-transition! (fsm-transition true-condition? state-sick))
    ;;---
    (state-healing 'add-transition! (fsm-transition true-condition? state-sick))
    ;;---
    (state-refused 'add-transition! (fsm-transition true-condition? state-sick)))


  (define state-healthy (fsm-state '() '() 4))
  (define state-sick (fsm-state '() '() 5))
  (define state-healing (fsm-state heal '() 1))
  (define state-refused (fsm-state refuse-medicine '() 1))
  (define state-dead (fsm-state '() '() 0))
  
  (define my-fsm (fsm state-awake))
  
  (define (health-fsm-object msg . args)
    (let ((my-param (make-param 'health-fsm-object)))
      (case msg
        ('sick? (eq? (my-fsm 'get-current-state) state-sick))
        ('healthy? (eq? (my-fsm 'get-current-state) state-healthy))
        ('healing? (eq? (my-fsm 'get-current-state) state-healing))
        ('refused? (eq? (my-fsm 'get-current-state) state-refused))
        ('dead? (eq? (my-fsm 'get-current-state) state-dead))
        ('sicken! (sickness-level 'raise!))
        (else (apply my-fsm msg args)))))
  
  (init-transitions)

  health-fsm-object)
