; waste-fsm.scm
; Desc: Defines the object sleep-fsm.
;*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*

(load "fsm.scm")
(load "need-level.scm")

;***************************************************
; Object waste-fsm
; Constructor spec: (  -> waste-fsm )
; Desc: FSM specification to waste
; Args: /
;***************************************************

(define (waste-fsm)
  (define waste-level (need-level))
  (define clean-waste #f)
  
  ;===================================================
  ; Method clean
  ; Spec: (  -> { #<void> } )
  ; Desc: set clean-waste to #f and calls lower-waste-level!
  ; Args: /
  ;===================================================
  (define (clean)
    (set! clean-waste #f)
    (waste-level 'lower!))

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
    (state-clean 'add-transition! (fsm-transition (λ () (waste-level 'high?)) state-disgusting))
    (state-clean 'add-transition! (fsm-transition (λ () clean-waste) state-cleaning))
    (state-clean 'add-transition! (fsm-transition true-condition? state-clean))
    ;;---
    (state-disgusting 'add-transition! (fsm-transition (λ () clean-waste) state-cleaning))
    (state-disgusting 'add-transition! (fsm-transition (λ () (waste-level 'low?)) state-clean))
    (state-disgusting 'add-transition! (fsm-transition (λ () (waste-level 'deadly?)) state-sickening))
    (state-disgusting 'add-transition! (fsm-transition true-condition? state-disgusting))
    ;;---
    (state-cleaning 'add-transition! (fsm-transition true-condition? state-disgusting))
    ;;---
    (state-sickening 'add-transition! (fsm-transition true-condition? state-disgusting)))


  (define state-clean (fsm-state (λ () (waste-level 'raise!)) '() 3))
  (define state-disgusting (fsm-state (λ () (waste-level 'raise!)) '() 4))
  (define state-cleaning (fsm-state clean '() 1))
  (define state-sickening (fsm-state (λ () (external 'health 'sicken!)) '() 1))
  
  (define my-fsm (fsm state-awake))
  
  (define (waste-fsm-object msg . args)
    (let ((my-param (make-param args 'waste-fsm-object)))
      (case msg
        ('disgusting? (eq? (my-fsm 'get-current-state) state-disgusting))
        ('clean? (eq? (my-fsm 'get-current-state) state-clean))
        ('cleaning? (eq? (my-fsm 'get-current-state) state-cleaning))
        ('sickening (eq? (my-fsm 'get-current-state) state-sickening))
        ('dead? #f)
        (else (apply my-fsm msg args)))))
  
  (init-transitions)

  waste-fsm-object)
