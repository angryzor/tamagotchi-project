; hunger-fsm.scm
; Desc: Defines the object hunger-fsm.
;*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*

(load "fsm.scm")
(load "need-level.scm")

;***************************************************
; Object hunger-fsm
; Constructor spec: (  -> honger-fsm )
; Desc: FSM specification to hunger 
; Args: /
;***************************************************

(define (hunger-fsm)
  (define hunger-level (need-level))
  (define food-offered #f)
  
  ;===================================================
  ; Method eat-the-food!
  ; Spec: (  -> { #<void> } )
  ; Desc: set food-offered to #f and calls lower-hunger-level!
  ; Args: /
  ;===================================================
  (define (eat-the-food!)
    (set! food-offered #f)
    (hunger-level 'lower!))

  ;===================================================
  ; Method reject-the-food!
  ; Spec: (  -> { #<void> } )
  ; Desc: set food-offered to #f
  ; Args: /
  ;===================================================
  (define (reject-the-food!)
    (set! food-offered #f))

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
    (state-satisfied 'add-transition! (fsm-transition (λ () (hunger-level 'high?)) state-hungry))
    (state-satisfied 'add-transition! (fsm-transition (λ () (and food-offered (not (rebels?)))) state-eating))
    (state-satisfied 'add-transition! (fsm-transition (λ () (and food-offered (rebels?))) state-refused))
    (state-satisfied 'add-transition! (fsm-transition true-condition? state-satisfied))
    ;;---
    (state-hungry 'add-transition! (fsm-transition (λ () (and food-offered (not (rebels?)))) state-eating))
    (state-hungry 'add-transition! (fsm-transition (λ () (and food-offered (rebels?))) state-refused))
    (state-hungry 'add-transition! (fsm-transition (λ () (hunger-level 'low?)) state-satisfied))
    (state-hungry 'add-transition! (fsm-transition (λ () (hunger-level 'deadly?)) state-dead))
    (state-hungry 'add-transition! (fsm-transition true-condition? state-hungry))
    ;;---
    (state-eating 'add-transition! (fsm-transition true-condition? state-hungry))
    ;;---
    (state-refused 'add-transition! (fsm-transition true-condition? state-hungry)))

    
  (define state-satisfied (fsm-state (λ () (hunger-level 'raise!)) '() 4))
  (define state-hungry (fsm-state (λ () (hunger-level 'raise!)) '() 5))
  (define state-eating (fsm-state eat-the-food! '() 1))
  (define state-refused (fsm-state reject-the-food! '() 1))
  (define state-dead (fsm-state '() '() 0))
  
  (define my-fsm (fsm state-satisfied))
  
  (define (hunger-fsm-object msg . args)
    (let ((my-param (make-param 'hunger-fsm-object)))
      (case msg
        ('hungry? (eq? (my-fsm 'get-current-state) state-hungry))
        ('satisfied? (eq? (my-fsm 'get-current-state) state-satisfied))
        ('eating? (eq? (my-fsm 'get-current-state) state-eating))
        ('refused? (eq? (my-fsm 'get-current-state) state-refused))
        ('dead? (eq? (my-fsm 'get-current-state) state-dead))
        (else (apply my-fsm msg args)))))
  
  (init-transitions)

  hunger-fsm-object)
