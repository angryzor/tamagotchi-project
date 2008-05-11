; fsm.scm
; Desc: Defines the object fsm.
;       To use the Finite State Machine ADT, include this file.
;       This 
;*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*

(load "fsm.scm")

;***************************************************
; Object hunger-fsm
; Constructor spec: (  -> honger-fsm )
; Desc: FSM specification to hunger 
; Args: /
;***************************************************

(define (hunger-fsm)
  (define hunger-level 0)
  (define food-offered #f)
  
  ;===================================================
  ; Method raise-hunger-level!
  ; Spec: (  -> { #<void> } )
  ; Desc: slowly raises the hunger level
  ; Args: /
  ;===================================================
  (define (raise-hunger-level!)
    (set! hunger-level (clamp (+ hunger-level 5) 0 100)))
  
  ;===================================================
  ; Method eat-the-food!
  ; Spec: (  -> { #<void> } )
  ; Desc: set food-offered to #f and calls lower-hunger-level!
  ; Args: /
  ;===================================================
  (define (eat-the-food!)
    (set! food-offered #f)
    (lower-hunger-level!))

  ;===================================================
  ; Method lower-hunger-level!
  ; Spec: (  -> { #<void> } )
  ; Desc: lower the hunger level
  ; Args: /
  ;===================================================
  (define (lower-hunger-level!)
    (set! hunger-level (clamp (- hunger-level 30) 0 100)))
  
  ;===================================================
  ; Method hunger-level-high?
  ; Spec: (  -> { #<void> } )
  ; Desc: returns true if the hunger level is above or equal to 75
  ; Args: /
  ;===================================================
  (define (hunger-level-high?)
    (>= hunger-level 75))
  
  ;===================================================
  ; Method hunger-level-low?
  ; Spec: (  -> { #<void> } )
  ; Desc: returns true if the hunger level is lower than 75
  ; Args: /
  ;===================================================
  (define (hunger-level-low?)
    (not (hunger-level-high?)))
  
  ;===================================================
  ; Method hunger-level-critical?
  ; Spec: (  -> { #<void> } )
  ; Desc: returns true if the hunger level is above or equal to 100 (deadly)
  ; Args: /
  ;===================================================
  (define (hunger-level-critical?)
    (>= hunger-level 100))
  
  ;===================================================
  ; Method true-condition?
  ; Spec: (  -> { #<void> } )
  ; Desc: always returns true
  ; Args: /
  ;===================================================
  (define (true-condition?)
    #t)

  (define (init-transitions)
    (state-satisfied 'add-transition! (fsm-transition hunger-level-high? state-hungry))
    (state-satisfied 'add-transition! (fsm-transition true-condition? state-satisfied))
    ;;---
    (state-hungry 'add-transition! (fsm-transition (λ ()
                                                     (and food-offered
                                                          (not (rebels?)))) state-eating))
    (state-hungry 'add-transition! (fsm-transition (λ ()
                                                     (and food-offered
                                                          (rebels?))) state-refused))
    (state-hungry 'add-transition! (fsm-transition hunger-level-low? state-satisfied))
    (state-hungry 'add-transition! (fsm-transition hunger-level-critical? state-dead))
    (state-hungry 'add-transition! (fsm-transition true-condition? state-hungry))
    ;;---
    (state-eating 'add-transition! (fsm-transition true-condition? state-hungry))
    ;;---
    (state-refused 'add-transition! (fsm-transition true-condition? state-hungry)))

    
  (define state-satisfied (fsm-state raise-hunger-level! '() 2))
  (define state-hungry (fsm-state raise-hunger-level! '() 5))
  (define state-eating (fsm-state eat-the-food! '() 1))
  (define state-refused (fsm-state '() '() 1))
  (define state-dead (fsm-state '() '() 0))
  
  (init-transitions)
  
  (define my-fsm (fsm state-satisfied))
  
  (define (hunger-fsm-object msg . args)
    (let ((my-param (make-param 'hunger-fsm-object)))
      (case msg
        ('transition (transition))
        ('get-current-state state)
        (else (apply my-fsm msg args)))))
  hunger-fsm-object)
