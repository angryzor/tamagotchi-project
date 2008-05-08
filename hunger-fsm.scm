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
; Args: hunger-value - the starting value for the percentage of hunger
;***************************************************

(define (hunger-fsm hunger-level)
  
  ;===================================================
  ; Method transition
  ; Spec: (  -> { #<void> } )
  ; Desc: Is called to possibly transition the fsm to a next state
  ; Args: /
  ;===================================================
  (define (raise-hunger-level!)
    (set! hunger-value (+ hunger-value 5)))
  
  (define (lower-hunger-level!)
    (set! hunger-value (- hunger-value 30)))

  (define (init-transitions)
    (define trans-satisfied-to-hungry (fsm-transition 
  (define state-satisfied (fsm-state raise-hunger-level! '() 2))
  (define state-hungry (fsm-state raise-hunger-level! '() 5))
  (define state-eating (fsm-state lower-hunger-level! '() 1))
  (define state-refused (fsm-state '() '() 1))
  (define state-dead (fsm-state '() '() 0))
  
  (define my-fsm (fsm xxxx))