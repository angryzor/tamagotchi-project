; fsm.scm
; Desc: Defines the object fsm.
;       To use the Finite State Machine ADT, include this file.
;       This 
;*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*

(load "fsm-transition.scm")
(load "fsm-state.scm")

;***************************************************
; Object fsm
; Constructor spec: ( fsm-state -> fsm )
; Desc: FSM class; represents a Finite State Machine
; Args: state - the starting that the fsm is in
;***************************************************

(define (fsm state)
  
  ;===================================================
  ; Method transition
  ; Spec: (  -> { 'ok } )
  ; Desc: Is called to possibly transition the fsm to a next state
  ; Args: /
  ;===================================================
  
  (define (transition)
    (let ((next-state (state 'next-state)))
      (if next-state
          (begin
            (state 'leave)
            (set! state next-state)
            (state 'enter)
            'ok)
          'ok)))
  
  (define (fsm-object msg . args)
    (let ((my-param (make-param 'fsm-object)))
      (case msg
        ('transition (transition))
        ('get-current-state state)
        (else (error 'fsm-object "message \"~S\" unknown" msg)))))
  
  
  fsm-object)
