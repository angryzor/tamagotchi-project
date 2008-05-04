; fsm-transition.scm
; Desc: defines the object fsm-transition
;*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*

(load "globloader.scm")

;*************************************************************
; Object fsm-transition
; Constructor spec: ( (  -> boolean ) fsm-state-object )
; Desc: FSM transition class; represents a transition of a
;       Finite State Machine
;*************************************************************

(define (fsm-transition condition target)

;=========================================================
; Method check
; Spec: (  -> fsm-state-object U { #f } )
; Desc: Call this procedure on every state-change check.
;       It will return the state object specified in target when the condition is met, and #f if it is not.
; Args: /
;=========================================================
  (define (check)
    (if (condition)
        target
        #f))
  
  (define (fsm-transition-object msg . args)
      (case msg
        ('check (check))
        (else (error 'fsm-transition-object "message \"~S\" unknown" msg))))
  fsm-transition-object)
