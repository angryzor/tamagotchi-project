; fsm-transition.scm
; Desc: defines the object fsm-transition
;*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*

(load "globloader.scm")

;*************************************************************
; Object fsm-transition
; Constructor spec: ( (  -> boolean ) fsm-state-object )
; Desc: FSM transition class; represents a transition of a
;       Finite State Machine
; Args: condition - a lambda that will return whether the condition for this
;                   transition is met
;       target - the target state for this transition
;       trans-action - optional Transition Action, which will be
;                      called when the fsm transition through this transition
;*************************************************************

(define (fsm-transition condition target . trans-action)
  
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
  
  (define (act)
    (if (and (not (null? trans-action))
             (not (null? (car trans-action))))
        ((car trans-action))))
  
  (define (fsm-transition-object msg . args)
    (case msg
      ('check (check))
      ('act (act))
      (else (error 'fsm-transition-object "message \"~S\" unknown" msg))))
  fsm-transition-object)
