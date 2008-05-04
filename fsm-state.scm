; fsm-state.scm
; Desc: defines the object fsm-state
;*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*

(load "globloader.scm")
(load "vector-position-list.scm")

;***************************************************
; Object fsm-state
; Constructor spec: ( (  -> { #<void> } ) (  -> { #<void> } ) number . fsm-state -> fsm-state )
; Desc: FSM state class; represents a state of a
;       Finite State Machine
; Args: entry-action - a procedure containing the entry action
;       exit-action - a procedure containing the exit action
;       size - the maximum number of transition that can be contained
;***************************************************

(define (fsm-state entry-action exit-action size)
  (define trans-list (position-list size eq?))
  
;=================================================
; Method add-transition
; Spec: ( fsm-transition -> { #<void> } )
; Desc: adds a new transition to the state
; Args: trans - the transition to be added
;=================================================
  (define (add-transition trans)
    (if (trans-list 'full?)
        (error 'fsm-state.add-transition "transition list is full. check your fsm-state size")
        (trans-list 'add-after trans)))
  
;=================================================
; Method next-state
; Spec: (  -> fsm-state )
; Desc: "check"'s all transitions and returns the first non-#f value returned.
;       Returns #f if all of the transitions return #f.
;       The FSM should stay in its current state in this case.
;       NOTE: transitions are checked in the order in which they were added
; Args: /
;=================================================
  (define (next-state)
    (define (find-next-state pos)
      (let ((trans-check ((trans-list 'value pos) 'check)))
        (cond (trans-check trans-check)
              ((trans-list 'has-next? pos) (find-next-state (trans-list 'next pos)))
              (else #f))))
    (if (trans-list 'empty?)
        #f
        (find-next-state (trans-list 'first-position))))
  
     
     
     
     
     
  (define (fsm-state-object msg . args)
    (let ((my-param (make-param 'fsm-state-object)))
      (case msg
        ('add-transition (add-transition (my-param args 1)))
        ('next-state (next-state))
        (else (error 'fsm-state-object "message \"~S\" unknown" msg)))))
  fsm-state-object)
     
     
;%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
; Obsolete ideas:
;       else-state - an optional state that will be returned by next-state
;                    if none of the transitions' conditions are met.
;                    if this argument is not set, next-state will return #f in this case,
;                    and the fsm should stay in this state
;%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
  