; fsm.scm
; Desc: Defines the object fsm.
;       To use the Finite State Machine ADT, include this file.
;*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*

(load "fsm-transition.scm")
(load "fsm-state.scm")

;***************************************************
; Object fsm
; Constructor spec: ( (  -> { #<void> } ) (  -> { #<void> } ) number . fsm-state -> fsm-state )
; Desc: FSM state class; represents a state of a
;       Finite State Machine
; Args: entry-action - a procedure containing the entry action
;       exit-action - a procedure containing the exit action
;       size - the maximum number of transition that can be contained
;***************************************************
