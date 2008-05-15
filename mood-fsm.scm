; mood-fsm.scm
; Desc: Defines the object mood-fsm.
;*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*

(load "fsm.scm")

;***************************************************
; Object mood-fsm
; Constructor spec: (  -> honger-fsm )
; Desc: FSM specification for mood
; Args: /
;***************************************************

(define (mood-fsm)
  (define unhappiness-level (need-level))
  (define play-game #f)
    
  ;===================================================
  ; Method true-condition?
  ; Spec: (  -> { #<void> } )
  ; Desc: always returns true
  ; Args: /
  ;===================================================
  (define (true-condition?)
    #t)
  
  ;===================================================
  ; Method play-a-game
  ; Spec: (  -> { #<void> } )
  ; Desc: starts a game
  ; Args: /
  ;===================================================
  (define (play-a-game)
    ;; Do something here with passed procedures to start a game
    (set! play-game #f))
  
  ;===================================================
  ; Method reject-a-game
  ; Spec: (  -> { #<void> } )
  ; Desc: resets play-game
  ; Args: /
  ;===================================================
  (define (reject-a-game)
    (set! play-game #f))
  
  ;===================================================
  ; Method init-transitions
  ; Spec: (  -> { #<void> } )
  ; Desc: adds transitions to all states (must be called after the states are created)
  ; Args: /
  ;===================================================
  (define (init-transitions)
    (state-happy 'add-transition! (fsm-transition (λ () (unhappiness-level 'high?)) state-unhappy))
    (state-happy 'add-transition! (fsm-transition true-condition? state-happy))
    ;;---
    (state-unhappy 'add-transition! (fsm-transition (λ () (and play-game (not (rebels?)))) state-playing-game))
    (state-unhappy 'add-transition! (fsm-transition (λ () (and play-game (rebels?))) state-refused))
    (state-unhappy 'add-transition! (fsm-transition (λ () (unhappiness-level 'low?)) state-happy))
    (state-unhappy 'add-transition! (fsm-transition (λ () (unhappiness-level 'deadly?)) state-dead))
    (state-unhappy 'add-transition! (fsm-transition true-condition? state-unhappy))
    ;;---
    (state-playing-game 'add-transition! (fsm-transition true-condition? state-unhappy))
    ;;---
    (state-refused 'add-transition! (fsm-transition true-condition? state-unhappy)))

  (define state-happy (fsm-state raise-unhappiness-level! '() 2))
  (define state-unhappy (fsm-state raise-unhappiness-level! '() 5))
  (define state-playing-game (fsm-state eat-the-food! '() 1))
  (define state-refused (fsm-state reject-a-game '() 1))
  (define state-dead (fsm-state '() '() 0))    ; suicide? :S

  (init-transitions)
  
  (define my-fsm (fsm state-satisfied))
  
  (define (mood-fsm-object msg . args)
    (let ((my-param (make-param 'mood-fsm-object)))
      (case msg
        ('unhappy? (eq? (fsm 'get-current-state) state-unhappy))
        ('happy? (eq? (fsm 'get-current-state) state-happy))
        ('playing-game? (eq? (fsm 'get-current-state) state-playing-game))
        ('refused? (eq? (fsm 'get-current-state) state-refused))
        ('dead? (eq? (fsm 'get-current-state) state-dead))
        (else (apply my-fsm msg args)))))
  mood-fsm-object)
