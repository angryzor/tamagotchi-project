; docility-fsm.scm
; Desc: Defines the object docility-fsm.
;*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*

(load "fsm.scm")
(load "need-level.scm")

;***************************************************
; Object docility-fsm
; Constructor spec: (  -> docility-fsm )
; Desc: FSM specification to docility
; Args: /
;***************************************************

(define (docility-fsm external)
  (define rebelliosity-level (need-level))
  (define punishment-given #f)
  
  ;===================================================
  ; Method receive-punishment
  ; Spec: (  -> { #<void> } )
  ; Desc: set punishment-given to #f and calls lower-rebelliosity-level!
  ; Args: /
  ;===================================================
  (define (receive-punishment)
    (set! punishment-given #f)
    (rebelliosity-level 'lower!))

  ;===================================================
  ; Method rebels?
  ; Spec: (  -> { #<void> } )
  ; Desc: checks random value
  ; Args: /
  ;===================================================
  (define (rebels?)
    (if (eq? (my-fsm 'get-current-state) state-rebellish)
        #t
        (< (rand 'get 0 100) 10)))

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
    (state-docile 'add-transition! (fsm-transition (lambda () (rebelliosity-level 'high?)) state-rebellish))
    (state-docile 'add-transition! (fsm-transition (lambda () punishment-given) state-getting-punished))
    (state-docile 'add-transition! (fsm-transition true-condition? state-docile))
    ;;---
    (state-rebellish 'add-transition! (fsm-transition (lambda () punishment-given) state-getting-punished))
    (state-rebellish 'add-transition! (fsm-transition (lambda () (rebelliosity-level 'low?)) state-docile))
    (state-rebellish 'add-transition! (fsm-transition true-condition? state-rebellish))
    ;;---
    (state-getting-punished 'add-transition! (fsm-transition (lambda () (rebelliosity-level 'low?)) state-docile))
    (state-getting-punished 'add-transition! (fsm-transition (lambda () (rebelliosity-level 'high?)) state-rebellish)))


  (define state-docile (fsm-state (lambda () (rebelliosity-level 'raise!)) '() 3))
  (define state-rebellish (fsm-state (lambda () (rebelliosity-level 'raise!)) '() 3))
  (define state-getting-punished (fsm-state receive-punishment '() 2))
  
  (define my-fsm (fsm state-docile))
  
  (define (docility-fsm-object msg . args)
    (let ((my-param (make-param args 'docility-fsm-object)))
      (case msg
        ('rebellish? (eq? (my-fsm 'get-current-state) state-rebellish))
        ('docile? (eq? (my-fsm 'get-current-state) state-docile))
        ('getting-punished? (eq? (my-fsm 'get-current-state) state-getting-punished))
        ('dead? #f)
        ('rebels? (rebels?))
        ('punish (set! punishment-given #t))
        ('level rebelliosity-level)
        (else (apply my-fsm msg args)))))
  
  (init-transitions)

  docility-fsm-object)
