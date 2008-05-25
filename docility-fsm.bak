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

(define (docility-fsm)
  (define rebelliosity-level (need-level))
  (define punishment-given #f)
  
  ;===================================================
  ; Method receive-punishment
  ; Spec: (  -> { #<void> } )
  ; Desc: set punishment-given to #f and calls lower-rebelliosity-level!
  ; Args: /
  ;===================================================
  (define (sleep)
    (set! punishment-given #f)
    (rebelliosity-level 'lower!))

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
    (state-docile 'add-transition! (fsm-transition (λ () (rebelliosity-level 'high?)) state-rebellish))
    (state-docile 'add-transition! (fsm-transition true-condition? state-docile))
    ;;---
    (state-rebellish 'add-transition! (fsm-transition (λ () punishment-given) state-getting-punished))
    (state-rebellish 'add-transition! (fsm-transition (λ () (rebelliosity-level 'low?)) state-docile))
    (state-rebellish 'add-transition! (fsm-transition true-condition? state-rebellish))
    ;;---
    (state-getting-punished 'add-transition! (fsm-transition true-condition? state-rebellish)))


  (define state-docile (fsm-state (λ () (rebelliosity-level 'raise!)) '() 2))
  (define state-rebellish (fsm-state (λ () (rebelliosity-level 'raise!)) '() 3))
  (define state-getting-punished (fsm-state receive-punishment '() 1))
  
  (define my-fsm (fsm state-docile))
  
  (define (docility-fsm-object msg . args)
    (let ((my-param (make-param args 'docility-fsm-object)))
      (case msg
        ('rebellish? (eq? (my-fsm 'get-current-state) state-rebellish))
        ('docile? (eq? (my-fsm 'get-current-state) state-docile))
        ('getting-punished? (eq? (my-fsm 'get-current-state) state-getting-punished))
        ('dead? #f)
        (else (apply my-fsm msg args)))))
  
  (init-transitions)

  docility-fsm-object)
