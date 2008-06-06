; need-level.scm
; Desc: Defines the object need-level.
;*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*

(load "fsm.scm")

;***************************************************
; Object need-level
; Constructor spec: (  -> need-level )
; Desc: Abstraction of a level of for example hunger, tiredness, etc.
; Args: /
;***************************************************

(define (need-level)
  (define level 0)
  
  ;===================================================
  ; Method raise!
  ; Spec: (  -> { #<void> } )
  ; Desc: raises the level
  ; Args: /
  ;===================================================
  (define (raise!)
    (set! level (clamp (+ level 5) 0 100)))
  
  ;===================================================
  ; Method lower!
  ; Spec: (  -> { #<void> } )
  ; Desc: lowers the level
  ; Args: /
  ;===================================================
  (define (lower!)
    (set! level (clamp (- level 30) 0 100)))
  
  ;===================================================
  ; Method lower!
  ; Spec: (  -> { #<void> } )
  ; Desc: lowers the level
  ; Args: /
  ;===================================================
  (define (lower-a-bit!)
    (set! level (clamp (- level 5) 0 100)))
  
  ;===================================================
  ; Method high?
  ; Spec: (  -> { #<void> } )
  ; Desc: returns true if the level is above or equal to 75
  ; Args: /
  ;===================================================
  (define (high?)
    (>= level 75))
  
  ;===================================================
  ; Method low?
  ; Spec: (  -> { #<void> } )
  ; Desc: returns true if the level is lower than 75
  ; Args: /
  ;===================================================
  (define (low?)
    (not (high?)))
  
  ;===================================================
  ; Method critical?
  ; Spec: (  -> { #<void> } )
  ; Desc: returns true if the level is above or equal to 90
  ; Args: /
  ;===================================================
  (define (critical?)
    (>= level 90))

  ;===================================================
  ; Method deadly?
  ; Spec: (  -> { #<void> } )
  ; Desc: returns true if the level is equal to 100 (deadly)
  ; Args: /
  ;===================================================
  (define (deadly?)
    (= level 100))

  (define (level-object msg . args)
    (let ((my-param (make-param args 'level-object)))
      (case msg
        ('raise! (raise!))
        ('lower! (lower!))
        ('lower-a-bit! (lower-a-bit!))
        ('high? (high?))
        ('low? (low?))
        ('critical? (critical?))
        ('deadly? (deadly?))
        ('percent level)
        (else (error 'level-object "message \"~S\" unknown" msg)))))
  level-object)
