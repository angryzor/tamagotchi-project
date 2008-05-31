; ADT Stack< T >
;======================================================
; Specifications
;------------------------------------------------------
; Stack: ( -> Stack )
; Stack.empty?: ( -> boolean )
; Stack.full?: ( -> boolean )
; Stack.push!: ( T -> T )
; Stack.top: ( -> T )
; Stack.pop!: ( -> T )
; Stack.Implements? ( symbol -> boolean )
(load "global.ss")

(define (Stack)
  (define istack '())
  ;************************************************
  ; function empty?
  ;
  ; @params: /
  ; @return: true if stack is empty, false otherwise
  ;************************************************
  (define (empty?)
    (null? istack))
  ;************************************************
  ; function full?
  ;
  ; @params: /
  ; @return: true if stack is full (which will never happen),
  ;          false otherwise
  ;************************************************
  (define (full?)
    #f)
  ;************************************************
  ; function push!
  ;
  ; @params: x (T)
  ; @return: x
  ;************************************************
  (define (push! x)
    (set! istack (cons x istack))
    x)
  ;************************************************
  ; function top
  ; -> Peek on top of stack
  ;
  ; @params: /
  ; @return: the object on top of the stack (T)
  ;************************************************
  (define (top)
    (if (empty?)
        #f
        (car istack)))
  ;************************************************
  ; function pop!
  ; -> Destructively read and remove top of stack
  ;
  ; @params: /
  ; @return: the removed object (T)
  ;************************************************
  (define (pop!)
    (if (empty?)
        #f
        (let ((result (car istack)))
          (set! istack (cdr istack))
          result)))
  ;*****************************************************************
  ; function Implements?
  ;
  ; @params: ClassDef (symbol)
  ; @return: true if the ADT supports the functions for the 
  ;          type passed via ClassDef, false otherwise
  ;*****************************************************************
  (define (Implements? ClassDef)
    (eq? ClassDef 'Stack))
  (λ msg
    (if (null? msg)
        (top)
        (case (car msg)
          ('empty? (empty?))
          ('full? (full?))
          ('push! (push! (GetParam msg 0)))
          ('top (top))
          ('pop! (pop!))
          ('Implements? (Implements? (GetParam msg 0)))
          (else (error 'Stack "message not understood: ~S" (car msg)))))))

