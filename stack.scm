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

(define (stack)
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
  
  (define (stack-object msg . args)
    (let ((my-param (make-param args 'stack-object)))
      (case msg
        ('empty? (empty?))
        ('full? (full?))
        ('push! (push! (my-param 1)))
        ('top (top))
        ('pop! (pop!))
        (else (error 'stack-object "message \"~S\" unknown" msg)))))
  
  stack-object)
