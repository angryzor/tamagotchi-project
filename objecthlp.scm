; objecthlp.scm
; Desc: procedures to ease the use of objects
;*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*

;******************************************************
; Procedure param
; Spec: ( list<sobj> number symbol -> sobj )
; Desc: Gets the correct param from a list of params
;       Provides detailed error message
; Args: prms - a list of arguments from which the n'th element will be taken
;       n - the index of the required element in the prms list
;       sender - a symbol representation of the object that receives the message
;******************************************************
(define (param prms n sender)
  (define (print-arguments)
    (define (print-actual-arguments iprms)
      (if (not (null? iprms))
          (begin
            (display " ")
            (display (car iprms)))))
    (display "arguments passed:")
    (print-actual-arguments prms)
    (newline))
  (define (find-nth iprms i)
    (cond ((null? iprms) (print-arguments)
                         (error sender "not enough arguments (given ~S)" (- n i)))
          ((= i 1) (car iprms))
          (else (find-nth (cdr iprms) (- i 1)))))
  (find-nth prms n))

;******************************************************
; Procedure make-param
; Spec: ( symbol -> ( list<sobj> number -> sobj ) )
; Desc: makes a param procedure which always calls with sender as sender,
;       so this would not have to be repeated numerous times in the object definition
; Args: sender - a symbol representation of the object that receives the message
;******************************************************
(define (make-param sender)
  (λ (prms n)
    (param prms n sender)))