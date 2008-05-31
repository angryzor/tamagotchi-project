(load "input-context.scm")

(define (simple-game input won-event lost-event)
  (define ipt-ctxt (input-context 4))
  (define rval 0)
  (define correct-guess #f)
  (define buttons-pressed 0)
  
  (define (make-valchecker val)
    (lambda ()
      (if (= rval val)
          (set! correct-guess #t))
      (set! buttons-pressed (+ 1 buttons-pressed))))
  
  (define (init)
    (let ((b0-press (input-digital GPIO_0 10))
          (b1-press (input-digital GPIO_0 9))
          (b2-press (input-digital GPIO_0 8))
          (b3-press (input-digital GPIO_0 7)))
      (ipt-ctxt 'map b0-press (make-valchecker 0))
      (ipt-ctxt 'map b1-press (make-valchecker 1))
      (ipt-ctxt 'map b2-press (make-valchecker 2))
      (ipt-ctxt 'map b3-press (make-valchecker 3))))
  
  (define (correct-pin)
    (case rval
      ('(0) (pin 10))
      ('(1) (pin 9))
      ('(2) (pin 8))
      ('(3) (pin 7))))
  
  (define (blink-correct-pin ms)
    (set-pin (correct-pin))
    (wait ms)
    (clear-pin (correct-pin)))
  
  (define (check-if-won)
    (if correct-guess
        (if (not (null? won-event))
            (won-event))
        (if (not (null? lost-event))
            (lost-event))))
  
  (define (start)
    (define (set-values!)
      (set! correct-guess #f)
      (set! buttons-pressed 0)
      (set! rval (rand 'get 0 3)))
    (define (inputloop)
      (if (not (and (input 'check)
                    (= buttons-pressed 1)))
          (inputloop)))
    
    (input 'push-context! ipt-ctxt)
    (set-values!)
    (inputloop)
    (check-if-won)
    (blink-correct-pin 800)
    (input 'pop-context!))
  
  (define (simple-game-object msg . args)
    (let ((my-param (make-param args 'simple-game-object)))
      (case msg
        ('start (start))
        (else (error 'simple-game-object "message \"~S\" unknown" msg)))))
  
  (init)
  
  simple-game-object)
