(require (lib "graphics.ss" "graphics"))

(define (lcd-drawer)
  (define vp '())
  
  (define (init)
    (open-graphics)
    (set! vp (open-viewport "SIMULATION: LCD VIEW" 128 126))) ; 126: simulate px 0 and 1 to be offscreen
  
  ;; REDEFINES FOR SIM
  (define (extract-R RGB332)
    (/ (ash RGB332 -5) 7))
  (define (extract-G RGB332)
    (/ (logand (ash RGB332 -2) 7) 7))
  (define (extract-B RGB332)
    (/ (logand RGB332 3) 3))
  (define (RGB332->rgb color)
    (make-rgb (extract-R color) (extract-G color) (extract-B color)))
  
  (define (fill-rectangle x y width height color)
    ((draw-solid-rectangle vp) (make-posn x y) width height (RGB332->rgb color)))
  
  (define (write-string str x y)
    (let ((i 0))
      (loop (lambda ()
              (let ((letter (cdr (assoc (string-ref str i) charmap))))  ;ohgod
                (write-letter letter (+ x (* i 6)) y 0 #xff)
                (set! i (+ i 1))))
            (string-length str))))
  
  (define (write-letter letter x y foreground-colour background-colour)
    (let ((x1 x)
          (x2 (+ x 5))
          (y1 y)         ; first 2 rows are offscreen
          (y2 (+ y 7))
          (colour-map 
           (map 
            (lambda (bit)
              (if bit
                  foreground-colour
                  background-colour))
            letter)))
      (send-command PASET y1 y2)
      (send-command CASET x1 x2)
      (let loop ((x 0)
                 (y 0)
                 (cmap colour-map))
        (if (not (null? cmap))
            (begin
              ((draw-pixel vp) (make-posn (+ x1 x) (+ y1 y)) (RGB332->rgb (car cmap)))
              (let ((newx (modulo (+ x 1) (- x2 x1 -1))))
                (loop newx (if (= newx 0)
                               (+ y 1)
                               y) (cdr cmap))))))))
  
  
  ;; OBJECT TABLE
  
  (define (lcd-drawer-object msg . args)
    (let ((my-param (make-param args 'lcd-drawer-object)))
      (case msg
        ('fill-rectangle (fill-rectangle (my-param 1) (my-param 2) (my-param 3) (my-param 4) (my-param 5)))
        ('write-string (write-string (my-param 1) (my-param 2) (my-param 3)))
        (else (error 'lcd-drawer-object "message \"~S\" unknown" msg)))))
  
  
  (init)
  
  lcd-drawer-object)
