(define (send-byte a b)
  'ok)

;;;; Boot File, only execute this code once per memory flash

;;;; Utility Functions
(define (reduce reduce-fun args)
  (if (pair? args)
      (if (null? (cdr args))
          (car args)
          (reduce reduce-fun (cons (reduce-fun (car args) (cadr args)) 
                                   (cddr args))))))
;; converts a number/hex to a string with its binary presentation
(define (n2b num) (number->string num 2))
;; converts a string with a binary presentation to a number
(define (b2n str) (string->number str 2))

;;;; Pin Mode Selection Registers
(define pinsel0 #xE002C000)
(define pinsel1 #xE002C004)
(define pinsel2 #xE002C014)
;; write pin select's default:
;;  P0.00 -> P0.26 setting 00 (gpio0 usually)
;;  P0.27 -> P0.30 setting 01 (ain0, analog input for a/d converter)
;    (write #x15400000 pinsel1 0) ; proper reset value

;;;; Analog/Digital Converter

(define (bit nr) (ash 1 nr)) ; helper function
(define (getbit val nr) (logand 1 (ash val (- nr))))
(define pin bit)

;;;  A/D Control Register Offsets (ad-control)
(define ad-control #xE0034000)  
(define ad-sel (bit 0)) ; size 3
(define ad-clkdiv (bit 8)) ;size 8
(define ad-burst (bit 16)) ; size 1
(define ad-clks (bit 17)) ; size 3
(define ad-pdn (bit 21)) ; size 2
(define ad-start (bit 24)) ; size 3
(define ad-edge (bit 27)) ; size 1

;;; A/D Data Register Offsets (ad-data)

(define ad-data #xE0034004)
(define ad-done (bit 31))
(define ad-overun (bit 30))
(define ad-chn (bit 24)) ; size 3
(define ad-volt (bit 6)) ; size 10


;;;; General Input/Output Registers

(define GPIO_0 #xE0028000) ; IO0 port 
(define GPIO_1 #xE0028010) ; IO1 port 
(define IOxPIN #x00)  ; IOxPIN register offset
(define IOxSET #x04)  ; IOxSET register offset
(define IOxCLR #x0C)  ; IOxCLR register offset
(define IOxDIR #x08)  ; IOxDIR register offset

(define (read-pin pin-nr)
  (= (getbit (read GPIO_0 IOxPIN) pin-nr) 1))
(define (set-pin pin)
  (write pin GPIO_0 IOxSET))
(define (clear-pin pin)
  (write pin GPIO_0 IOxCLR))
(define (output-pin pin)
  (write (logior (read GPIO_0 IOxDIR)
                 pin)
         GPIO_0 IOxDIR))
(define (input-pin pin)
  (write (logand (read GPIO_0 IOxDIR)
                 (lognot pin))
         GPIO_0 IOxDIR))

;;;; Timer Definitions

;; Timer0 base register port
(define TIMER0 #xE0004000)
(define TIMER1 #xE0008000)
;; Timer offsets
(define TxIR #x00)
(define TxTCR #x04)
(define TxTC #x08)
(define TxPR #x0C)
(define TxPC #x10)
(define TxMCR #x14)
(define TxMR0 #x18)
(define TxMR1 #x1C)
(define TxMR2 #x20)
(define TxMR3 #x24)
(define TxCCR #x28)
(define TxCR0 #x2C)
(define TxCR1 #x30)
(define TxCR2 #x34)
(define TxEMR #x3C)
(define TxCALLBACK #x010000) ;; Special value that does not really write to a register but sets a callback

(define (timer.set-PR timer val)
  (write val timer TxPR))

(define (timer.start timer)
  (write #b01 timer TxTCR))

(define (timer.stop timer)
  (write #b00 timer TxTCR))

(define (timer.reset timer)
  (write #b10 timer TxTCR))

(define (timer.value timer)
  (read timer TxTC))

(define (timer.reset-n-start timer)
  (timer.reset timer)
  (timer.start timer))

(define (timer.set-MR timer mr val)
  (write val timer mr))

(define (timer.MR-control-over timer mr-id interrupt reset stop)
  (write (ash (logior interrupt (ash reset 1) (ash stop 2)) (* mr-id 3)) timer TxMCR))

(define (timer.MR-control timer mr-id interrupt reset stop)
  (write (logior (read timer TxMCR)
                 (ash (logior interrupt (ash reset 1) (ash stop 2)) (* mr-id 3))) timer TxMCR))

(define (timer.itr-callback timer proc)
  (write proc timer TxCALLBACK))

(define (wait ms) 
  (wait-us (* ms 1000)))


;;;; Analog/Digital Converter
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define ad-default-config (reduce logior (list ad-pdn
                                               (* ad-clkdiv 13) 
                                               ad-start)))

(define (measure-pin pin-nr) ; pin-nr is 0, 1, 2 or 3
  ; power up the AD converter
  ; set the A/D clock speed (value is a divider)
  ; start conversion
  (let ((ad-configuration (logior (ash ad-sel pin-nr)
                                  ad-default-config)))
    (write ad-configuration ad-control 0))  ; perform the measurement!
  ; data starts at bit 6 of the data register
  (/ (logand (ash (read ad-data 0) -6) ; read data register and shift 6 to the right
             #x03FF)                   ; mask away the upper 6 bits
     #x03FF))  ; normalise the result [0,1023] -> [0-1]   (1=3.3V in practice)

;; ADT of Analog to Digital measurements on pins 1, 2 and 3 (respectively P.27, 28 and 29)
;; returns a normalized [0-1]
(define (measure msg)
  (case msg
    ('ain0 (measure-pin 0))  ; P0.27
    ('ain1 (measure-pin 1))  ; P0.28
    ('ain2 (measure-pin 2))  ; P0.29
    ('ain3 (measure-pin 3))  ; P0.30
    ('all (map measure-pin (list 0 1 2 3)))
    (else (display "error: did not understand message ")
          (display msg) (newline))))


;;;; LCD definitions

;; these pins are hardcoded into the primitive send-byte, don't change them
(define cs (bit 20))
(define sclk (bit 4))
(define sdata (bit 6))
(define reset (bit 21))

(define lcd-cmd 0)
(define lcd-data 1)

;; lcd commands

(define CASET #x15)
(define COMSCN #xBB)
(define DATCTL #xBC)
(define DISCTL #xCA)
(define DISINV #xA7)
(define DISON #xAF)
(define OSCON #xD1)
(define PTLOUT #xA9)
(define PWRCTR #x20)
(define RGBSET8 #xCE)
(define PASET #x75)
(define RAMWR #x5C)
(define SLPOUT #x94)
(define TMPGRD #x82)
(define VOLCTR #x81)
(define NOP #x25)

;; returns 0 when bitnr in the byte isn't set
(define (get-bit byte bitnr)  
  (logand (bit bitnr) byte))

(define colour-black 0)
(define colour-white #xff)
(define colour-red #xe0)
(define colour-green #x1C)
(define colour-blue 3)

;;;; LCD's Serial Protocol Interface

;;  hardcoded primitive:
;;    (define (send-byte cmd-bit byte) ...)

;; send a command byte over to the LCD, rest argments are sent as raw data
;;  uses pins sclk, sdata and cs
(define (send-command cmd . args)
  (send-byte lcd-cmd cmd)
  (apply send-data args))

;; send raw data over to the lcd
(define (send-data . data-list) 
  (if (pair? data-list)
      (begin 
        (send-byte lcd-data (car data-list))
        (apply send-data (cdr data-list)))))

;; Initializes the LCD screen.
;;  This needs to be called before doing anything with the LCD.
(define (init-lcd)
  (for-each output-pin (list cs sclk sdata reset))
  (set-pin cs)
  (clear-pin reset)
  (wait 1)
  (set-pin reset)
  (wait 20)
  (send-command DISCTL #x03 32 12 0)
  (send-command COMSCN #x01)
  (send-command OSCON)
  (send-command SLPOUT)
  (send-command VOLCTR 28 3)
  (send-command TMPGRD 0)
  (send-command PWRCTR #x0F)
  (wait 100)
  (send-command DISINV)
  (send-command PTLOUT)
  (send-command DATCTL 0 0 #x01 0) ; change last arg to #x02 or #x04 for 16-bit colors
  (send-command RGBSET8)  ; 8 bit colors
  (send-data 0 2 4 6 8 #xA #xC #xF)  ; red palette
  (send-data 0 2 4 6 8 #xA #xC #xF)  ; green alette
  (send-data 0 6 9 #xF)  ; blue palette
  (send-command NOP)
  (send-command DISON)
  (wait 200))

;; write a colour to the specified region on the LCD
;;  warning: does not check for invalid arguments
(define (fill-rectangle x y width height colour)
  (let ((x1 x)
        (x2 (+ x width -1))
        (y1 (+ y 2))         ; first 2 rows are offscreen
        (y2 (+ y height 1)))   ; 2 - 1
    (send-command PASET y1 y2)    ; which columns
    (send-command CASET x1 x2)   ; which rows
    (send-command RAMWR))          ; anounce raw memory data stream
  (loop (lambda () (send-byte lcd-data colour)) 
        (* width height)))

;;;; 
;;;; Writing Strings/Letters on screen
;;;;

(define a (list
           #f #f #t #t #f #f
           #f #t #f #f #t #f
           #f #t #f #f #t #f
           #f #t #t #t #t #f
           #f #t #f #f #t #f
           #f #t #f #f #t #f))

(define b (list
           #f #t #t #t #f #f
           #f #t #f #f #t #f
           #f #t #t #t #f #f
           #f #t #f #f #t #f
           #f #t #f #f #t #f
           #f #t #t #t #f #f))

(define c (list
           #f #f #t #t #t #f
           #f #t #f #f #f #f
           #f #t #f #f #f #f
           #f #t #f #f #f #f
           #f #t #f #f #f #f
           #f #f #t #t #t #f))

(define d (list
           #f #t #t #t #f #f
           #f #t #f #f #t #f
           #f #t #f #f #t #f
           #f #t #f #f #t #f
           #f #t #f #f #t #f
           #f #t #t #t #f #f))

(define e (list
           #f #t #t #t #t #f
           #f #t #f #f #f #f
           #f #t #f #f #f #f
           #f #t #t #t #f #f
           #f #t #f #f #f #f
           #f #t #t #t #t #f))

(define f (list
           #f #t #t #t #t #f
           #f #t #f #f #f #f
           #f #t #t #t #f #f
           #f #t #f #f #f #f
           #f #t #f #f #f #f
           #f #t #f #f #f #f))

(define g (list
           #f #f #t #t #t #f
           #f #t #f #f #f #f
           #f #t #f #f #f #f
           #f #t #f #t #t #f
           #f #t #f #f #t #f
           #f #f #t #t #f #f))

(define h (list
           #f #t #f #f #t #f
           #f #t #f #f #t #f
           #f #t #t #t #t #f
           #f #t #f #f #t #f
           #f #t #f #f #t #f
           #f #t #f #f #t #f))

(define i (list
           #f #t #t #t #f #f
           #f #f #t #f #f #f
           #f #f #t #f #f #f
           #f #f #t #f #f #f
           #f #f #t #f #f #f
           #f #t #t #t #f #f))

(define j (list
           #f #f #f #t #f #f
           #f #f #f #t #f #f
           #f #f #f #t #f #f
           #f #f #f #t #f #f
           #f #t #f #t #f #f
           #f #f #t #f #f #f))

(define k (list
           #f #t #f #f #t #f
           #f #t #f #t #f #f
           #f #t #t #t #f #f
           #f #t #t #t #f #f
           #f #t #f #t #f #f
           #f #t #f #f #t #f))

(define l (list
           #f #t #f #f #f #f
           #f #t #f #f #f #f
           #f #t #f #f #f #f
           #f #t #f #f #f #f
           #f #t #f #f #f #f
           #f #t #t #t #t #f))

(define m (list
           #f #t #f #f #f #t
           #f #t #t #f #t #t
           #f #t #f #t #f #t
           #f #t #f #f #f #t
           #f #t #f #f #f #t
           #f #t #f #f #f #t))

(define n (list
           #f #t #f #f #t #f
           #f #t #t #f #t #f
           #f #t #f #t #t #f
           #f #t #f #f #t #f
           #f #t #f #f #t #f
           #f #t #f #f #t #f))

(define o (list
           #f #f #t #t #t #f
           #f #t #f #f #f #t
           #f #t #f #f #f #t
           #f #t #f #f #f #t
           #f #t #f #f #f #t
           #f #f #t #t #t #f))

(define p (list
           #f #t #t #t #f #f
           #f #t #f #f #t #f
           #f #t #f #f #t #f
           #f #t #t #t #f #f
           #f #t #f #f #f #f
           #f #t #f #f #f #f))

(define q (list
           #f #f #t #t #t #f
           #f #t #f #f #f #t
           #f #t #f #f #f #t
           #f #t #f #t #f #t
           #f #t #f #f #t #t
           #f #f #t #t #t #t))

(define r (list
           #f #t #t #t #f #f
           #f #t #f #f #t #f
           #f #t #f #f #t #f
           #f #t #t #t #f #f
           #f #t #f #f #t #f
           #f #t #f #f #f #t))

(define s (list
           #f #t #t #t #t #f
           #f #t #f #f #f #f
           #f #t #f #f #f #f
           #f #f #t #t #f #f
           #f #f #f #f #t #f
           #f #t #t #t #f #f))

(define t (list
           #f #t #t #t #t #t
           #f #f #f #t #f #f
           #f #f #f #t #f #f
           #f #f #f #t #f #f
           #f #f #f #t #f #f
           #f #f #f #t #f #f))

(define u (list
           #f #t #f #f #f #t
           #f #t #f #f #f #t
           #f #t #f #f #f #t
           #f #t #f #f #f #t
           #f #t #f #f #f #t
           #f #f #t #t #t #f))

(define v (list
           #f #t #f #f #f #t
           #f #t #f #f #f #t
           #f #t #f #f #f #t
           #f #t #f #f #f #t
           #f #f #t #f #t #f
           #f #f #f #t #f #f))

(define w (list
           #f #t #f #f #f #t
           #f #t #f #f #f #t
           #f #t #f #f #f #t
           #f #t #f #t #f #t
           #f #t #f #t #f #t
           #f #f #t #f #t #f))

(define x (list
           #f #t #f #f #f #t
           #f #t #f #f #f #t
           #f #f #t #f #t #f
           #f #f #f #t #f #f
           #f #f #t #f #t #f
           #f #t #f #f #f #t))

(define y (list
           #f #t #f #f #f #t
           #f #t #f #f #f #t
           #f #f #t #f #t #f
           #f #f #f #t #t #f
           #f #f #f #t #f #f
           #f #f #f #t #f #f))

(define z (list
           #f #t #t #t #t #t
           #f #f #f #f #t #f
           #f #f #f #t #f #f
           #f #f #t #f #f #f
           #f #t #f #f #f #f
           #f #t #t #t #t #t))

(define space (list
               #f #f #f #f #f #f
               #f #f #f #f #f #f
               #f #f #f #f #f #f
               #f #f #f #f #f #f
               #f #f #f #f #f #f
               #f #f #f #f #f #f))

(define charmap
  (list
   (cons #\  space)
   (cons #\a a)
   (cons #\b b)
   (cons #\c c)
   (cons #\d d)
   (cons #\e e)
   (cons #\f f)
   (cons #\g g)
   (cons #\h h)
   (cons #\i i)
   (cons #\j j)
   (cons #\k k)
   (cons #\l l)
   (cons #\m m)
   (cons #\n n)
   (cons #\o o)
   (cons #\p p)
   (cons #\q q)
   (cons #\r r)
   (cons #\s s)
   (cons #\t t)
   (cons #\u u)
   (cons #\v v)
   (cons #\w w)
   (cons #\x x)
   (cons #\y y)
   (cons #\z z)))

;; Write a string of pre-defined letters to the screen ( black on white )
(define (write-string str x y)
  (let ((i 0))
    (loop (lambda ()
            (let ((letter (cdr (assoc (string-ref str i) charmap))))  ;ohgod
              (write-letter letter (+ x (* i 6)) y 0 #xff)
              (set! i (+ i 1))))
          (string-length str))))

;; Write a single pre-defined letter of size 6x6 to the screen
(define (write-letter letter x y foreground-colour background-colour)
  (let ((x1 x)
        (x2 (+ x 5))
        (y1 (+ y 2))         ; first 2 rows are offscreen
        (y2 (+ y 7))
        (colour-map 
         (map 
          (lambda (bit)
            (if bit
                foreground-colour
                background-colour))
          letter)))
    (display (list x1 x2 y1 y2))
    (send-command PASET y1 y2)
    (send-command CASET x1 x2)
    (send-command RAMWR)
    (for-each (lambda (colour)
                (send-byte lcd-data colour))
              colour-map)))
