(define of (open-output-file "boot"))
;;;; Boot File, only execute this code once per memory flash

;;;; Utility Functions
(write '(begin 
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

(define (send-bit bit)
  (clear-pin sclk)
  (if (eq? bit 0)
      (clear-pin sdata)
      (set-pin sdata))
  (set-pin sclk))

(define (send-byte cmd data)
  (clear-pin cs)
  ;; send the command bit
  (send-bit cmd)
  ;; send the byte
  (let loop ((bitnr 7))
    (if (>= bitnr 0)
        (begin
          (send-bit (get-bit data bitnr))
          (loop (- bitnr 1)))))
  (set-pin cs))

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
) of)

(write '(begin 
(define (vector-position-list size ==? . lst) ;lst is used if creating from scheme list
  (define vect (make-vector size))
  (define ptr 0)
  
  (define (checkpos pos)
    (if (or (< pos 0)
            (>= pos ptr)) ; Also incorporates empty check
        (error vector-position-list.checkpos "invalid position")))
  
  (define (storage-move-left src len dest)
    (if (> len 0)
        (begin
          (vector-set! vect dest (vector-ref vect src))
          (storage-move-left (+ src 1) (- len 1) (+ dest 1)))))
  
  (define (storage-move-right src len dest)
    (define (iter ilen)
      (if (>= ilen 0)
          (begin
            (vector-set! vect (+ ilen dest) (vector-ref vect (+ ilen src)))
            (iter (- ilen 1)))))
    (iter (- len 1)))
  
  ; Public functions
  
  (define (getnext pos)
    (checkpos pos)
    
    (if (= pos (- ptr 1))
        (error vector-position-list.getnext "position has no next")
        (+ pos 1)))
  
  (define (getprev pos)
    (checkpos pos)
    
    (if (= pos 0)
        (error vector-position-list.getprev "position has no previous")
        (- pos 1)))
  
  (define (getval pos)
    (checkpos pos)
    
    (vector-ref vect pos))
  
  (define (gethas-next? pos)
    (checkpos pos)
    
    (not (= pos (- ptr 1))))
  
  (define (gethas-prev? pos)
    (checkpos pos)
    
    (not (= pos 0)))
  
  (define (update! pos val)
    (checkpos pos)
    
    (vector-set! vect pos val))
  
  (define (from-scheme-list lst)
    (if (not (null? lst))
        (begin (add-after! (car lst))
               (from-scheme-list (cdr lst)))))
  (define (length)
    ptr)
  (define (full?)
    (= (length) size))
  (define (empty?)
    (= (length) 0))
  (define (for-each func)
    (define (iter thispos)
      (func (getval thispos))
      (if (gethas-next? thispos)
          (iter (getnext thispos))))
    (if (not (empty?))
        (iter (first-position)))
    'ok)
  (define (for-each-counting func)
    (define (iter thispos i)
      (func (getval thispos) i)
      (if (gethas-next? thispos)
          (iter (getnext thispos) (+ i 1))))
    (if (not (empty?))
        (iter (first-position) 0))
    'ok)
  (define (map func new==?)
    (let ((res (vector-position-list size new==?)))
      (define (iter thispos)
        (res 'add-after! (func (getval thispos)))
        (if (gethas-next? thispos)
            (iter (getnext thispos))))
      (if (not (empty?))
          (iter (first-position)))
      res))
  (define (map! func new==?)
    (define (iter thispos)
      (update! thispos (func (getval thispos)))
      (if (gethas-next? thispos)
          (iter (getnext thispos))))
    (set! ==? new==?)
    (if (not (empty?))
        (iter (first-position)))
    'ok)
  (define (foldl comb zero)
    (define (iter thispos res)
      (if (gethas-next? thispos)
          (iter (getnext thispos) (comb res (getval thispos)))
          (comb res (getval thispos))))
    (if (empty?)
        zero
        (iter (first-position) zero)))
  (define (foldr comb zero)
    (define (iter thispos res)
      (if (gethas-next? thispos)
          (iter (getprev thispos) (comb res (getval thispos)))
          (comb res (getval thispos))))
    (if (empty?)
        zero
        (iter (last-position) zero)))
  
  (define (first-position)
    (if (empty?)
        (error 'vector-position-list.first-position "first position requested, but list is empty!")
        0))
  
  (define (last-position)
    (if (empty?)
        (error 'vector-position-list.last-position "last position requested, but list is empty!")
        (- ptr 1)))
  
  (define (find value)
    (define (iter pos)
      (cond ((==? (getval pos) value) pos)
            ((gethas-next? pos) (iter (getnext pos)))
            (else #f)))
    (if (empty?)
        #f
        (iter (first-position))))
  
  (define (find-eq value)
    (define (iter pos)
      (cond ((eq? (getval pos) value) pos)
            ((gethas-next? pos) (iter (getnext pos)))
            (else #f)))
    (if (empty?)
        #f
        (iter (first-position))))
  
  (define (delete! pos)
    (checkpos pos)
    (let ((behind (+ pos 1)))
      (storage-move-left behind (- (length) behind) pos)
      (set! ptr (- ptr 1))))
  
  (define (add-before! val . pos)
    (let ((realpos (if (null? pos)
                       (if (empty?)
                           -1
                           (first-position))
                       (car pos))))
      (if (full?)
          (error 'vector-position-list.add-before! "list is full")
          (begin
            (storage-move-right realpos (- (length) realpos) (+ realpos 1))
            (vector-set! vect realpos val)
            (set! ptr (+ ptr 1))))))
  
  (define (add-after! val . pos)
    (let* ((realpos (if (null? pos)
                        (if (empty?)
                            -1
                            (last-position))
                        (car pos)))
           (behind (+ realpos 1)))
      (if (full?)
          (error 'vector-position-list.add-after! "list is full")
          (begin
            (storage-move-right behind (- (length) behind) (+ behind 1))
            (vector-set! vect behind val)
            (set! ptr (+ ptr 1))))))
  
  (define (duplicate)
    (define (iter lst pos)
      (lst 'add-after! (getval pos))
      (if (gethas-next? pos)
          (iter lst (getnext pos))
          lst))
    (if (empty?)
        (vector-position-list ==?)
        (iter (vector-position-list ==?) (first-position))))
  
  (define (debug-print-complete)
    (define (iter pos)
      (display (getval pos))
      (display " ")
      (if (gethas-next? pos)
          (iter (getnext pos))))
    (cond ((empty?) (display "()")
                    (newline))
          (else (display "(")
                (iter (first-position))
                (display ")"))))
  
  (define (to-scheme-list)
    (define (rec pos)
      (if (gethas-next? pos)
          (cons (getval pos) (rec (getnext pos)))
          (cons (getval pos) '())))
    (if (empty?)
        '()
        (rec (first-position))))
  
  (define (vector-position-list-object msg . args)
    (let ((my-param (make-param args 'vector-position-list)))
      (case msg
        ('length (length))
        ('full? (full?))
        ('empty? (empty?))
        ('for-each (for-each (my-param 1)))
        ('for-each-counting (for-each-counting (my-param 1)))
        ('map (map (my-param 1) (my-param 2)))
        ('map! (map! (my-param 1) (my-param 2)))
        ('foldl (foldl (my-param 1) (my-param 2)))
        ('foldr (foldr (my-param 1) (my-param 2)))
        ('first-position (first-position))
        ('last-position (last-position))
        ('find (find (my-param 1)))
        ('find-eq (find-eq (my-param 1)))
        ('delete! (delete! (my-param 1)))
        ('add-before! (apply add-before! (my-param 1) (cdr args)))
        ('add-after! (apply add-after! (my-param 1) (cdr args)))
        ('next (getnext (my-param 1)))
        ('prev (getprev (my-param 1)))
        ('value (getval (my-param 1)))
        ('update! (update! (my-param 1) (my-param 2)))
        ('has-next? (gethas-next? (my-param 1)))
        ('has-prev? (gethas-prev? (my-param 1)))
        ('print (debug-print-complete))
        ('duplicate (duplicate))
        ('to-scheme-list (to-scheme-list))
        (else (error 'vector-position-list "message not understood: ~S" msg)))))
  
  (if (not (null? lst))
    (from-scheme-list (car lst)))
  
  vector-position-list-object)
) of)

(write '(begin 
; Makes the type of position list selectable by load
(define position-list vector-position-list)

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
(define (make-param prms sender)
  (lambda (n)
    (param prms n sender)))



(define (clamp x low high)
  (cond ((< x low) low)
        ((> x high) high)
        (else x)))

(define (safe-optionalbool bl)
  (and (not (null? bl))
       bl))



(define (random-generator timer . dontrun)
  (define pausesave 0)
  (define (resume)
    (timer.stop TIMER0) ; safety measure (to not mess up possible already running timers)
    (timer.set-PR TIMER0 0)
    (timer.MR-control-over TIMER0 0 0 1 0)
    (timer.set-MR TIMER0 TxMR0 100000)
    (timer.reset-n-start TIMER0))
  (define (pause)
    (timer.stop TIMER0))
  (define (get min max)
    (+ min (modulo (timer.value TIMER0)
                   (+ (- max min) 1))))
  
  (define (random-generator-object msg . args)
    (let ((my-param (make-param args 'random-generator-object)))
      (case msg
        ('resume (resume))
        ('pause (pause))
        ('get (get (my-param 1) (my-param 2)))
        (else (error 'random-generator-object "message \"~S\" unknown" msg)))))
  (resume)
  random-generator-object)


(define rand (random-generator TIMER0))


(define (input-base init-extra proc-that-gets-input)
  (define (check)
    (proc-that-gets-input))
  
  (define (input-base-object msg . args)
    (let ((my-param (make-param args 'input-base-object)))
      (case msg
        ('check (check))
        (else (error 'input-base-object "message \"~S\" unknown" msg)))))
  (init-extra)
  input-base-object)



(define (input-digital gpio pin-id)
  (define (this-init)
    (input-pin (pin pin-id)))
  
  (define (this-check)
    (read-pin pin-id))
  
  (define inpt (input-base this-init this-check))
  
  (define (input-digital-object msg . args)
    (let ((my-param (make-param args 'input-digital-object)))
      (case msg
        (else (apply inpt msg args)))))
  
  input-digital-object)



(define (input-analog adc-in-id trigger-proc)
  (define (this-init)
    'ok) ;; init PINSEL reg?
  
  (define (this-check)
    (trigger-proc (round (* 100 (measure adc-in-id)))))
  
  (define inpt (input-base this-init this-check))
  
  (define (input-analog-object msg . args)
    (let ((my-param (make-param args 'input-analog-object)))
      (case msg
        (else (apply inpt msg args)))))
  
  input-analog-object)


(define (input-binding inpt proc)
  (define (check)
    (if (inpt 'check)
        (proc)))
  
  (define (input-binding-object msg . args)
    (let ((my-param (make-param args 'input-binding-object)))
      (case msg
        ('check (check))
        (else (error 'input-binding-object "message \"~S\" unknown" msg)))))
  
  input-binding-object)


(define (input-mapper)
  (define mappings '()) ;; Using an ordinary list here. Could be changed later
  
  (define (map input proc)
    (set! mappings (cons (input-binding input proc) mappings)))
  
  (define (check)
    (for-each (lambda (x)
                (x 'check)) mappings))
  
  (define (input-mapper-object msg . args)
    (let ((my-param (make-param args 'input-mapper-object)))
      (case msg
        ('map (map (my-param 1) (my-param 2)))
        ('check (check))
        (else (error 'input-mapper-object "message \"~S\" unknown" msg)))))
  
  input-mapper-object)


(define (statusbar levels fill-rectangle draw-string)
  
  (define (update)
    (define (convert-to-RGB332 pct)
      (define (rescale-RG f)
        (round (* f 7)))
      
      (let ((red (rescale-RG (clamp (/ pct 50) 0 1)))
            (green (rescale-RG (/ (- 50 (clamp (- pct 50) 0 50)) 50))))
        (logior (ash red 5) (ash green 2))))
    
    
    (levels 'for-each-counting (lambda (level i)
                                 (fill-rectangle (+ (* 21 i) 10) 10 4 4 (convert-to-RGB332 (level 'percent))))))
  
  (define (totalredraw)
    (draw-string "hgr" 3 23)
    (draw-string "slp" 24 23)
    (draw-string "gem" 45 23)
    (draw-string "reb" 66 23)
    (draw-string "gez" 87 23)
    (draw-string "uit" 108 23)
    (update))
  
  (define (statusbar-object msg . args)
    (let ((my-param (make-param args 'statusbar-object)))
      (case msg
        ('totalredraw (totalredraw))
        ('update (update))
        (else (error 'statusbar-object "message \"~S\" unknown" msg)))))
  
  statusbar-object)


(define (lcd bundle)
  (define sbar (statusbar (bundle 'send-to-all 'level) fill-rectangle write-string))
  
  (define (init)
    (init-lcd)
    (sbar 'totalredraw))
  
  (define (update)
    (sbar 'update))
  
  (define (lcd-object msg . args)
    (let ((my-param (make-param args 'lcd-object)))
      (case msg
        ('update (update))
        (else (error 'lcd-object "message \"~S\" unknown" msg)))))
  
  (init)
  
  lcd-object)


(define (buzzer port mpin)
  (define (init)
    (output-pin (pin mpin)))
  
  (define (beep time)
    (define (blink delay)
      (define (iter itime)
        (if (> itime 0)
            (begin (set-pin (pin mpin))
                   (wait-us delay)
                   (clear-pin (pin mpin))
                   (wait-us delay)
                   (iter (- itime 1)))))
      (iter (/ time delay 2)))
    (blink (round (/ 1000000 440))))
  
  (define (buzzer-object msg . args)
    (let ((my-param (make-param args 'buzzer-object)))
      (case msg
        ('beep (beep (my-param 1)))
        (else (error 'buzzer-object "message \"~S\" unknown" msg)))))
  
  (init)
  
  buzzer-object)
) of)

(write '(begin 
(define (fsm-transition condition target . trans-action)
  
  ;=========================================================
  ; Method check
  ; Spec: (  -> fsm-state-object U { #f } )
  ; Desc: Call this procedure on every state-change check.
  ;       It will return the state object specified in target when the condition is met, and #f if it is not.
  ; Args: /
  ;=========================================================
  (define (check)
    (if (condition)
        target
        #f))
  
  (define (act)
    (if (and (not (null? trans-action))
             (not (null? (car trans-action))))
        ((car trans-action))))
  
  (define (fsm-transition-object msg . args)
    (case msg
      ('check (check))
      ('act (act))
      (else (error 'fsm-transition-object "message \"~S\" unknown" msg))))
  fsm-transition-object)


(define (fsm-state entry-action exit-action size)
  (define trans-list (position-list size eq?))
  
  ;=================================================
  ; Method add-transition
  ; Spec: ( fsm-transition -> { #<void> } )
  ; Desc: adds a new transition to the state
  ; Args: trans - the transition to be added
  ;=================================================
  (define (add-transition! trans)
    (if (trans-list 'full?)
        (error 'fsm-state.add-transition! "transition list is full. check your fsm-state size")
        (trans-list 'add-after! trans)))
  
  ;=================================================
  ; Method next-state
  ; Spec: (  -> fsm-state )
  ; Desc: "check"'s all transitions and returns the first non-#f value returned.
  ;       Returns #f if all of the transitions return #f.
  ;       The FSM should stay in its current state in this case.
  ;       NOTE1: transitions are checked in the order in which they were added
  ;       NOTE2: when a state is returned, the FSM MUST transition to that state
  ; Args: /
  ;=================================================
  (define (next-state)
    (define (find-next-state pos)
      (let* ((the-trans (trans-list 'value pos))
             (trans-check (the-trans 'check)))
        (cond (trans-check (the-trans 'act)
                           trans-check)
              ((trans-list 'has-next? pos) (find-next-state (trans-list 'next pos)))
              (else #f))))
    (if (trans-list 'empty?)
        #f
        (find-next-state (trans-list 'first-position))))
  
  ;=================================================
  ; Method enter
  ; Spec: (  -> sobj )
  ; Desc: should be called when the fsm enters this state;
  ;       executes the entry event
  ; Args: /
  ;=================================================
  (define (enter)
    (if (not (null? entry-action))
        (entry-action)))
  
  ;=================================================
  ; Method leave
  ; Spec: (  -> sobj )
  ; Desc: should be called when the fsm leaves this state;
  ;       executes the exit event
  ; Args: /
  ;=================================================
  (define (leave)
    (if (not (null? exit-action))
        (exit-action)))
  
  (define (fsm-state-object msg . args)
    (let ((my-param (make-param args 'fsm-state-object)))
      (case msg
        ('add-transition! (add-transition! (my-param 1)))
        ('next-state (next-state))
        ('enter (enter))
        ('leave (leave))
        (else (error 'fsm-state-object "message \"~S\" unknown" msg)))))
  
  
  fsm-state-object)


(define (fsm state)
  
  ;===================================================
  ; Method transition
  ; Spec: (  -> { 'ok } )
  ; Desc: Is called to possibly transition the fsm to a next state
  ; Args: /
  ;===================================================
  
  (define (transition)
    (let ((next-state (state 'next-state)))
      (if next-state
          (begin
            (state 'leave)
            (set! state next-state)
            (state 'enter)
            'ok)
          'ok)))
  
  (define (fsm-object msg . args)
    (let ((my-param (make-param args 'fsm-object)))
      (case msg
        ('transition (transition))
        ('get-current-state state)
        (else (error 'fsm-object "message \"~S\" unknown" msg)))))
  
  
  fsm-object)

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
        ('high? (high?))
        ('low? (low?))
        ('critical? (critical?))
        ('deadly? (deadly?))
        ('percent level)
        (else (error 'level-object "message \"~S\" unknown" msg)))))
  level-object)
) of)

(write '(begin 
(define (hunger-fsm external)
  (define hunger-level (need-level))
  (define food-offered #f)
  
  ;===================================================
  ; Method eat-the-food!
  ; Spec: (  -> { #<void> } )
  ; Desc: set food-offered to #f and calls lower-hunger-level!
  ; Args: /
  ;===================================================
  (define (eat-the-food!)
    (set! food-offered #f)
    (hunger-level 'lower!))

  ;===================================================
  ; Method reject-the-food!
  ; Spec: (  -> { #<void> } )
  ; Desc: set food-offered to #f
  ; Args: /
  ;===================================================
  (define (reject-the-food!)
    (set! food-offered #f))

  ;===================================================
  ; Method rebels?
  ; Spec: (  -> { #<void> } )
  ; Desc: does external call to see if the animal rebels.
  ; Args: /
  ;===================================================
  (define (rebels?)
    (external 'docility 'rebels?))

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
    (state-satisfied 'add-transition! (fsm-transition (lambda () (hunger-level 'high?)) state-hungry))
    (state-satisfied 'add-transition! (fsm-transition (lambda () (and food-offered (not (rebels?)))) state-eating))
    (state-satisfied 'add-transition! (fsm-transition (lambda () (and food-offered (rebels?))) state-refused))
    (state-satisfied 'add-transition! (fsm-transition true-condition? state-satisfied))
    ;;---
    (state-hungry 'add-transition! (fsm-transition (lambda () (and food-offered (not (rebels?)))) state-eating))
    (state-hungry 'add-transition! (fsm-transition (lambda () (and food-offered (rebels?))) state-refused))
    (state-hungry 'add-transition! (fsm-transition (lambda () (hunger-level 'low?)) state-satisfied))
    (state-hungry 'add-transition! (fsm-transition (lambda () (hunger-level 'deadly?)) state-dead))
    (state-hungry 'add-transition! (fsm-transition true-condition? state-hungry))
    ;;---
    (state-eating 'add-transition! (fsm-transition (lambda () (hunger-level 'low?)) state-satisfied))
    (state-eating 'add-transition! (fsm-transition (lambda () (hunger-level 'high?)) state-hungry))
    ;;---
    (state-refused 'add-transition! (fsm-transition true-condition? state-hungry)))

    
  (define state-satisfied (fsm-state (lambda () (hunger-level 'raise!)) '() 4))
  (define state-hungry (fsm-state (lambda () (hunger-level 'raise!)) '() 5))
  (define state-eating (fsm-state eat-the-food! '() 2))
  (define state-refused (fsm-state reject-the-food! '() 1))
  (define state-dead (fsm-state '() '() 0))
  
  (define my-fsm (fsm state-satisfied))
  
  (define (hunger-fsm-object msg . args)
    (let ((my-param (make-param args 'hunger-fsm-object)))
      (case msg
        ('hungry? (eq? (my-fsm 'get-current-state) state-hungry))
        ('satisfied? (eq? (my-fsm 'get-current-state) state-satisfied))
        ('eating? (eq? (my-fsm 'get-current-state) state-eating))
        ('refused? (eq? (my-fsm 'get-current-state) state-refused))
        ('dead? (eq? (my-fsm 'get-current-state) state-dead))
        ('feed (set! food-offered #t))
        ('level hunger-level)
        (else (apply my-fsm msg args)))))
  
  (init-transitions)

  hunger-fsm-object)


(define (mood-fsm external)
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
    (unhappiness-level 'lower!)
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
  ; Method rebels?
  ; Spec: (  -> { #<void> } )
  ; Desc: does external call to see if the animal rebels.
  ; Args: /
  ;===================================================
  (define (rebels?)
    (external 'docility 'rebels?))

  ;===================================================
  ; Method init-transitions
  ; Spec: (  -> { #<void> } )
  ; Desc: adds transitions to all states (must be called after the states are created)
  ; Args: /
  ;===================================================
  (define (init-transitions)
    (state-happy 'add-transition! (fsm-transition (lambda () (unhappiness-level 'high?)) state-unhappy))
    (state-happy 'add-transition! (fsm-transition (lambda () (and play-game (not (rebels?)))) state-playing-game))
    (state-happy 'add-transition! (fsm-transition (lambda () (and play-game (rebels?))) state-refused))
    (state-happy 'add-transition! (fsm-transition true-condition? state-happy))
    ;;---
    (state-unhappy 'add-transition! (fsm-transition (lambda () (and play-game (not (rebels?)))) state-playing-game))
    (state-unhappy 'add-transition! (fsm-transition (lambda () (and play-game (rebels?))) state-refused))
    (state-unhappy 'add-transition! (fsm-transition (lambda () (unhappiness-level 'low?)) state-happy))
    (state-unhappy 'add-transition! (fsm-transition (lambda () (unhappiness-level 'deadly?)) state-dead))
    (state-unhappy 'add-transition! (fsm-transition true-condition? state-unhappy))
    ;;---
    (state-playing-game 'add-transition! (fsm-transition (lambda () (unhappiness-level 'low?)) state-happy))
    (state-playing-game 'add-transition! (fsm-transition (lambda () (unhappiness-level 'high?)) state-unhappy))
    ;;---
    (state-refused 'add-transition! (fsm-transition true-condition? state-unhappy)))

  (define state-happy (fsm-state (lambda () (unhappiness-level 'raise!)) '() 4))
  (define state-unhappy (fsm-state (lambda () (unhappiness-level 'raise!)) '() 5))
  (define state-playing-game (fsm-state play-a-game '() 2))
  (define state-refused (fsm-state reject-a-game '() 1))
  (define state-dead (fsm-state '() '() 0))    ; suicide? :S
  
  (define my-fsm (fsm state-happy))
  
  (define (mood-fsm-object msg . args)
    (let ((my-param (make-param args 'mood-fsm-object)))
      (case msg
        ('unhappy? (eq? (my-fsm 'get-current-state) state-unhappy))
        ('happy? (eq? (my-fsm 'get-current-state) state-happy))
        ('playing-game? (eq? (my-fsm 'get-current-state) state-playing-game))
        ('refused? (eq? (my-fsm 'get-current-state) state-refused))
        ('dead? (eq? (my-fsm 'get-current-state) state-dead))
        ('play-a-game (set! play-game #t))
        ('level unhappiness-level)
        (else (apply my-fsm msg args)))))

  (init-transitions)

  mood-fsm-object)


(define (waste-fsm external)
  (define waste-level (need-level))
  (define clean-waste #f)
  
  ;===================================================
  ; Method clean
  ; Spec: (  -> { #<void> } )
  ; Desc: set clean-waste to #f and calls lower-waste-level!
  ; Args: /
  ;===================================================
  (define (clean)
    (set! clean-waste #f)
    (waste-level 'lower!))

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
    (state-clean 'add-transition! (fsm-transition (lambda () (waste-level 'high?)) state-disgusting))
    (state-clean 'add-transition! (fsm-transition (lambda () clean-waste) state-cleaning))
    (state-clean 'add-transition! (fsm-transition true-condition? state-clean))
    ;;---
    (state-disgusting 'add-transition! (fsm-transition (lambda () clean-waste) state-cleaning))
    (state-disgusting 'add-transition! (fsm-transition (lambda () (waste-level 'low?)) state-clean))
    (state-disgusting 'add-transition! (fsm-transition (lambda () (waste-level 'deadly?)) state-sickening))
    (state-disgusting 'add-transition! (fsm-transition true-condition? state-disgusting))
    ;;---
    (state-cleaning 'add-transition! (fsm-transition (lambda () (waste-level 'low?)) state-clean))
    (state-cleaning 'add-transition! (fsm-transition (lambda () (waste-level 'high?)) state-disgusting))
    ;;---
    (state-sickening 'add-transition! (fsm-transition true-condition? state-disgusting)))


  (define state-clean (fsm-state (lambda () (waste-level 'raise!)) '() 3))
  (define state-disgusting (fsm-state (lambda () (waste-level 'raise!)) '() 4))
  (define state-cleaning (fsm-state clean '() 2))
  (define state-sickening (fsm-state (lambda () (external 'health 'sicken!)) '() 1))
  
  (define my-fsm (fsm state-clean))
  
  (define (waste-fsm-object msg . args)
    (let ((my-param (make-param args 'waste-fsm-object)))
      (case msg
        ('disgusting? (eq? (my-fsm 'get-current-state) state-disgusting))
        ('clean? (eq? (my-fsm 'get-current-state) state-clean))
        ('cleaning? (eq? (my-fsm 'get-current-state) state-cleaning))
        ('sickening? (eq? (my-fsm 'get-current-state) state-sickening))
        ('dead? #f)
        ('clean (set! clean-waste #t))
        ('level waste-level)
        (else (apply my-fsm msg args)))))
  
  (init-transitions)

  waste-fsm-object)
) of)

(write '(begin 
(define (sleep-fsm external)
  (define tiredness-level (need-level))
  (define put-in-bed #f)
  
  ;===================================================
  ; Method sleep
  ; Spec: (  -> { #<void> } )
  ; Desc: set put-in-bed to #f and calls lower-tiredness-level!
  ; Args: /
  ;===================================================
  (define (sleep)
    (wait 2)
    (set! put-in-bed #f)
    (tiredness-level 'lower!))

  ;===================================================
  ; Method dont-sleep
  ; Spec: (  -> { #<void> } )
  ; Desc: set put-in-bed to #f and calls lower-tiredness-level!
  ; Args: /
  ;===================================================
  (define (dont-sleep)
    (set! put-in-bed #f))

  ;===================================================
  ; Method rebels?
  ; Spec: (  -> { #<void> } )
  ; Desc: does external call to see if the animal rebels.
  ; Args: /
  ;===================================================
  (define (rebels?)
    (external 'docility 'rebels?))

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
    (state-awake 'add-transition! (fsm-transition (lambda () (tiredness-level 'high?)) state-tired))
    (state-awake 'add-transition! (fsm-transition (lambda () (and put-in-bed (not (rebels?)))) state-asleep))
    (state-awake 'add-transition! (fsm-transition (lambda () (and put-in-bed (rebels?))) state-refused))
    (state-awake 'add-transition! (fsm-transition true-condition? state-awake))
    ;;---
    (state-tired 'add-transition! (fsm-transition (lambda () (and put-in-bed (not (rebels?)))) state-asleep))
    (state-tired 'add-transition! (fsm-transition (lambda () (and put-in-bed (rebels?))) state-refused))
    (state-tired 'add-transition! (fsm-transition (lambda () (tiredness-level 'low?)) state-awake))
    (state-tired 'add-transition! (fsm-transition (lambda () (tiredness-level 'deadly?)) state-dead))
    (state-tired 'add-transition! (fsm-transition true-condition? state-tired))
    ;;---
    (state-asleep 'add-transition! (fsm-transition (lambda () (tiredness-level 'low?)) state-awake))
    (state-asleep 'add-transition! (fsm-transition (lambda () (tiredness-level 'high?)) state-tired))
    ;;---
    (state-refused 'add-transition! (fsm-transition true-condition? state-tired)))


  (define state-awake (fsm-state (lambda () (tiredness-level 'raise!)) '() 4))
  (define state-tired (fsm-state (lambda () (tiredness-level 'raise!)) '() 5))
  (define state-asleep (fsm-state sleep '() 2))
  (define state-refused (fsm-state dont-sleep '() 1))
  (define state-dead (fsm-state '() '() 0))
  
  (define my-fsm (fsm state-awake))
  
  (define (sleep-fsm-object msg . args)
    (let ((my-param (make-param args 'sleep-fsm-object)))
      (case msg
        ('tired? (eq? (my-fsm 'get-current-state) state-tired))
        ('awake? (eq? (my-fsm 'get-current-state) state-awake))
        ('sleeping? (eq? (my-fsm 'get-current-state) state-sleeping))
        ('refused? (eq? (my-fsm 'get-current-state) state-refused))
        ('dead? (eq? (my-fsm 'get-current-state) state-dead))
        ('put-in-bed (set! put-in-bed #t))
        ('level tiredness-level)
        (else (apply my-fsm msg args)))))
  
  (init-transitions)
  
  sleep-fsm-object)


(define (health-fsm external)
  (define sickness-level (need-level))
  (define give-medicine #f)
  
  ;===================================================
  ; Method heal
  ; Spec: (  -> { #<void> } )
  ; Desc: set give-medicine to #f and calls lower-sickness-level!
  ; Args: /
  ;===================================================
  (define (heal)
    (set! give-medicine #f)
    (sickness-level 'lower!))

  ;===================================================
  ; Method refuse-medicine
  ; Spec: (  -> { #<void> } )
  ; Desc: set give-medicine to #f
  ; Args: /
  ;===================================================
  (define (refuse-medicine)
    (set! give-medicine #f))

  ;===================================================
  ; Method rebels?
  ; Spec: (  -> { #<void> } )
  ; Desc: does external call to see if the animal rebels.
  ; Args: /
  ;===================================================
  (define (rebels?)
    (external 'docility 'rebels?))

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
    (state-healthy 'add-transition! (fsm-transition (lambda () (sickness-level 'high?)) state-sick))
    (state-healthy 'add-transition! (fsm-transition (lambda () (and give-medicine (not (rebels?)))) state-healing))
    (state-healthy 'add-transition! (fsm-transition (lambda () (and give-medicine (rebels?))) state-refused))
    (state-healthy 'add-transition! (fsm-transition true-condition? state-healthy))
    ;;---
    (state-sick 'add-transition! (fsm-transition (lambda () (and give-medicine (not (rebels?)))) state-healing))
    (state-sick 'add-transition! (fsm-transition (lambda () (and give-medicine (rebels?))) state-refused))
    (state-sick 'add-transition! (fsm-transition (lambda () (sickness-level 'low?)) state-healthy))
    (state-sick 'add-transition! (fsm-transition (lambda () (sickness-level 'deadly?)) state-dead))
    (state-sick 'add-transition! (fsm-transition true-condition? state-sick))
    ;;---
    (state-healing 'add-transition! (fsm-transition (lambda () (sickness-level 'low?)) state-healthy))
    (state-healing 'add-transition! (fsm-transition (lambda () (sickness-level 'high?)) state-sick))
    ;;---
    (state-refused 'add-transition! (fsm-transition true-condition? state-sick)))


  (define state-healthy (fsm-state '() '() 4))
  (define state-sick (fsm-state '() '() 5))
  (define state-healing (fsm-state heal '() 2))
  (define state-refused (fsm-state refuse-medicine '() 1))
  (define state-dead (fsm-state '() '() 0))
  
  (define my-fsm (fsm state-healthy))
  
  (define (health-fsm-object msg . args)
    (let ((my-param (make-param args 'health-fsm-object)))
      (case msg
        ('sick? (eq? (my-fsm 'get-current-state) state-sick))
        ('healthy? (eq? (my-fsm 'get-current-state) state-healthy))
        ('healing? (eq? (my-fsm 'get-current-state) state-healing))
        ('refused? (eq? (my-fsm 'get-current-state) state-refused))
        ('dead? (eq? (my-fsm 'get-current-state) state-dead))
        ('sicken! (sickness-level 'raise!))
        ('cure (set! give-medicine #t))
        ('level sickness-level)
        (else (apply my-fsm msg args)))))
  
  (init-transitions)

  health-fsm-object)


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
    (state-docile 'add-transition! (fsm-transition true-condition? state-docile))
    ;;---
    (state-rebellish 'add-transition! (fsm-transition (lambda () punishment-given) state-getting-punished))
    (state-rebellish 'add-transition! (fsm-transition (lambda () (rebelliosity-level 'low?)) state-docile))
    (state-rebellish 'add-transition! (fsm-transition true-condition? state-rebellish))
    ;;---
    (state-getting-punished 'add-transition! (fsm-transition (lambda () (rebelliosity-level 'low?)) state-docile))
    (state-getting-punished 'add-transition! (fsm-transition (lambda () (rebelliosity-level 'high?)) state-rebellish)))


  (define state-docile (fsm-state (lambda () (rebelliosity-level 'raise!)) '() 2))
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
) of)

(write '(begin 
(define (tama-fsm-bundler)
  (define fsm-id car)
  (define fsm-obj cdr)
  
  (define fsms (position-list 6 (lambda (x y)
                                  (eq? (fsm-id x) y))))
  
  (define (add-fsms)
    (fsms 'add-after! (cons 'hunger (hunger-fsm fsm-external-procedure-call)))
    (fsms 'add-after! (cons 'sleep (sleep-fsm fsm-external-procedure-call)))
    (fsms 'add-after! (cons 'mood (mood-fsm fsm-external-procedure-call)))
    (fsms 'add-after! (cons 'docility (docility-fsm fsm-external-procedure-call)))
    (fsms 'add-after! (cons 'health (health-fsm fsm-external-procedure-call)))
    (fsms 'add-after! (cons 'waste (waste-fsm fsm-external-procedure-call))))
  
  (define (fsm-external-procedure-call fsm command . args)
    (let ((the-fsm (fsm-obj (fsms 'value (fsms 'find fsm)))))
      (apply the-fsm command args)))
  
  (define (transition-all)
    (fsms 'for-each (lambda (x)
                      ((fsm-obj x) 'transition))))
  
  (define (one-dead?)
    (let ((death-map (fsms 'map (lambda (x)                                  ;;;;; TODO: REMOVE DBUG CODE
                                  (let ((isdead ((fsm-obj x) 'dead?)))
                                    (if isdead
                                        (begin
                                          (display "FSM ")
                                          (display (fsm-id x))
                                          (display " REPORTS DEAD!")
                                          (newline)))
                                    isdead)) eq?)))
      (death-map 'foldl (lambda (x y)
                          (or x y)) #f)))
  
  (define (send-to-all msg . args)
    (fsms 'map (lambda (x)
                 (apply (fsm-obj x) msg args)) eq?))
  
  (define (tama-fsm-bundler-object msg . args)
    (let ((my-param (make-param args 'tama-fsm-bundler-object)))
      (case msg
        ('transition (transition-all))
        ('one-dead? (one-dead?))
        ('send-to-all (apply send-to-all args))
        ('send-to (let ((r (apply fsm-external-procedure-call args)))
                    (display "sending ")
                    (display (car args))
                    (display " ")
                    (display (cadr args))
                    (display ", returns: ")
                    (display r)
                    (newline)
                    r))
        (else (error 'tama-fsm-bundler-object "message \"~S\" unknown" msg)))))
  
  (add-fsms)
  
  tama-fsm-bundler-object)


(define (tamagotchi-manager)
  (define fsm-bundle (tama-fsm-bundler))
  (define input (input-mapper))
  (define my-lcd (lcd fsm-bundle))
  (define my-buzz (buzzer GPIO_0 11))
  
  (define (init-input)
    (let ((b0-press (input-digital GPIO_0 7))
          (b1-press (input-digital GPIO_0 8))
          (b2-press (input-digital GPIO_0 9))
          (b3-press (input-digital GPIO_0 10))
          (temp-hot (input-analog 'ain0 (lambda (val)
                                      (> val 75))))
          (ldr-dark (input-analog 'ain1 (lambda (val)
                                       (> val 70)))))
      (input 'map b0-press (lambda ()
                             (fsm-bundle 'send-to 'hunger 'feed)))
      (input 'map b1-press (lambda ()
                             (fsm-bundle 'send-to 'mood 'play-a-game)))
      (input 'map b2-press (lambda ()
                             (fsm-bundle 'send-to 'docility 'punish)))
      (input 'map b3-press (lambda ()
                             (fsm-bundle 'send-to 'waste 'clean)))
      (input 'map temp-hot (lambda ()
                             (fsm-bundle 'send-to 'health 'cure)))
      (input 'map ldr-dark (lambda ()
                             (fsm-bundle 'send-to 'sleep 'put-in-bed)))))

;;==========================================================
  (define (beep-if-necessary)
    (if (or (fsm-bundle 'send-to 'hunger 'hungry?)
            (fsm-bundle 'send-to 'mood 'unhappy?)
            (fsm-bundle 'send-to 'waste 'disgusting?)
            (fsm-bundle 'send-to 'health 'sick?)
            (fsm-bundle 'send-to 'sleep 'tired?)
            (and (fsm-bundle 'send-to 'docility 'rebels?)
                 (> (rand 'get 0 100) 85)))
        (my-buzz 'beep 1000000)))
;;==========================================================
  (define (transition-all-fsms)
    (fsm-bundle 'transition))
  
  (define (iptloop)
    (define (innerloop)
      (input 'check)
      (if (< (timer.value TIMER1) 2)
          (innerloop)))
    (timer.set-PR TIMER1 59999999)
    (timer.reset-n-start TIMER1)
    (innerloop)
    (timer.stop TIMER1))
    
  
  (define (mainloop)
    (if (fsm-bundle 'one-dead?)
        (begin
          (display "DEAD")
          (newline))
        (begin
          (display "TRANSITIONING")
          (newline)
          (iptloop)
          (transition-all-fsms)
          (my-lcd 'update)
          (beep-if-necessary)
          (mainloop))))
  (init-input)
  (output-pin (pin 19))
  (set-pin (pin 19))
  (mainloop)
  )

) of)

(close-output-port of)
