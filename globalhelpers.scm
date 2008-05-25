; globalhelpers.scm
; Desc: some global functions that may prove useful.
;*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*

(define (clamp x low high)
  (cond ((< x low) low)
        ((> x high) high)
        (else x)))

(define (safe-optionalbool bl)
  (and (not (null? bl))
       bl))
