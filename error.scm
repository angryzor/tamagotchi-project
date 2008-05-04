(define (my-error symb frmt-str . v)
  (display "error: ")
  (display (apply format (string-append "~s: " frmt-str) symb v)))
