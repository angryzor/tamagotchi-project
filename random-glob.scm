(load "settings.scm")
(if RUN_ON_ARM
    (load "timer-random.scm")
    (load "sim-random.scm"))

(define rand (random-generator TIMER0))
