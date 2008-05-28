(load "settings.scm")

(if RUN_ON_ARM
    (load "lcd.scm")
    (load "lcdsim.scm"))
