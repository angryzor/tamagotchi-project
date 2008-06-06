(load "settings.scm")

(if RUN_ON_ARM
    (load "lcd-drawer.scm")
    (load "lcd-sim-drawer.scm"))
