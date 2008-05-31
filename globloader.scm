; globloader.scm
; Desc: Loads all global resources.
;*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*

;Uncomment when uploading to LPCH2214
;(load "error.scm")
(load "settings.scm")
(if (not RUN_ON_ARM)
    (load "simulation.scm"))
(load "armscheme-nowrite.scm")
(if (not RUN_ON_ARM)
    (load "simulationtimer.scm"))
(load "objecthlp.scm")
(load "globalhelpers.scm")
(load "random-glob.scm")
