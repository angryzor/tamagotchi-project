; settings.scm
; Desc: Non-runtime settings
;*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*

(define RUN_ON_ARM #f)

; FSM Settings
;***************

; Sleep FSM
;------------

; Time for the creature to be asleep.
; NOTE: Change to wake up when light falls in
(define SLEEP_TIME_SECONDS 30)



; Control settings
;*******************

; Debug option: Check for doubles in the input mappings
(define DEBUG_MAPPING_CHECK_FOR_DOUBLES #f)
