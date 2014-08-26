; ( -- )
; R( -- ) enter power saving sleep mode until interrupt comes
VE_IDLE:
    .db $04, "idle", $00
    .dw VE_HEAD
    .set VE_HEAD = VE_IDLE
XT_IDLE:
    .dw PFA_IDLE

PFA_IDLE:	; power saving mode
	in_ temp0, MCUCR
	sbr temp0, (1<<SE)
	cbr temp0, (1<<SM)
	out_ MCUCR, temp0
	sleep
	in_ temp0, MCUCR
	cbr temp0, (1<<SE)
	out_ MCUCR, temp0
	rjmp DO_NEXT


