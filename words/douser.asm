; ( -- addr )
; R( -- )
;VE_DOUSER:
;    .db $06, "(user)", 0
;    .dw VE_HEAD
;    .set VE_HEAD = VE_DOUSER
XT_DOUSER:
    .dw PFA_DOUSER
PFA_DOUSER:
;    movw zl, wl
	mov zl, wl
	mov zh, wh
    adiw zl, 1
    lsl zl
    rol zh
;    lpm temp0, Z+
	lpm
	mov temp0, r0
	adiw zl, 1
;    lpm temp1, Z
	lpm
	mov temp1, r0
    add temp0, upl
    adc temp1, uph
    st -Y, temp0
    st -Y, temp1
    rjmp DO_NEXT

