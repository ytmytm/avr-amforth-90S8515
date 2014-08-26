; ( -- addr )
; R( -- )
;VE_DOVARIABLE:
;    .db $0a, "(variable)", 0
;    .dw VE_HEAD
;    .set VE_HEAD = VE_DOVARIABLE
XT_DOVARIABLE:
    .dw PFA_DOVARIABLE
PFA_DOVARIABLE:
;    movw zl, wl
	mov zl, wl
	mov zh, wh
    adiw zl,1
	rcall READ_WORD
;    lsl zl
;    rol zh
;;    lpm temp0, Z+
;	lpm
;;	mov temp0, r0
;	adiw zl, 1
;	st -Y, r0
    st -Y, temp0
;;    lpm temp1, Z
;	lpm
;;	mov temp1, r0
;	st -Y, r0
    st -Y, temp1
    rjmp DO_NEXT
