; ( -- )
; R( -- )
;VE_DOBRANCH:
;    .db $08, "(branch)",0
;    .dw VE_HEAD
;    .set VE_HEAD = VE_DOBRANCH
XT_DOBRANCH:
    .dw PFA_DOBRANCH
PFA_DOBRANCH:
;    movw zl, xl
	mov zl, xl
	mov zh, xh
	rcall READ_WORD
	mov xl, temp0
	mov xh, temp1
;    lsl zl
;    rol zh
;;    lpm xl, Z+
;	lpm
;	mov xl, r0
;	adiw zl, 1
;;    lpm xh, Z
;	lpm
;	mov xh, r0
    rjmp DO_NEXT
