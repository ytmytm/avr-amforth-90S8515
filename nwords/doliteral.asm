; ( -- n1 )
; R( -- )
;VE_DOLITERAL:
;    .db $09, "(literal)"
;    .dw VE_HEAD
;    .set VE_HEAD = VE_DOLITERAL
XT_DOLITERAL:
    .dw PFA_DOLITERAL
PFA_DOLITERAL:
;    movw zl, xl
    mov zl, xl
    mov zh, xh
	rcall READ_WORD
;    lsl zl
;    rol zh
;;    lpm temp0, Z+
;    lpm
;;    mov temp0, r0
;    adiw zl, 1
;    st -Y, r0
    st -Y, temp0
;;    lpm temp1, Z
;    lpm
;;    mov temp1, r0
;    st -Y, r0
    st -Y, temp1
    adiw xl, 1
    rjmp DO_NEXT
