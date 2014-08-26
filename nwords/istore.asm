; ( n addr -- )
; R( -- )
; writes a cell into flash, unfinished
VE_ISTORE:
    .db $02, "i!",0
    .dw VE_HEAD
    .set VE_HEAD = VE_ISTORE
XT_ISTORE:
;    .dw DO_COLON
	.dw PFA_ISTORE
PFA_ISTORE:
    ld zh, Y+
    ld zl, Y+
    lsl zl		; cell address
    rol zh
;    ldi temp0, $00	; offset $1000 (/2)
;    ldi temp1, $08
;    add zl, temp0
;    adc zh, temp1
    ld temp1, Y+
    ld temp0, Y+
    st Z+, temp0
    st Z, temp1
    rjmp DO_NEXT
