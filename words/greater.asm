; ( n1 n2 -- flag )
; R( -- )
VE_GREATER:
    .db $01, ">"
    .dw VE_HEAD
    .set VE_HEAD = VE_GREATER
XT_GREATER:
    .dw PFA_GREATER
PFA_GREATER:
    ld temp1, Y+
    ld temp0, Y+
    ld temp3, Y+
    ld temp2, Y+
    cp temp2, temp0
    cpc temp3, temp1
;    movw zl, zerol
	mov zl, zerol
	mov zh, zeroh
    brlt PFA_GREATER1
    brbs 1, PFA_GREATER1
    sbiw zl, 1
PFA_GREATER1:
    st -Y, zl
    st -Y, zh
    rjmp DO_NEXT
