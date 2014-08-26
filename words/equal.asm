; ( n1 n2 -- flag )
; R( -- )
VE_EQUAL:
    .db $01, "="
    .dw VE_HEAD
    .set VE_HEAD = VE_EQUAL
XT_EQUAL:
    .dw PFA_EQUAL
PFA_EQUAL:
    ld temp1, Y+
    ld temp0, Y+
    ld temp3, Y+
    ld temp2, Y+
    cp temp0, temp2
    cpc temp1, temp3
;    movw zl, zerol
	mov zl, zerol
	mov zh, zeroh
    brne PFA_EQUAL1
    sbiw zl, 1
PFA_EQUAL1:
    st -Y, zl
    st -Y, zh
    rjmp DO_NEXT
