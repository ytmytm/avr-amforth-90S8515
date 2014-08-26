; register assignements for multiplication storage
	.def rm1 = r13
	.def rm2 = r14
	.def rmh = r15
	.def rel = r0
	.def reh = r1

; (n1 n2 -- n3)
; R( -- )
VE_MUL:
    .db $01, "*"
    .dw VE_HEAD
    .set VE_HEAD = VE_MUL
XT_MUL:
    .dw PFA_MUL
PFA_MUL:
    ld temp3, Y+
    ld temp2, Y+
    ld temp1, Y+
    ld temp0, Y+
    ; result: (temp3*temp1)* 65536 + (temp3*temp0 + temp1*temp2) * 256 + (temp0 * temp2)
    ; low bytes
;    mul temp0,temp2
	mov rm1, temp0
	mov rm2, temp2
	rcall multiply8x8
;    movw zl, r0
	mov zl, r0
	mov zh, r1
    ; hi bytes
;    mul temp2,temp1
	mov rm1, temp2
	mov rm2, temp1
	rcall multiply8x8
    add zh, r0

;    mul temp3, temp0
	mov rm1, temp3
	mov rm2, temp0
	rcall multiply8x8
    add zh, r0

    ; put low cell on stack
    st -Y, zl
    st -Y, zh
    rjmp DO_NEXT

multiply8x8:
	; multiply r13*r15 -> r1:r0

	clr rmh		; clear storage
	clr rel		; clear result lo
	clr reh		; clear result hi

mult8a:
	clc
	ror rm2
	brcc mult8b
	add rel, rm1
	adc reh, rmh
mult8b:
	clc
	rol rm1
	rol rmh
	tst rm2
	brne mult8a

	ret



