; ( addr -- n1 )
; R( -- )
; reads a cell from flash, addr is cell address, not byte addres
; first byte gets into the lower word on tos
VE_IFETCH:
    .db $02, "i@", 0
    .dw VE_HEAD
    .set VE_HEAD = VE_IFETCH
XT_IFETCH:
    .dw PFA_IFETCH
PFA_IFETCH:
    ld zh, Y+
    ld zl, Y+
	rcall READ_WORD
;    lsl zl
;    rol zh
;;    lpm temp0, z+
;	lpm
;;	mov temp0, r0
;	st -Y, r0
;	adiw zl, 1
;;    lpm temp1, z
;	lpm
;	st -Y, r0
;;	mov temp1, r0
   st -Y, temp0
   st -Y, temp1
    rjmp DO_NEXT
