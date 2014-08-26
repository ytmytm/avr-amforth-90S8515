; ( -- addr)
; R( -- )
VE_SP0:
    .db $03, "sp0"
    .dw VE_HEAD
    .set VE_HEAD = VE_SP0
XT_SP0:
    .dw DO_COLON
PFA_SP0:
    .dw XT_DOSP0
    .dw XT_FETCH
    .dw XT_EXIT

XT_DOSP0:
    .dw PFA_DOUSER
PFA_DOSP0:
    .dw 6

; ( -- addr)
; R( -- )
VE_SP:
    .db $02, "sp",0
    .dw VE_HEAD
    .set VE_HEAD = VE_SP
XT_DOSP:
    .dw PFA_DOUSER
PFA_DOSP:
    .dw 8

; ( addr -- ) stackpointer changed to addr
; R( -- )
VE_SP_STORE:
    .db $03, "sp!"
    .dw VE_HEAD
    .set VE_HEAD = VE_SP_STORE
XT_SP_STORE:
    .dw PFA_SP_STORE
PFA_SP_STORE:
    ld temp1, Y+
    ld temp0, Y+
    in temp2, SREG
    cli
    mov yl, temp0
    mov yh, temp1
    out SREG, temp2
    rjmp DO_NEXT 

; ( -- addr)
; R( -- )
VE_RP0:
    .db $03, "rp0"
    .dw VE_HEAD
    .set VE_HEAD = VE_RP0
XT_RP0:
    .dw DO_COLON
PFA_RP0:
    .dw XT_DORP0
    .dw XT_FETCH
    .dw XT_EXIT

XT_DORP0:
    .dw PFA_DOUSER
PFA_DORP0:
    .dw 2

; ( -- addr)
; R( -- )
VE_RP:
    .db $02, "rp",0
    .dw VE_HEAD
    .set VE_HEAD = VE_RP
XT_RP:
    .dw PFA_DOUSER
PFA_RP:
    .dw 4     

; ( n  -- )
; R( -- xy) 
VE_RP_STORE:
    .db $03, "rp!"
    .dw VE_HEAD
    .set VE_HEAD = VE_RP_STORE
XT_RP_STORE:
    .dw PFA_RP_STORE
PFA_RP_STORE:
    ld temp1, Y+
    ld temp0, Y+
    in temp2, SREG
    cli
    out SPL, temp0
    out SPH, temp1
    out SREG, temp2
    rjmp DO_NEXT 

