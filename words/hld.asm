; ( -- addr )
; R( -- )
VE_HLD:
    .db $03, "hld"
    .dw VE_HEAD
    .set VE_HEAD = VE_HLD
XT_HLD:
;backport from 1.7
;    .dw PFA_DOVARIABLE
	.dw DO_COLON
PFA_HLD:
	.dw XT_PAD
	.dw XT_EXIT
;backport from 1.7
;    .dw heap
;    .set heap = heap + HLDSIZE
