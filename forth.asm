; Settings for the eval board with AT90S8515 & 4 MHz

;
; TODO:
; - join core words into single source file (vmlab limitation)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
.nolist
;.include "m8def.inc"
.include "8515def.inc"
.list

; cpu clock in hertz
.equ cpu_frequency = 4000000
; baud rate of terminal
.equ baud_rate = 9600

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; macros (here due to aesthetics)
  .def zerol = r2
  .def zeroh = r3
  .def upl = r4
  .def uph = r5

  .def temp0 = r16
  .def temp1 = r17
  .def temp2 = r18
  .def temp3 = r19
  .def temp4 = r20
  .def temp5 = r21
  .def temp6 = r22
  .def temp7 = r23

  .def wl = r24
  .def wh = r25

  .set heap = ramstart
  .set VE_HEAD = $0000

.macro jmp_
    rjmp @0
.endmacro

.macro in_
;.if (@1 < $40)
  in @0,@1
;.else
;  lds @0,@1
;.endif
.endmacro

.macro out_
;.if (@0 < $40)
  out @0,@1
;.else
;  sts @0,@1
;.endif
.endmacro

.macro sbi_
;.if (@0 < $40)
  sbi @0,@1
;.else
;  in_ @2,@0
;  ori @2,exp2(@1)
;  out_ @0,@2
;.endif
.endmacro

.macro cbi_
;.if (@0 < $40)
  cbi @0,@1
;.else
 ; in_ @2,@0
 ; andi @2,~(exp2(@1))
 ; out_ @0,@2
;.endif
.endmacro

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;.include "devices\atmega8.asm"

  ; first address of RAM
  .equ ramstart = $60
  .equ stackstart = RAMEND - 80
  .equ HLDSIZE  = $10 ; 16 bit cellsize with binary representation
  .equ TIBSIZE  = $64 ; 80 characters is one line...
  .equ CELLSIZE = 2   ;
  .equ USERSIZE = 32  ; size of user area

 ; .equ PAGEMASK =  ~ ( PAGESIZE - 1 )

;  .equ nrww = $0c00
;  .equ codestart = $14

;  .equ UBRR0L = UBRRL
;  .equ UBRR0H = UBRRH
;.equ UBRR0L = UBRR
;.equ UBRR0H = UBRR
;  .equ UCSR0C = UCSRC
;  .equ UCSR0B = UCSRB
.equ UCSR0C = UCR
.equ UCSR0B = USR
  .equ UDR0   = UDR

;  .equ TXEN0  = TXEN
;  .equ RXEN0  = RXEN
;  .equ RXCIE0 = RXCIE
;  .equ UMSEL01 = URSEL
;.equ UMSEL01 = 0
;  .equ UCSZ00  = UCSZ0
;.equ UCSZ00 = 0
;  .equ UDRIE0  = UDRIE

.org $0000
  rjmp reset
.org	INT0addr ; External Interrupt0 Vector Address
    rjmp int0_isr
.org	INT1addr ; External Interrupt1 Vector Address
    rjmp int1_isr
.org	ICP1addr
    reti	; Input Capture1 Interrupt Vector Address
.org	OC1Aaddr
    reti	; Output Compare1A Interrupt Vector Address
.org	OC1Baddr
    reti	; Output Compare1B Interrupt Vector Address
.org	OVF1addr
    reti	; Overflow1 Interrupt Vector Address
.org	OVF0addr
    reti	; Overflow0 Interrupt Vector Address
.org	SPIaddr
    reti	; SPI Interrupt Vector Address
.org	URXCaddr
	rjmp usart0_rx_isr
;    reti	; USART Receive Complete Interrupt Vector Address
.org	UDREaddr
    rjmp usart0_udre_isr
;    reti	; USART Data Register Empty Interrupt Vector Address
.org	UTXCaddr
    reti	; USART Transmit Complete Interrupt Vector Address
.org	ACIaddr
    reti	; Analog Comparator Interrupt Vector Address
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

  .set heap = ramstart
  .set VE_HEAD = $0000

device_init:
    ; just a dummy
	in_ temp0, ACSR
	cbr temp0, (1<<ACIE)	; clear irq enable
	sbr temp0, (1<<ACD)	; disable Analog Comparator to reduce
	out_ ACSR, temp0	; power consumption in sleep mode
    ret

;.include "amforth.asm"

.set pc_ = pc
;.include "macros.asm"

.org pc_
; main entry point
reset:
    clr zerol
    clr zeroh
    ; init first user data area
    ldi zl, low(heap)
    ldi zh, high(heap)
    ;movw upl, zl
    mov upl, zl
    mov uph, zh
    ; init return stack pointer
    ldi temp0,low(ramend)
    out SPL,temp0
    std Z+2, temp0
    ldi temp1,high(ramend)
    out SPH,temp1
    std Z+3, temp1

    ; init parameter stack pointer
    ldi yl,low(stackstart)
    std Z+6, yl
    ldi yh,high(stackstart)
    std Z+7, yh

    ; set IO
    ldi yl, low(xt_tx0)
    std Z+12, yl
    ldi yh, high(xt_tx0)
    std Z+13, yh

    ldi yl, low(xt_tx0q)
    std Z+14, yl
    ldi yh, high(xt_tx0q)
    std Z+15, yh

    ldi yl, low(xt_rx0)
    std Z+16, yl
    ldi yh, high(xt_rx0)
    std Z+17, yh

    ldi yl, low(xt_rx0q)
    std Z+18, yl
    ldi yh, high(xt_rx0q)
    std Z+19, yh

    ldi yl, low(xt_noop)
    std Z+20, yl
    ldi yh, high(xt_noop)
    std Z+21, yh

    ; keep free space for User Area
    .set heap = heap + USERSIZE * CELLSIZE

    ; load Forth IP with starting word
    ldi xl, low(PFA_COLD)
    ldi xh, high(PFA_COLD)
;    ldi xl, low(PFA_TESTME)
;    ldi xh, high(PFA_TESTME)
        ldi yl,low(stackstart)
    	ldi yh,high(stackstart)

    ; the following is a turnkey-action, and a few more words for the dictionary
    rcall usart0_init
    rcall device_init
    ; enable interrupts (needed for getting (terminal) input)
    sei
    ; its a far jump...
    jmp_ DO_NEXT

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; ISR routines
;.include "intx.asm"

.equ INTVECTORS = 4 ; only a few interrupts for now

.set intcur   = heap ; current interrupt
.set heap     = heap + 1
.set intcount = heap ; interrupt counter, incremented for every int
.set heap     = heap + INTVECTORS
.set intvec   = heap ; forth interrupt vector (contains the XT)
.set heap     = heap + INTVECTORS * CELLSIZE

; map avr interrupts to amforth interrupts
int0_isr:
    push yl
    ldi yl, 0
    rjmp intx_isr

int1_isr:
    push yl
    ldi yl, 1
    rjmp intx_isr

int2_isr:
    push yl
    ldi yl, 2
    rjmp intx_isr

int3_isr:
    push yl
    ldi yl, 3
    rjmp intx_isr

intx_isr:
    sts intcur, yl
    push yh
    in yh,SREG
    push yh
    push zh
    push zl
    ldi zl, low(intcount)
    ldi zh, high(intcount)
    add zl, yl
    adc zh, zeroh
    ldd yl, Z+0
    inc yl
    std Z+0, yl
    pop zl
    pop zh
    pop yh
    out SREG,yh
    pop yh
    pop yl
    set ; set the interrupt flag for the inner interpreter
    reti

VE_INTCOUNTER:
    .db $0a, "intcounter",0
    .dw VE_HEAD
    .set VE_HEAD = VE_INTCOUNTER
XT_INTCOUNTER:
    .dw DO_COLON
PFA_INTCOUNTER:
    .dw XT_DOLITERAL
    .dw intcount
    .dw XT_PLUS
    .dw XT_EXIT

VE_INTVECTOR:
    .db $09, "intvector"
    .dw VE_HEAD
    .set VE_HEAD = VE_INTVECTOR
XT_INTVECTOR:
    .dw DO_COLON
PFA_INTVECTOR:
    .dw XT_2STAR
    .dw XT_DOLITERAL
    .dw intvec
    .dw XT_PLUS
    .dw XT_EXIT

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
XT_STUCK:
	.dw PFA_XT_STUCK
PFA_XT_STUCK:
	ldi xl, low(PFA_COLD)
	ldi xh, high(PFA_COLD)
	rjmp DO_NEXT

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;.include "usart.asm"

.set pc_ = pc

.org pc_

.equ baudrate  = cpu_frequency/(baud_rate*16)-1
; sizes have to be powers of 2!
.equ usart0_tx_size = $10
.equ usart0_rx_size = $10

.equ usart0_tx_mask = usart0_tx_size - 1
.equ usart0_rx_mask = usart0_rx_size - 1

.set usart0_tx_in = heap
.set heap = heap + 1

.set usart0_tx_out = heap
.set heap = heap + 1

.set usart0_tx_data = heap
.set heap = heap + usart0_tx_size

.set usart0_rx_in = heap
.set heap = heap + 1

.set usart0_rx_out = heap
.set heap = heap + 1

.set usart0_rx_data = heap
.set heap = heap + usart0_rx_size

usart0_init:
  sts usart0_tx_in,zerol
  sts usart0_tx_out,zerol
  sts usart0_rx_in,zerol
  sts usart0_rx_out,zerol

;  ldi temp0, LOW( baudrate )
;  out_ UBRR0L, temp0
;  ldi temp0, HIGH( baudrate )
;  out_ UBRR0H, temp0
;  ldi temp0, (1<<UMSEL01)|(3<<UCSZ00)
;  out_ UCSR0C, temp0
;  sbi_ UCSR0B, TXEN0, temp0
;  sbi_ UCSR0B, RXEN0, temp0
;  sbi_ UCSR0B, RXCIE0, temp0
    ldi temp0, LOW( baudrate )
    out_ UBRR, temp0
    sbi_ UCR, TXEN, temp0
    sbi_ UCR, RXEN, temp0
    sbi_ UCR, RXCIE, temp0
  ret

usart0_udre_isr:
  push xl
  in xl,SREG
  push xl
  push xh
  push zl
  push zh

  lds xl,usart0_tx_in
  lds xh,usart0_tx_out

  cp xh,xl
  brne usart0_udre_next

usart0_udre_last:
;  in_ xl,UCSR0B
;  cbr xl,(1<<UDRIE0)
;  out_ UCSR0B,xl
    in_ xl, UCR
    cbr xl, (1<<UDRIE)	; clear interrupts
    out_ UCR, xl

  rjmp usart0_udre_done

usart0_udre_next:
  inc xh
  andi xh,usart0_tx_mask
  sts usart0_tx_out,xh

  ldi zl,low(usart0_tx_data)
  ldi zh,high(usart0_tx_data)
  add zl,xh
  adc zh,zeroh

  ld xl,z
;  out_ UDR0,xl
    out_ UDR, xl

usart0_udre_done:
  pop zh
  pop zl
  pop xh
  pop xl
  out SREG,xl
  pop xl
  reti
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
usart0_rx_isr:
  push xl
  in xl,SREG
  push xl
  push xh
  push zl
  push zh

  lds xl,usart0_rx_in
  lds xh,usart0_rx_out

  inc xl
  andi xl,usart0_rx_mask
  cp xh,xl
  brne usart0_rxc_next

usart0_rxc_full:
;  in_ xl,UDR0
    in_ xl, UDR
  rjmp usart0_rxc_done
usart0_rxc_next:
  ldi zl,low(usart0_rx_data)
  ldi zh,high(usart0_rx_data)
  add zl,xl
  adc zh,zeroh
;  in_ xh,UDR0
    in xh, UDR
  st z,xh
  sts usart0_rx_in,xl

usart0_rxc_done:
  pop zh
  pop zl
  pop xh
  pop xl
  out SREG,xl
  pop xl
  reti

; (c -- )
VE_TX0:
    .db $03, "tx0"
    .dw VE_HEAD
    .set VE_HEAD = VE_TX0
XT_TX0:
    .dw PFA_TX0
PFA_TX0:
    lds temp0,usart0_tx_in
    inc temp0
    andi temp0,usart0_tx_mask

    lds temp1,usart0_tx_out
    cp temp0,temp1
    brne PFA_tx0_store
    rjmp PFA_tx0

PFA_tx0_store:
    sts usart0_tx_in,temp0
    ldi zl,low(usart0_tx_data)
    ldi zh,high(usart0_tx_data)
    add zl, temp0
    adc zh, zeroh
    ld temp1, Y+
    ld temp0, Y+
    st z,temp0

;    in_ temp0,UCSR0B
;    sbr temp0,(1<<UDRIE0)
;    out_ UCSR0B,temp0
    in_ temp0, UCR
    sbr temp0, (1<<UDRIE)
    out_ UCR, temp0
    jmp_ DO_NEXT

; ( -- f) always true
VE_TX0Q:
    .db $04, "tx0?",0
    .dw VE_HEAD
    .set VE_HEAD = VE_TX0Q
XT_TX0Q:
    .dw PFA_TX0Q
PFA_TX0Q:
;    movw_ zl, zerol
    mov zl, zerol
    mov zh, zeroh
    sbiw zl, 1
    st -Y, zl
    st -Y, zh
    jmp_ DO_NEXT

; ( -- c)
VE_RX0:
    .db $03, "rx0"
    .dw VE_HEAD
    .set VE_HEAD = VE_RX0
XT_RX0:
    .dw PFA_RX0
PFA_RX0:
    lds temp1,usart0_rx_out
    lds temp0,usart0_rx_in
    cp temp1, temp0
    brne PFA_rx0_fetch
    rjmp PFA_rx0

PFA_rx0_fetch:
    inc temp1
    andi temp1,usart0_rx_mask
    sts usart0_rx_out, temp1

    ldi zl,low(usart0_rx_data)
    ldi zh,high(usart0_rx_data)
    add zl, temp1
    adc zh, zeroh
    ld temp0, Z	
    st -Y, temp0
    st -Y, zeroh
    jmp_ DO_NEXT

; ( -- f)
VE_RX0Q:
    .db $04, "rx0?",0
    .dw VE_HEAD
    .set VE_HEAD = VE_RX0Q
XT_RX0Q:
    .dw PFA_RX0Q
PFA_RX0Q:
    lds temp0,usart0_rx_out
    lds temp1,usart0_rx_in
;    movw_ zl, zerol
    mov zl, zerol
    mov zh, zeroh
    cpse temp0, temp1
    sbiw zl, 1
    st -Y, zl
    st -Y, zh
    jmp_ DO_NEXT


; lower part of the dictionary
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;.include "dict_low.asm"
.include "words/cold.asm"
;
.include "dict_core.asm"
;;.include "words/sp0.asm"
;;.include "words/spstore.asm"
;;.include "words/rp0.asm"
;;.include "words/rpstore.asm"
.include "words/zero.asm"
.include "words/dovariable.asm"
.include "words/state.asm"
.include "words/store.asm"
.include "words/fetch.asm"
.include "words/douser.asm"
.include "words/r_from.asm"
.include "words/to_r.asm"
.include "words/dup.asm"
.include "words/ifetch.asm"
.include "words/icount.asm"
.include "words/and.asm"
.include "words/itype.asm"
.include "words/dodo.asm"
.include "words/swap.asm"
.include "words/over.asm"
.include "words/equal.asm"
.include "words/equalzero.asm"
.include "words/greaterzero.asm"
.include "words/drop.asm"
.include "words/r_fetch.asm"
.include "words/rshift.asm"
.include "words/emit.asm"
.include "words/qexecute.asm"
.include "words/qdup.asm"
.include "words/execute.asm"
.include "words/dobranch.asm"
.include "words/docondbranch.asm"
.include "words/doloop.asm"
;
.include "words/accept.asm"
.include "words/pause.asm"
	.include "words/idle.asm"
.include "words/key.asm"
.include "words/keyq.asm"
.include "words/notequal.asm"
.include "words/bl.asm"
.include "words/space.asm"
.include "words/less.asm"
.include "words/cstore.asm"
.include "words/minus.asm"
.include "words/cr.asm"
.include "words/slashkey.asm"
.include "words/tib.asm"
; pierwociny do interpret...
;.include "words/cfetch.asm"
;.include "words/sharptib.asm"
;.include "words/word.asm"
.include "words/notequalzero.asm"
;.include "words/g_in.asm"
;
.include "words/head.asm"
.include "words/heap.asm"
;.include "words/pad.asm"
.include "words/efetch.asm"
.include "words/words.asm"
;
.include "words/exit.asm"
.include "words/noop.asm"
.include "words/plus.asm"
.include "words/1plus.asm"
.include "words/1minus.asm"
.include "words/doliteral.asm"
.include "words/2star.asm"
.include "words/2slash.asm"
.include "words/ver.asm"
.include "words/dodotstring.asm"

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; set label to latest used cell in cseg
VE_LATEST:
	ret	; probably required!

; high part of the dictionary (primitives and words for self programming)
;.org nrww
;.include "forth.asm"
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;
; the inner interpreter.
DO_DODOES:
    adiw wl, 1
    st -Y, wl
    st -Y, wh

    pop wh
    pop wl

    push xl
    push xh
;    movw xl, wl
    mov xl, wl
    mov xh, wh
    rjmp DO_NEXT

DO_COLON: ; 31 CPU cycles to ijmp
    push xl
    push xh          ; PUSH IP
    adiw wl, 1       ; set W to PFA
;    movw xl, wl
    mov xl, wl
    mov xh, wh

DO_NEXT: ; 24 CPU cycles to ijmp
    brts DO_INTERRUPT
;    movw zl,xl        ; READ IP
    mov zl, xl
    mov zh, xh
    lsl zl
    rol zh
;    lpm wl, Z+
    lpm
    mov wl, r0
    inc zl
;    lpm wh, Z      ; done read IP
    lpm
    mov wh, r0
    adiw xl, 1        ; INC IP

DO_EXECUTE: ; 12 cpu cycles to ijmp
;    movw zl, wl
    mov zl, wl
    mov zh, wh
    lsl zl
    rol zh
;    lpm temp0, Z+
    lpm
    mov temp0, r0
    inc zl
;    lpm temp1, Z
    lpm
    mov temp1, r0
;    movw zl, temp0
    mov zl, temp0
    mov zh, temp1
    ijmp

DO_INTERRUPT: ; 12 cpu cycles to rjmp (+12=24 to ijmp)
    ; here we deal with interrupts the forth way
    lds temp0, intcur
    ldi zl, LOW(intvec)
    ldi zh, HIGH(intvec)
    lsl temp0
    add zl, temp0
    adc zh, zeroh
    ldd wl, Z+0
    ldd wh, Z+1

    clt ; clear the t flag to indicate that the interrupt is handled
    rjmp DO_EXECUTE

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;.include "dict_high.asm"
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


.eseg
; flash addresses
dp:  .dw VE_LATEST
head:.dw VE_HEAD
; ram free memory (well, stack matters)
rheap:
    .dw heap
; eeprom free memory
    .dw eheap
; turnkey address
    .dw XT_VER
; 1st free address in EEPROM, see above
eheap:
;.cseg




