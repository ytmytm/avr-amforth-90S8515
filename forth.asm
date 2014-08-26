; Settings for the eval board with AT90S8515 & 8 MHz
;
; TODO:
; - wstawiæ XT_NOOP do RAM i sprawdziæ dzia³anie interpretera
;	- uwaga na kolejnoœæ bajtów!
;	- uwaga na adresy! we flash*2, w RAM nie! (gdzie¶ dzieliæ? przy zapisie? wcale?)
; - ustawiæ koniec heap na pocz±tek ram (>1k), ale ustawienia jak dla 8515 bez extram
; - implementacja istore; mo¿e niech flaga kontroluje gdzie to ma byæ zapisywane
;
;;
; port 8515
; - makra cbi/sbi/in/out zast±pione rozkazami
; - inne definicje portów dla UART, napisane po swojemu
; - brak movw, zast±pione przez mov+mov; brak jmp zast±piony przez rjmp
; - wczytywanie tylko przez 'lpm', przepisane rêcznie przez r0
; - procedura mno¿enia (XT_MUL, star.asm)
;    - u¿ywa rejestrów nie wymienionych tutaj!
; nowo¶ci
; - s³owo idle, które usypia cpu

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
.nolist
;.include "m8def.inc"
.include "8515def.inc"
.list

; cpu clock in hertz
.equ cpu_frequency = 8000000
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
	cbi ACSR, ACIE		; clear irq enable from analog comparator
	sbi ACSR, ACD		; disable analog comparator to reduce power consumption in idle mode 
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
        ldi yl,low(stackstart)
    	ldi yh,high(stackstart)

    ; the following is a turnkey-action, and a few more words for the dictionary
    rcall usart0_init
    rcall device_init
    ; enable interrupts (needed for getting (terminal) input)
    sei
    ; its a far jump...
    rjmp DO_NEXT

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

    ldi temp0, LOW( baudrate )
    out UBRR, temp0
	sbi UCR, TXEN
	sbi UCR, RXEN
	sbi UCR, RXCIE
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
	cbi UCR, UDRIE	; clear interrupts

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
    out UDR, xl

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
    in xl, UDR
  rjmp usart0_rxc_done
usart0_rxc_next:
  ldi zl,low(usart0_rx_data)
  ldi zh,high(usart0_rx_data)
  add zl,xl
  adc zh,zeroh
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

	sbi UCR, UDRIE	; enable interrupts
    rjmp DO_NEXT

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
    rjmp DO_NEXT

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
    rjmp DO_NEXT

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
    rjmp DO_NEXT


; lower part of the dictionary
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;.include "dict_low.asm"
.include "words_low.asm"
	.include "words/idle.asm"
.include "nwords/dobranch.asm"
.include "nwords/docondbranch.asm"	; doesn't belong do lpm cat, but must be near dobranch
.include "nwords/dovariable.asm"
.include "nwords/douser.asm"
.include "nwords/ifetch.asm"
.include "nwords/doliteral.asm"

.include "words/cold.asm"
.include "words/turnkey.asm"
.include "words/quit.asm"

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; set label to latest used cell in cseg
;VE_LATEST:
.set lowflashlast = pc
;	ret	; probably required!

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
	rcall READ_WORD
	mov wl, temp0
	mov wh, temp1
;    lsl zl
;    rol zh
;    lpm wl, Z+
;    lpm
;    mov wl, r0
;    adiw zl, 1
;;    lpm wh, Z      ; done read IP
;    lpm
;    mov wh, r0
    adiw xl, 1        ; INC IP

DO_EXECUTE: ; 12 cpu cycles to ijmp
;    movw zl, wl
    mov zl, wl
    mov zh, wh
	rcall READ_WORD
;    lsl zl
;    rol zh
;;    lpm temp0, Z+
;    lpm
;    mov temp0, r0
;    adiw zl, 1
;;    lpm temp1, Z
;    lpm
;    mov temp1, r0
;;    movw zl, temp0
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


; this costs 7 cycles on each entry/leave, could be saved by placing it in macro
; read a word from address zl/zh into temp0/temp1
; bit15 set - read from dataspace, otherwise - programspace
READ_WORD:
	lsl zl		; address := address*2
	rol zh
	; bit 7 of zh is in C now...
	brcs READ_RAM	; branch to RAM read if set (less often than base words - cycle saved)
	; read from FLASH
	lpm
	mov temp0, r0
	adiw zl, 1
	lpm
	mov temp1, r0
;READ_CONT:
	ret		; quit
READ_RAM:
	ld temp0, Z+	; read word from RAM into temp0/temp1
	ld temp1, Z
	ret
;	rjmp READ_CONT	; continue

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;.include "dict_high.asm"
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
.set flashlast = pc


.eseg
;; flash addresses
;dp:  .dw VE_LATEST
;head:.dw VE_HEAD
;; ram free memory (well, stack matters)
;rheap:
;    .dw heap
;; eeprom free memory
;    .dw eheap
;; turnkey address
;    .dw XT_VER
	.dw lowflashlast	; DP
	.dw VE_HEAD		; HEAD
	.dw heap		; HEAP
	.dw edp			; EDP
	.dw XT_VER		; 'TURNKEY
; 1st free address in EEPROM, see above
;eheap:
edp:
.cseg


