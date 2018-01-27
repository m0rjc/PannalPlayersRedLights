
;**********************************************************************
; PIC16F628A use internal 4 mHz clock                                                     
;**********************************************************************

	title "Pannal Players Red-Eyes Effect"
	list      p=16f628A           ; list directive to define processor
	#include <p16F628A.inc>       ; processor specific variable definitions
	;errorlevel  -302              ; suppress message 302 from list file

	__CONFIG   _CP_OFF & _LVP_OFF & _BOREN_OFF & _MCLRE_OFF & _WDT_OFF & _PWRTE_ON & _INTOSC_OSC_NOCLKOUT 

	; Random seeds. Two non zero values
	#define RANDOM_SEED_1 b'10010101'
	#define RANDOM_SEED_2 b'01101011'
	
	; Pins on PORTA for status display. these are all CMOS
	#define PORTA_STATUS_LEDS b'10000011'
	#define LED_RX	  1
	#define LED_RUN   0
	#define LED_LATCH 7
	
;------------------------------------------------------------------------------
; BusyLoop macro. Uses COUNT3 as loop variable.
; Takes 2+2n operations
BUSYLOOP MACRO iterations
    MOVLW iterations
    MOVWF COUNT3
    DECFSZ COUNT3, F
    GOTO $-1
    ENDM
	
; for 16F628A only	
; Use  _INTOSC_OSC_NOCLKOUT  for 
; internal 4 mHz osc and no ext reset, use pin RA5 as an input
; Use _HS_OSC for a 16 mHz ext crystal. 
; Use _XT_OSC for 4 mHz ext crystal. Page 95 in spec sheet. 

	UDATA_SHR	; Begin General Purpose-Register
COUNT1	    RES 1
COUNT2	    RES 1
COUNT3	    RES 1
PRNG1	    RES 1		; Random number generator shift register 1
PRNG2	    RES 1		; Random number generator shift register 2
COMMAND	    RES 1		; Desired output state of the LEDs
FADE_MASK   RES 1		; Mask to use when fading

	ORG     0x000   ; processor reset vector
;	goto    setup   ; go to beginning of program

setup ; init PIC16F628A

	movlw	0x07  ; Turn comparators off
	movwf	CMCON
	banksel TRISB    ; BSF	STATUS,RP0 Jump to bank 1 use BANKSEL instead
	clrf    TRISB
	MOVLW PORTA_STATUS_LEDS;
	XORWF	TRISA, F    ; Set the status LED pins high impedance. 
	banksel INTCON ; back to bank 0
	clrf	PORTB

	; setp TMR0 interrupts
	banksel OPTION_REG 
	movlw b'10000111' 
	; internal clock, pos edge, prescale 256
	movwf OPTION_REG
	banksel INTCON ; bank 0

initRandom
	banksel PRNG1
	movlw RANDOM_SEED_1
	movwf PRNG1
	movlw RANDOM_SEED_2
	movwf PRNG2
	clrf COMMAND

	;goto main


main
LOC1
	; Shift random number generator 3 times as we want 3 bits
	MOVLW D'3'
	CALL nextRandom
	
	; Use it to select a bit, output in COUNT2
	ANDLW D'7'
	ADDLW D'1'
	MOVWF COUNT1
	CLRF COUNT2
	BSF STATUS, C
bitSelect_shiftLoop
	RLF COUNT2, F
	DECFSZ COUNT1, F
	GOTO bitSelect_shiftLoop
	
	; In run mode, use the selected bit to toggle a channel. Don't allow zero.
	MOVF COMMAND, W
	XORWF COUNT2, W
	BTFSC STATUS, Z
	XORWF COUNT2, W ; Undo last operation if it resulted in a zero
	MOVWF COMMAND

	CALL crossFade
	
	; Delay a random amount. Load W with a time in units of 0.05s
	; If we take 5 bits of random number, multiply by 4.
	; This gives 0 to 124, or up to 6.2s in 0.2s steps
	MOVLW D'5'
	CALL nextRandom
	ANDLW B'00011111'
	MOVWF COUNT1
	RLF COUNT1, F
	RLF COUNT1, W
	CALL TMR0_DEL ; any delay routine
	goto LOC1 

; Timer0 delay routine. W is the amount of 0.05s intervals to time for
TMR0_DEL 
	MOVWF COUNT1
	MOVLW D'59' ; 196 cycles before overflow
	MOVWF TMR0
	BCF INTCON, T0IF ; clear over flow flag bit 2
	BTFSS INTCON, T0IF ; wait for flag set
	GOTO $-1
	DECFSZ COUNT1, F
	GOTO $-6
	RETLW 0
	
;-------------------------------------------------------------------------------
; Read the current command. There are two modes:
;	Local input on switches
;	Failing that AFSK remote input.
;
; AFSK REMOTE:
; Read the input frequency from the controller.
; Use TMR0 in count mode and TMR1 to time 1/10s.	
; If too few counts are detected we indicate no signal and do not change state.
;
;       b'00xxxxxx'  no data	DC to 640Hz
;	b'01xxxxxx'  stop	640Hz to 1280Hz    - Target 960Hz
;	b'10xxxxxx'  run	1280Hz to 1920Hz   - Target 1600Hz
;	b'11xxxxxx'  latch	1920Hz to 2560Hz   - Target 2240Hz
;;	
; The oscillators in the PICs are 4MHz+-5%. So a source signal at 1.6kHz
; may raise to 1.68kHz and our next detection threshold may be as low as 
; 1.82kHz. 
; This gives a large room for error, but ultimately limits how many
; commands can be sent using a single symbol (continuous) AFSK signal.
; The 5% should allow one additional bit (1.34kHz signal into a 1.36kHz threshold)
;-------------------------------------------------------------------------------
	subtitle "AFSK Input Read"
readInput
	; First check to see if external switches  are set giving local control
	
	
	
	
	
;-------------------------------------------------------------------------------
; Perform a PWM transition over approximately 1 second moving PORTB to COMMAND
;
; If we fade over 256 transitions using an 8 bit PWM we need a decision frequency
; of about 65.5kHz. This is an inner loop of about 15 cycles. Tune this to give
; the fade length.
;-------------------------------------------------------------------------------
	subtitle "PWM output transition"
crossFade
	MOVFW COMMAND
	XORWF PORTB, W
	MOVWF FADE_MASK	    ; fadeMask when xored with PORTB will produce COMMAND
	CLRF  COUNT1	    ; Outer loop, increment to 255
crossFade_outerLoop
	MOVFW FADE_MASK
	XORWF PORTB, F	    ; PWM high
	INCF  COUNT1, F	    ; Increment outer counter, finish on overflow
	BTFSC STATUS, Z
	GOTO crossFade_end
	MOVFW COUNT1	    ; count2 = count1
	MOVWF COUNT2
	NOP		    ; NOPs here plus the logic above and below
	NOP		    ; will take the minimum time to PWM low to 15 operations
	NOP
	NOP
	NOP
crossFade_innerLoopHigh
	DECF  COUNT2, F
	BTFSC STATUS, Z
	GOTO crossFade_innerLoopHighEnd
	BUSYLOOP 4	    ; 10 cycles busy loop plus 5 cycles loop logic
	GOTO crossFade_innerLoopHigh
crossFade_innerLoopHighEnd
	MOVFW FADE_MASK
	XORWF PORTB, F	    ; PWM low
	MOVFW COUNT1
	MOVWF COUNT2	    ; count2 = count1 (count1 = {1..255})
	COMF COUNT2, F	    ; count2 ones complement {254..0}
	INCF COUNT2, F	    ; count2 ranges {255..1}
	NOP		    ; NOPs here plus the logic above and below
	NOP		    ; will take the minimum time to PWM high to 15 operations
	NOP
	NOP
	NOP
	NOP
	NOP
crossFade_innerLoopLow
	DECF COUNT2, F
	BTFSC STATUS, Z
	GOTO crossFade_outerLoop
	BUSYLOOP 4
	GOTO crossFade_innerLoopLow
crossFade_end
	RETLW 0
	
;-------------------------------------------------------------------------------
; Calculate a random number. Leave the result in W.
; Input in W is the amount of shifts to perform, the amount of bits
; we require.
;-------------------------------------------------------------------------------
	subtitle "Pseudo random number generator"
nextRandom
	BANKSEL PRNG1
	MOVWF COUNT1
	INCF COUNT1, F
	; Read off the current random number before performing shifts, so that
	; the shifts end up removing now 'dirty' bits. This costs a little more
	; as I don't just end up with the result naturally in W, but is more
	; correct.
	MOVFW PRNG1
	XORWF PRNG2, W
	MOVWF COUNT2 ; tmp
nextRandom_shiftLoop	
	; PRNG1 is 0x69, 7 bits long
	BCF STATUS, C  ; Clear carry
	RRF PRNG1, W   ; Shift PRNG1. Result goes into W
	BTFSC STATUS, C
	XORLW H'69'
	MOVWF PRNG1
	; PRNG2 is 0xA6, 8 bits long
	BCF STATUS, C  ; Clear carry
	RRF PRNG2, W   ; Shift PRNG2. Result goes into W
	BTFSC STATUS, C
	XORLW H'A6'
	MOVWF PRNG2
	DECFSZ COUNT1, F
	GOTO nextRandom_shiftLoop
	; Return result temporarily stored in COUNT2
	MOVFW COUNT2
	RETURN
	
	END  ; directive 'end of program'
	


