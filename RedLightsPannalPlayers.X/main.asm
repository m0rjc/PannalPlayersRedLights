
;**********************************************************************
; PIC16F628A use internal 4 mHz clock                                                     
;**********************************************************************

	title "Pannal Players Red-Eyes Effect"
	list      p=16f628A           ; list directive to define processor
	#include <p16F628A.inc>       ; processor specific variable definitions

	 __CONFIG _FOSC_INTOSCCLK & _WDTE_OFF & _PWRTE_ON & _MCLRE_OFF & _BOREN_OFF & _LVP_OFF & _CPD_OFF & _CP_OFF

	; Random seeds. Two non zero values
	#define RANDOM_SEED_1 b'10010001'
	#define RANDOM_SEED_2 b'01101011'
		
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
prngRegister1	    RES 1		; Random number generator shift register 1
prngRegister2	    RES 1		; Random number generator shift register 2
outputState	    RES 1		; Desired output state of the LEDs
fadeMask	    RES 1		; Mask to use when fading
runState	    RES 1		; System run state
	    #define RUNSTATE_RUN    0
	    #define RUNSTATE_LATCH  1
	    #define RUNSTATE_RX	    2
	    #define RUNSTATE_TEST   7
timeToNextChange    RES 1		; How many 0.1s reads to wait until next lighting change
lampCount	RES 1	
timerRead   RES 1

	ORG     0x000   ; processor reset vector
;	goto    setup   ; go to beginning of program

setup ; init PIC16F628A

	BANKSEL CMCON
	movlw	0x07  ; Turn comparators off
	movwf	CMCON

	banksel TRISB    ; BSF	STATUS,RP0 Jump to bank 1 use BANKSEL instead
	CLRF TRISB	    ; All outputs
	CLRF TRISA	    ; All outputs
	BSF TRISA, 4	    ; ... except input on 4 for Timer0 clock 
	
	banksel INTCON ; back to bank 0
	clrf	PORTB

	; Timer 1, 1:2 prescaler, internal clock
	MOVLW b'00010001'
	MOVWF T1CON
	
	; Timer 0, 1:1 prescaler, external input on RA4
	CLRWDT ;Clear WDT  (instructions from datasheet for safe setup of prescaler)
	CLRF TMR0 ;Clear TMR0 and Prescaler
	BANKSEL OPTION_REG
	MOVLW b'00101111'
	MOVWF OPTION_REG 
	CLRWDT
	banksel INTCON ; bank 0

initRandom
	banksel prngRegister1
	movlw RANDOM_SEED_1
	movwf prngRegister1
	movlw RANDOM_SEED_2
	movwf prngRegister2
	CALL nextRandom
	clrf outputState
	clrf runState
	clrf timeToNextChange

	;goto main

	BSF runState, RUNSTATE_RUN

main
	CALL readInput
	BANKSEL PORTA
	MOVF runState, W
	MOVWF PORTA
	CALL changeDisplay
	GOTO main
	
;-------------------------------------------------------------------------------
; Change the display according to run mode.
;-------------------------------------------------------------------------------	
changeDisplay
	BTFSS runState, RUNSTATE_TEST	    ; if(TEST)
	GOTO changeDisplay_1
changeDisplay_test
;	CLRF outputState
;	CALL crossFade
;	COMF outputState
;	CALL crossFade
	MOVF timerRead, W
	BANKSEL PORTB
	MOVWF PORTB
	RETURN
changeDisplay_1
	BTFSC runState, RUNSTATE_RUN	    ; if(not RUN)
	GOTO changeDisplay_2
changeDisplay_stop			    ; In stop mode we fade out as fast as we can
	CALL selectChannel
	COMF fadeMask, W
	ANDWF outputState, F
	CALL crossFade
	CLRF timeToNextChange
	RETURN
changeDisplay_2
	BTFSS runState, RUNSTATE_LATCH	    ; if(LATCH)
	GOTO changeDisplay_normalRun
changeDisplay_latch			    ; In latch mode we fade in as quick as we can
	CALL selectChannel
	MOVF fadeMask, W
	IORWF outputState, F
	CALL crossFade
	CLRF timeToNextChange
	RETURN
changeDisplay_normalRun			    ; else normal run.
	MOVLW D'1'
	SUBWF timeToNextChange, F
	BTFSC STATUS, C	    ; Carry low means timeToNextChange<0
	RETURN
	CALL countLamps
changeDisplay_normalRun_retry	
	CALL selectChannel
	; Work out if we're fading in or out
	MOVF fadeMask, W
	ANDWF outputState, W
	BTFSC STATUS, Z
	GOTO changeDisplay_fadingIn
changeDisplay_fadingOut	    ; ZERO means fading out
	MOVLW D'4'
	SUBWF lampCount, W
	BTFSS STATUS, C	    ; Carry set if result is >= 0
	GOTO changeDisplay_normalRun_retry  ; Carry clear means lampCount<=3
	GOTO changeDisplay_normalRun_go	    ; lampCount>3 we can fade out
changeDisplay_fadingIn
	MOVLW D'4'
	SUBWF lampCount, W
	BTFSC STATUS, C
	GOTO changeDisplay_normalRun_retry  ; Carry set lampCount>6
changeDisplay_normalRun_go		    ; lamCount<=5 we can fade in
	MOVF fadeMask, W
	XORWF outputState, F
	CALL crossFade
	; Delay a random amount. Load W with a time in units of 0.1s
	MOVLW D'5'
	CALL nextRandom
	ANDLW B'00011111'
	MOVWF timeToNextChange
	RLF timeToNextChange, F
	RLF timeToNextChange, F
	RETURN

;-------------------------------------------------------------------------------
; Select a channel to work on. Result is in fadeMask.
;-------------------------------------------------------------------------------
selectChannel
	; Shift random number generator 3 times as we want 3 bits
	MOVLW D'3'
	CALL nextRandom
	
	ANDLW D'7' 
	ADDLW D'1'
	MOVWF COUNT1
	CLRF fadeMask
	BSF STATUS, C
selectChannel_shiftLoop
	RLF fadeMask, F
	DECFSZ COUNT1, F
	GOTO selectChannel_shiftLoop
	RETURN

;-------------------------------------------------------------------------------
; Count how many lamps are currently on.
; Result in lampCount
;-------------------------------------------------------------------------------
countLamps
	CLRF lampCount
	BTFSC outputState, 0
	INCF lampCount, F
	BTFSC outputState, 1
	INCF lampCount, F
	BTFSC outputState, 2
	INCF lampCount, F
	BTFSC outputState, 3
	INCF lampCount, F
	BTFSC outputState, 4
	INCF lampCount, F
	BTFSC outputState, 5
	INCF lampCount, F
	BTFSC outputState, 6
	INCF lampCount, F
	BTFSC outputState, 7
	INCF lampCount, F
	RETURN
	
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
	BANKSEL PORTA
;	BSF PORTA, 2
	BANKSEL TMR1H
	CLRF TMR1H
	MOVLW D'50'	; See calculation spreadsheet https://docs.google.com/spreadsheets/d/1D80uUIUwJ-t4BcKdUp8j-lb8OnDpyT60FGlRb9KkIp8/edit#gid=0
	MOVWF TMR1L
	MOVLW D'60'
	MOVWF TMR1H
	CLRF TMR0
readInput_delay
	BTFSS TMR1H, 7
	GOTO $-1
	BTFSC TMR1H, 7
	GOTO $-1
	MOVF TMR0, W
	MOVWF timerRead
	
	CLRF runState
	
	; Check to see if there was a signal.
	MOVLW D'15'
	SUBWF timerRead, W
	BTFSC STATUS, C
	BSF runState, RUNSTATE_RX

	; Inputs are 00 STOP, 01 RUN, 10 LATCH, 11 TEST
	BTFSC timerRead, 7
	GOTO readInput_latchOrTest
	BTFSC timerRead, 6
	BSF runState, RUNSTATE_RUN
	RETURN
readInput_latchOrTest
	BTFSC timerRead, 6
	GOTO readInput_selfTest
	BSF runState, RUNSTATE_LATCH
	BSF runState, RUNSTATE_RUN
	RETURN
readInput_selfTest
	BSF runState, RUNSTATE_TEST
	RETURN
	
	
	
	
;-------------------------------------------------------------------------------
; Perform a PWM transition over approximately 1 second moving PORTB to outputState
;
; If we fade over 256 transitions using an 8 bit PWM we need a decision frequency
; of about 65.5kHz. This is an inner loop of about 15 cycles. Tune this to give
; the fade length.
;-------------------------------------------------------------------------------
	subtitle "PWM output transition"
crossFade
	MOVFW outputState
	XORWF PORTB, W
	MOVWF fadeMask	    ; fadeMask when xored with PORTB will produce outputState
	CLRF  COUNT1	    ; Outer loop, increment to 255
crossFade_outerLoop
	MOVFW fadeMask
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
	MOVFW fadeMask
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
	BANKSEL prngRegister1
	MOVWF COUNT1
	INCF COUNT1, F
	; Read off the current random number before performing shifts, so that
	; the shifts end up removing now 'dirty' bits. This costs a little more
	; as I don't just end up with the result naturally in W, but is more
	; correct.
	MOVFW prngRegister1
	XORWF prngRegister2, W
	MOVWF COUNT2 ; tmp
nextRandom_shiftLoop	
	; prngRegister1 is 0x69, 7 bits long
	BCF STATUS, C  ; Clear carry
	RRF prngRegister1, W   ; Shift prngRegister1. Result goes into W
	BTFSC STATUS, C
	XORLW H'69'
	MOVWF prngRegister1
	; prngRegister2 is 0xA6, 8 bits long
	BCF STATUS, C  ; Clear carry
	RRF prngRegister2, W   ; Shift prngRegister2. Result goes into W
	BTFSC STATUS, C
	XORLW H'A6'
	MOVWF prngRegister2
	DECFSZ COUNT1, F
	GOTO nextRandom_shiftLoop
	; Return result temporarily stored in COUNT2
	MOVFW COUNT2
	RETURN
	
	END  ; directive 'end of program'
	


