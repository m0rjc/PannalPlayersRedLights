;
; Provide "animal eyes" effects for a village pantomime.
; Drives 8 LED pairs, operating in 3 modes.
;
; STOP - The lights are off. If they were on then they turn off by fading
;	 out one at a time. The light to fade out is chosen randomly. If the
;	 chosen light is already out then no change.
;	 We found in the show that this could take too long sometimes, so would
;	 change the code to either speed up or always choose a light that's on.
;	 In the case of the latter fade out would take up to 8 seconds so
;	 work well.
;
; RUN  - lights fade in and out such that there are always some animal eyes
;	 showing. The time intervals and selection of eyes is pseudorandom.
;	 We found that sometimes parts of the auditorium could be left dark for
;	 short periods. Such is randommness. Better placement of "eyes" may help.
;
; LATCH - Lights fade in quite rapidly and stay on. This was used when we wanted
;	 lots of eyes when the wolf characters were in the auditorium. We also used
;	 it to bring up the amount of lights quickly when going into RUN.
;
; Communication is by pulse counting. We used a balanced line to get pulses from the
; controller to the effects boxes to try to avoid issues from noise sources in the
; auditorium. This worked well, though in hindsight there is a risk of any extra pulses
; upsetting the count. We didn't notice this in practice, even with quite long cable
; runs. 
;	
; The PORTB Serial pins have been left open to allow future use of the hardware with more
; more complex command sets. To make up 8 channels we use 2 pins on PORT A as well.
	
	title "Pannal Players Red-Eyes Effect"
	list      p=16f628A           
	#include <p16F628A.inc>       

	 __CONFIG _FOSC_INTOSCCLK & _WDTE_OFF & _PWRTE_ON & _MCLRE_OFF & _BOREN_OFF & _LVP_OFF & _CPD_OFF & _CP_OFF

	; Random seeds. Two non zero values
	#define RANDOM_SEED_1 b'10010101'
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
	

	UDATA_SHR		; Use shared access RAM for simplicity in this project.
COUNT1	    RES 1		; Three temporary variables
COUNT2	    RES 1
COUNT3	    RES 1
prngRegister1	    RES 1		; Random number generator shift register 1
prngRegister2	    RES 1		; Random number generator shift register 2
outputState	    RES 1		; Desired output state of the LEDs
fadeMask	    RES 1		; Port B mask to use when fading
fadeMaskA	    RES 1		; Port A fade mask
runState	    RES 1		; System run state
	    #define RUNSTATE_RUN    7
	    #define RUNSTATE_LATCH  1
	    #define RUNSTATE_RX	    3
	    #define RUNSTATE_TEST   2
	    #define RUNSTATE_SKEW   0	; Clock skew detected
timeToNextChange    RES 1		; How many 0.1s reads to wait until next lighting change
lampCount	    RES 1		; How many lamps are currently lit
timerRead	    RES 1		; Number of pulses read by the timer.
timerTuning	    RES 1		; Control loop variable to adjust sample period.
timerError	    RES 1		; Magnitude of the control signal error, for debugging

	ORG     0x000   ; processor reset vector

setup 

	BANKSEL CMCON
	movlw	0x07  ; Turn comparators off
	movwf	CMCON

	banksel TRISB	    
	CLRF TRISB	    ; All outputs
	BCF TRISB, 1	    ; ... except RB1 which is RX if we use serial comms
	CLRF TRISA	    ; All outputs
	BSF TRISA, 4	    ; ... except RA4 for Timer0 clock 
	
	banksel INTCON 
	clrf	PORTB
	clrf	PORTA

	; Timer 1, 1:2 prescaler, internal clock
	MOVLW b'00010001'
	MOVWF T1CON
	MOVLW D'127'	    ; Start with the control loop at mid-point.
	MOVWF timerTuning
	
	; Timer 0, 1:1 prescaler, external input on RA4
	CLRWDT ; (See instructions in datasheet for safe setup of prescaler)
	CLRF TMR0 
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

	BSF runState, RUNSTATE_RUN

main
	CALL readInput

	; Write runstate debugging to the LEDs
	BANKSEL PORTA
	MOVF runState, W
	ANDLW b'11001001'   ; Do not affect driver outputs or inputs
	IORWF PORTA, F	    ; Set any on LEDs from runstate
	MOVF runState, W
	IORLW b'00110110'   ; Do not affect driver outputs or inputs
	ANDWF PORTA, F

	CALL changeDisplay
	GOTO main
	
;-------------------------------------------------------------------------------
; Change the display according to run mode.
;-------------------------------------------------------------------------------	
changeDisplay
	BTFSS runState, RUNSTATE_TEST	    ; if(TEST)
	GOTO changeDisplay_1
changeDisplay_test
	MOVF timerRead, W
	MOVWF outputState
	CALL crossFade
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
	COMF outputState, W
	BTFSC STATUS, Z
	GOTO changeDisplay_latch_nothingToDo
	CALL selectChannel
	MOVF fadeMask, W
	ANDWF outputState, W	   ; Check to see if the bit is already set
	BTFSS STATUS, Z		    ; ZERO means it was not
	GOTO changeDisplay_latch
	MOVF fadeMask, W
	IORWF outputState, F
	CALL crossFade
changeDisplay_latch_nothingToDo
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
	; Multiple timerTuning by 32 and place in {COUNT2,COUNT1}
	MOVF timerTuning, W
	MOVWF COUNT1
	CLRF COUNT2
	BCF STATUS, C
	RLF COUNT1  ;2
	RLF COUNT2
	RLF COUNT1  ;4
	RLF COUNT2
	RLF COUNT1  ;8
	RLF COUNT2
	RLF COUNT1  ;16
	RLF COUNT2
	RLF COUNT1  ;32
	RLF COUNT2
	; Add the minimum counts calculated in the spreadsheet such that 
	; when timerTuning is mid range we get the desired count.
	; https://docs.google.com/spreadsheets/d/1D80uUIUwJ-t4BcKdUp8j-lb8OnDpyT60FGlRb9KkIp8/edit#gid=0
	MOVLW D'210'  ; TMR1L
	ADDWF COUNT1, F
	BTFSC STATUS, C
	INCF COUNT2, F
	MOVLW D'44'	; TMR1H
	ADDWF COUNT2, F
	
	BANKSEL TMR1H
	CLRF TMR1H
	MOVF COUNT1, W
	MOVWF TMR1L
	MOVF COUNT2, W
	MOVWF TMR1H
	
	; Start counting pulses
	CLRF TMR0
readInput_delay
	BTFSS TMR1H, 7
	GOTO $-1
	BTFSC TMR1H, 7
	GOTO $-1
	MOVF TMR0, W
	MOVWF timerRead
		
	; Check to see if there was a signal. If not then skip read routine.
	BCF runState, RUNSTATE_RX
	MOVLW D'15'
	SUBWF timerRead, W
	BTFSS STATUS, C
	RETURN
	
	CLRF runState
	BSF runState, RUNSTATE_RX
	
	; Check for timer skew by looking for:
	;	b'xx10xxxx' - We got too many or correct counts (signal fast)
	;	b'xx01xxxx' - We got too few counts (signal slow)
	; If the timer is close enough run the feedback loop to bring it in
	; line.
	MOVF timerRead, W
	ANDLW b'00110000'   ; Bits we're interested in
	XORLW b'00100000'   ; XOR with desired result and check for zero
	BTFSC STATUS, Z
	GOTO readInput_skewCountHigh
	MOVF timerRead, W
	ANDLW b'00110000'   ; Bits we're interested in
	XORLW b'00010000'   ; XOR with desired result and check for zero
	BTFSC STATUS, Z
	GOTO readInput_skewCountLow
readInput_skewWacko
	BSF runState, RUNSTATE_SKEW 
	GOTO readInput_decodeCount
readInput_skewCountHigh	    ; We got a slightly high count so reduce the period
			    ; by increasing the timer initial value
	MOVF timerRead, W   ; debug show error
	ANDLW d'00001111'
	MOVWF timerError

	COMF timerTuning, W ; First we check we've not overflowed.
	BTFSS STATUS, Z
	INCF timerTuning, F
	GOTO readInput_decodeCount
readInput_skewCountLow	    ; We got a slightly low count so increase the period
	COMF timerRead, W   ; debug show negative of error
	ADDLW d'1'
	ANDLW d'00001111'
	MOVWF timerError
	
	MOVF timerTuning, F ; by reducing the timer initial value.
	BTFSS STATUS, Z	    ; First we check we've not overflowed.
	DECF timerTuning, F
	; Drop through
readInput_decodeCount
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
; Perform a PWM transition over approximately 1 second moving output pins to outputState
;
; Pin assignments:	output: 76543210
;			PORTA :      21
;			PORTB : 76543  0
;
; If we fade over 256 transitions using an 8 bit PWM we need a decision frequency
; of about 65.5kHz. This is an inner loop of about 15 cycles. Tune this to give
; the fade length.
;-------------------------------------------------------------------------------
	subtitle "PWM output transition"
crossFade
	; Port B fade mask
	MOVFW outputState
	XORWF PORTB, W
	ANDLW b'11111001'   ; Pins affected on Port B
	MOVWF fadeMask	    ; fadeMask when xored with PORTB will produce outputState
	
	; Port A fade mask
	MOVFW outputState
	XORWF PORTA, W
	ANDLW b'00000110'   ; Pins affected on Port A
	MOVWF fadeMaskA
	
	CLRF  COUNT1	    ; Outer loop, increment to 255
crossFade_outerLoop
	MOVFW fadeMask
	XORWF PORTB, F	    ; PWM high
	MOVFW fadeMaskA
	XORWF PORTA, F	
	INCF  COUNT1, F	    ; Increment outer counter, finish on overflow
	BTFSC STATUS, Z
	GOTO crossFade_end
	MOVFW COUNT1	    ; count2 = count1
	MOVWF COUNT2
	NOP		    ; NOPs here plus the logic above and below
	NOP		    ; will take the minimum time to PWM low to 15 operations
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
	MOVFW fadeMaskA
	XORWF PORTA, F
	MOVFW COUNT1
	MOVWF COUNT2	    ; count2 = count1 (count1 = {1..255})
	COMF COUNT2, F	    ; count2 ones complement {254..0}
	INCF COUNT2, F	    ; count2 ranges {255..1}
	NOP		    ; NOPs here plus the logic above and below
	NOP		    ; will take the minimum time to PWM high to 15 operations
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
	


