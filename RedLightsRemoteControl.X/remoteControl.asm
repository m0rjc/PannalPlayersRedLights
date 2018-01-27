
;**********************************************************************
; PIC12F675 use internal 4 mHz clock    
;
; Generate an AFSK remote control signal for the Pannal Players Red Eyes
; special effect devices. The signals are:
;
;   990Hz   - Fade Out / Stop
;  1650Hz   - Run
;  2340Hz   - Latch On
;
; See spreadsheet for calculations of frequencies and detection bands.
; https://docs.google.com/spreadsheets/d/1D80uUIUwJ-t4BcKdUp8j-lb8OnDpyT60FGlRb9KkIp8/edit#gid=0
;
; The internal clock gives 5% rated accuracy. As discussed in the receiver
; code, this gives us up to 3 bits per symbol (only one symbol) transmission.
;
;**********************************************************************

	title "Pannal Players Red-Eyes Effect Remote Control"
	list      p=12f675           ; list directive to define processor
	#include <p12f675.inc>       ; processor specific variable definitions

	 __CONFIG _FOSC_INTRCIO & _WDTE_OFF & _PWRTE_ON & _MCLRE_ON & _BOREN_ON & _CP_OFF & _CPD_OFF

;**********************************************************************
; Maths for timer setting see 
; https://docs.google.com/spreadsheets/d/1D80uUIUwJ-t4BcKdUp8j-lb8OnDpyT60FGlRb9KkIp8	
;**********************************************************************
	#define	TIMER_STOP_H		D'249'
	#define	TIMER_RUN_H		D'253'
	#define	TIMER_LATCH_H		D'254'
	#define	TIMER_SELF_TEST_H	D'255'
				
	#define	TIMER_STOP_L		D'230'
	#define	TIMER_RUN_L		D'228'
	#define	TIMER_LATCH_L		D'208'
	#define	TIMER_SELF_TEST_L	D'46'
	

	UDATA_SHR	; Begin General Purpose-Register
wTmp		RES 1
statusTmp	RES 1
timerOffsetLow	RES 1	; Amount to load into the timer on interrupt. See above
timerOffsetHigh	RES 1	
tmpPtr		RES 1
inputValue	RES 1

	ORG     0x000   ; processor reset vector
	GOTO main
	
	SUBTITLE "Interrupt handler - generate the output waveform"
	ORG	0x004	; Interrupt vector
	; State save code from Microchip documentation at 
	; http://ww1.microchip.com/downloads/en/devicedoc/33023a.pdf
	MOVWF wTmp
	SWAPF STATUS,W
	MOVWF statusTmp 
	
	; Toggle output
	BANKSEL GPIO
	MOVLW b'00000001'
	XORWF GPIO, F
	
	; Load new timer value
	CLRF	TMR1L	; Ensure I have enough time
	MOVFW	timerOffsetHigh
	MOVWF	TMR1H
	MOVFW	timerOffsetLow
	MOVWF	TMR1L

	; Clear timer1 overflow
	BCF PIR1, TMR1IF

	; Re-enable the interrupt
	BANKSEL PIE1
	BSF PIE1, TMR1IE
	
	; State restore code from Microchip documentation at 
	; http://ww1.microchip.com/downloads/en/devicedoc/33023a.pdf
	SWAPF statusTmp,W 
	MOVWF STATUS 
	SWAPF wTmp,F 
	SWAPF wTmp,W 
	RETFIE

	SUBTITLE "Main Code - Initialisation"
main
	; GPIO Pins 4,5 as input with weak pullup. Pin 0 as output
	;      Pin 3 is MCLR/Vpp. 
	;      Pin 1 high impedance with weak pullup - open 
	;      Pin 2 input with weak pullup. Pull low for test mode.
	BANKSEL TRISIO
	
	MOVLW b'00110110'	    ; Weak pullups on all input pins we can
	MOVWF WPU
	BCF OPTION_REG, 7	    ; Clear GPPU to turn on pullups
	
	CLRF ANSEL		    ; Disable analogue input to allow digital
	MOVLW b'00110110'
	MOVWF TRISIO

	; Enable TIMER1 interupts
	MOVLW b'11000000'
	MOVWF INTCON
	MOVLW b'00000001'
	MOVWF PIE1

	BANKSEL T1CON
	; Disable the comparator module to allow use of GP0-3 as digital pins
	MOVLW b'00000111'
	MOVWF CMCON
	
	; Configure TIMER 1, on, internal oscillator, no prescaler, no gate
	MOVLW b'00000001'
	MOVWF T1CON
	; Load timer values based on switches
	CALL readSwitches
	
	SUBTITLE "Main Loop"
mainLoop
	;  Load timer values based on switches
	CALL readSwitches
	GOTO mainLoop
	
readSwitches
	BANKSEL GPIO
	BTFSS GPIO, 2	    ; GP2 is low for test mode
	GOTO readSwitches_testMode
readSwitches_normalOperation
	SWAPF GPIO, W	    ; Read GPIO such that GP5 and GP4 are in the bottom nibble
	ANDLW b'00000011'   ; User only GP5 and GP4 
	
	MOVWF inputValue
	CALL getTimerValueLow
	MOVWF timerOffsetLow
	
	MOVFW inputValue
	CALL getTimerValueHigh
	MOVWF timerOffsetHigh
	
	RETURN

readSwitches_testMode
	MOVLW TIMER_SELF_TEST_L
	MOVWF timerOffsetLow
	MOVLW TIMER_SELF_TEST_H
	MOVWF timerOffsetHigh
	RETURN
	
; Fetch the timer value based on a 2 bit value in W. the value in W is the
; switch input shifted down to the bottom 2 bits of the register.
; I have ensured that the TIMER_VALUES table is on a page boundary, so don't
; need to deal with overflow of PCL.
getTimerValueLow
	MOVWF tmpPtr
	MOVLW HIGH TIMER_VALUES_LOW
	MOVWF PCLATH
	MOVLW LOW TIMER_VALUES_LOW 
	ADDWF tmpPtr, W
	MOVWF PCL   ; Performs jump

getTimerValueHigh
	MOVWF tmpPtr
	MOVLW HIGH TIMER_VALUES_HIGH
	MOVWF PCLATH
	MOVLW LOW TIMER_VALUES_HIGH 
	ADDWF tmpPtr, W
	MOVWF PCL   ; Performs jump

	
	ORG	0x0300			
TIMER_VALUES_LOW		; Switches pull low. GP5 is LATCH, GP4 is RUN			
	RETLW	TIMER_LATCH_L		; Both switches closed	
	RETLW	TIMER_STOP_L		; LATCH only	
	RETLW	TIMER_RUN_L		; RUN only	
	RETLW	TIMER_STOP_L		; No Switces	
					
TIMER_VALUES_HIGH		; Switches pull low. GP5 is LATCH, GP4 is RUN			
	RETLW	TIMER_LATCH_H		; Both switches closed	
	RETLW	TIMER_STOP_H		; LATCH only	
	RETLW	TIMER_RUN_H		; RUN only	
	RETLW	TIMER_STOP_H		; No Switces		
 
	END  ; directive 'end of program'
	


