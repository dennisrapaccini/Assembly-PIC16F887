;		      PROGETTO DI SISTEMI ELETTRONICI	    
;			        Tesina n.7
;			     Dennis Rapaccini 
;***DESCRIZIONE***
;Il progetto consiste nella realizzazione tramite microcontrollore PIC16F887 
;di un firmware in linguaggio assembly. Il PIC riceve da computer (tramite 
;porta EUSART) una cifra N tale che 0 <= N <= 9 e pilota il buzzer in modo da
;far emettere N brevi suoni alla frequenza di 932 Hz (Sib5). 
;Se il valore di N non è compreso nel range, esso viene ignorato e viene 
;inviato un messaggio di errore alla porta EUSART.
    
		#include "p16f887.inc"
		#include "macro.inc" 
		list     p=16f887
		; Configuration Bits
		__CONFIG _CONFIG1, _INTRC_OSC_NOCLKOUT & _WDT_OFF & _LVP_OFF
		__CONFIG _CONFIG2, _BOR21V


;***DEFINIZONE COSTANTI***
;Setting del contatore di Timer1 (prescaler a 1:8) per contare 300ms, 
;periodo di accensione/spegnimento del buzzer. 
		
tmr_300ms	EQU	    (.65536-.37500)

;Setting del periodo della PWM (Timer2, prescaler a 1:16)
;per avere un Sib5 (932 Hz).	

tone		EQU	    .67 
		
;Limite inferiore (N=0) e limite superiore (N=9) ammissibili da sottrarre 
;al numero ricevuto 
ascii_inf	EQU	    (.255-.48)
ascii_sup	EQU         (.255-.57)

	
;***DEFINIZIONE VARIABILI***
;Allocazione delle variabili in UDATA_SHR per salvare i valori dei registri W, 
;STATUS e PCLATH al momento dell'interrupt.
			UDATA_SHR
w_TEMP		RES	    1
status_TEMP	RES         1
pclath_TEMP	RES	    1
	
;Allocazione della variabile che contiene il numero ricevuto.
n		RES	    1

;Allocazione byte contenenti eventuali bits di flag
flag		RES	    1

;Allocazione variabili tempranee di appoggio
var1		RES         1
var2		RES	    1

;Allocazione variabile per contare il numero di byte rimasti da stampare e il
;numero di iterazioni
eusartCount     RES	    1 
counter		RES         1

;Allocazione della variabile per la stampa a video		
                        UDATA
printBuff       RES	    1
       
;***RESET VECTOR***
;In caso di reset del microcontrollore, la prima istruzione eseguita sarà
;indicata dalla label START.
RST_VECTOR	CODE	0x0000
			pagesel	start			
			goto	start
			
;*** INIZIO MAIN CODE***

MAIN		CODE
start			;Inizializzazione hardware
			pagesel initHw 
			call initHw
			clrf flag
			
			
			;Abilitazione interrupt periferici
			banksel INTCON ; 
			bsf INTCON, PEIE  
main_loop		
			setReg0 RCREG
			bcf INTCON, GIE  ;Disabilitazione interrupt globale per 
					 ;non entrare in interrupt al risveglio
			banksel BAUDCTL
			bsf BAUDCTL, WUE ;Abilitazione wake-up da EUSART
			banksel PIE1
			bsf PIE1,RCIE    ;Abilitazione interrupt da EUSART 
			                 ;in ricezione
			
			;Sleep con LED1 spento
			banksel PORTD
			bcf PORTD, 0
			sleep 
			
			;Ricezione wake-up break 
			banksel RCREG
			movf RCREG, W    ;Alla lettura di RCREG si azzera 
			                 ;l'interrupt flag
			banksel PIE1 
			bcf PIE1, RCIE   ;Disabilitazione interrupt da USART 
			                 ;in ricezione
			bsf INTCON, GIE  ;Abilitazione interrupt globale per 
					 ;EUSART in trasmissione
			banksel PORTD     
			bsf PORTD, 0     ;Accensione LED1 (wake-up)

;Routine che gestisce il carattere ricevuto			
enterNumber		
			banksel PIR1
			btfss PIR1, RCIF ;Aspetto finché l'EUSART Receive 
					 ;Interrupt Flag bit è 1
			goto $-1
			banksel RCREG
			
			;Controllo correttezza valore di N
			movf RCREG, W    ;Spostamento RECREG(byte ricevuto) su W
			movwf n          ;Spostamento W su n
			addlw ascii_inf  ;Sottrazione del numero ricevuto a .48 
			                 ;che equivale a sommare tale numero con 
					 ;.255-.48 
			movwf var1       ;Spostamento W (risultato sottr.)a var1
			movf n, W        ;Spostamento di n a W
			addlw ascii_sup  ;Sottrazione del numero ricevuto a .57
			movwf var2       ;Spostamento W (risultato sottr.)a var2
			
			movf var1, W	 ;Spostamento var1 in W
			subwf var2, W    ;Sottrazione var2 a W
			
			skpc		 ;Se il carry = 1 allora N è valido
					 ;(equivale a btfss STATUS,C)
			goto nonValid
			
			
;n valido				
ascii2dec	
		movlw .48   
		subwf n,W     ;Conversione di n da ASCII in decimale
		movwf counter ;Salvataggio di quest'ultimo in counter
		
valid		
		setRegK TMR1L, low tmr_300ms ; "Azzeramento" timer1
		setRegK TMR1H, high tmr_300ms
		banksel T1CON 
		bcf PIR1,TMR1IF   ; Clear flag overflow timer1
		bsf T2CON, TMR2ON ; PWM ON
		call delay
		bcf T2CON, TMR2ON ; PWM OFF
		call delay
		
		decfsz counter, f ;Decremento n, se 0 va a theend
		goto valid
		goto theend

;Routine che gestisce N non valido. Stampa un messaggio di errore
nonValid		
			setReg0 RCREG
			setReg0 printBuff 
			banksel printBuff
			movlw 'N'
			movwf printBuff
			movlw 'O'
			movwf (printBuff+1)
			movlw 'T'
			movwf (printBuff+2)
			movlw ' '
			movwf (printBuff+3)
			movlw 'V'
			movwf (printBuff+4)
			movlw 'A'
			movwf (printBuff+5)
			movlw 'L'
			movwf (printBuff+6)
			movlw 'I'
			movwf (printBuff+7)
	        	movlw 'D'
			movwf (printBuff+8)
			movlw .10 ;Invio
			movwf (printBuff+9)
			movlw .10
			call startTx
			btfsc flag, 0
			goto $-1	  ;Attesa fine trasmissiome
			banksel TXSTA     ;Aspetto che l'ultimo byte sia 
			btfss TXSTA, TRMT ;effettivamente trasmesso alla EUSART
			goto $-1 

;Routine effettuata alla fine di valid e nonValid
theend			
			clrf n        ;Clear n
			setReg0 RCREG ;Clear RCREG
			banksel PIE1 
			bsf PIE1, RCIE ;Clear EUSART Receive Interrupt Enable bit
			goto main_loop 



delay		
		banksel T1CON
		bsf T1CON, TMR1ON    ; Accensione timer1
		bcf PIR1,TMR1IF      ; Clear flag overflow timer1
waitdelay
		btfss PIR1,TMR1IF    ; Polling fino a overflow timer1
		goto waitdelay 
		return 

;Routine di inizio stampa su EUSART per salvare l'indirizzo iniziale della 
;stringa e il numero dei caratteri, utilizzati poi nell'ISR per la trasmissione
;vera e propria.
startTx
		movwf eusartCount    ;Salvataggio su eusartCount del numero di 
				     ;caratteri da stampare (presente in W) 
		bankisel printBuff   
		movlw printBuff      ;Scrittura indirizzo di printBuff su W
		movwf FSR            ;Scrittura indirizzo di printBuff su FSR
		bsf flag, 0          ;Set del primo bit di flag per indicare
		                     ;la trasmissione in corso
		banksel PIE1         
		bsf PIE1, TXIE       ;Abilitazione interrupt da EUSART 
				     ;in trasmissione                        
		return

;Interrupt service routine per trasmissione EUSART
ISR		CODE	    0x0004    
		
		;Salvataggio registri W, STATUS e PCLATH (context saving)
		movwf w_TEMP
		swapf STATUS, W
		movwf status_TEMP
		movf PCLATH, W
		movwf pclath_TEMP
eusartISR	
		;Controllo se interrupt causato da fine trasmissione 
		;(TXIE=1 && TXIF=1), ossia se interrupt EUSART disattivato e 
		;buffer vuoto), se sì si va in fine ISR.
		banksel PIE1
		btfss PIE1, TXIE ;Controllo EUSART Transmit Interrupt Enable bit
		goto endISR
		banksel PIR1     
		btfss PIR1, TXIF ;Controllo EUSART Transmit Interrupt Flag bit 
				 ;(se 1 EUSART buffer è vuoto)
		goto endISR
		
		
	
		movf eusartCount, W ;Spostamento eusartCount su W
		btfsc STATUS, Z     ;Controllo se eusartCount =0, 
		                    ;ossia byte da stampare terminati
		goto endTx
		
		;Trasmissione caratteri
		movf INDF, W ; Spostamento di INDF (contenuto dell'indirizzo
			     ; puntato da FSR) su TXREG
		banksel TXREG 
		movwf TXREG
		incf FSR, f         ; Incremento puntatore
		decf eusartCount, f ; Decremento counter caratteri rimasti
		goto endISR


;Routine di fine trasmissione EUSART		
endTx
		bcf flag, 0    ;Clear flag per indicare trasmissione terminata
		banksel PIE1
		bcf PIE1, TXIE ;Disabilitazione interrupt da EUSART 
			       ;in trasmissione
		goto endISR    ;A fine trasmissione si va nella routine di fine 
			       
;Ripristino registri W, STATUS e PCLATH come prima di interruzione
endISR	
		swapf status_TEMP, W
		movwf STATUS
		swapf w_TEMP, f
		swapf w_TEMP, W
		movf pclath_TEMP, W
		movwf PCLATH
		retfie

;***INIZIALIZZAZIONE HARDWARE***	
initHw		
		
		setRegK OSCCON, B'01110001' 
		
		setRegK OPTION_REG, B'00000111'
		
		;Timer1 (prescaler 1:8 e clock interno)
		setRegK	T1CON, B'00110000' 
		
		;Timer2 (prescaler 1:16)
		setRegK T2CON, B'00000010'
		
		;CCP1CON (in PWM)
		setRegK CCP1CON, B'00001100'
		
		;Porta A:
		setRegK PORTA, B'01000000' 
		setRegK ANSEL, B'11111111' 
		setRegK TRISA, B'00111111'

		;Porta B:
		setReg0 ANSELH
		setRegK PORTB, B'00100000' 
		setRegK TRISB, B'11011111'
		setRegK WPUB, B'00011111'
		movf PORTB, W 
		banksel INTCON
		bcf INTCON, RBIF
		movlw 0x0F
		banksel IOCB  
		movwf IOCB
		banksel INTCON
		bsf INTCON, RBIE  

		;Porta C;
		setReg0 PORTC
		setRegK TRISC, B'11111011'

		;Porta D:
		setReg0 PORTD
		setRegK TRISD, 0xF0

		;Porta E:
		setReg0 PORTE

		;ADC
		setRegK ADCON0, B'10011001'
		setReg0 ADCON1 
		
		;EUSART
		setRegK TXSTA, B'00100100'
		setRegK RCSTA, B'10010000'
		setReg0 BAUDCTL
		setRegK SPBRG, .25
		
		setRegK	PR2, tone  ;Impostazione frequenza (periodo) PWM
		bcf STATUS, C	   ;Clear carry
		rrf PR2, W	   ;Rotate right per effettuare divisione per 2
		banksel CCPR1L
		movwf CCPR1L
		setRegK	CCP1CON, B'00001100' ; Modalità PWM
		
		return
		
	end