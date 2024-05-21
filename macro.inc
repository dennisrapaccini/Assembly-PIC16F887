
;Macro per settare il valore di un registro		
setRegK			macro Reg, Const
		movlw Const
		banksel Reg
		movwf Reg
				endm


;Macro per azzerare un registro
setReg0			macro Reg
		banksel Reg
		clrf Reg
				endm


