/**
 Calculator.asm

 Created: 02/11/1396 11:43:14 ب.ظ
 Author : Hasti Hassani Moughadam, Aryan Ebrahimpour

 Let Fin = 1MHz

 Title:
	2 BCD digit calculator
 Description:
	This project is an implemention of BCD calculation, for example (99 x 99 = 9801)

	How to use it in proteus
		1- Add these devices: "ATMEGA32", "7Seg-BCD", "KEYPAD-SMALLCALC"
		2- Insert 4 "7-Seg BCD" in sheet
		3- Connect "PortC(3:0) -> 1st 7-seg, PortC(7:4) -> 2nd 7-seg, PortD(3:0) -> 3rd 7-Seg, PortD(7:4) -> 4th 7-Seg"
		4- Connect "PortA(3:0)" -> rows of keypad, "PortA(7:4)" -> columns of keypad

 Project Description:
	Project has 5 main sections:
		1- EQU Section: Use simple naming, instead of numbers.
		2- DEF Section: Use simple naming, instead of registers.
		3- Keyboard Section: This part scans 4 rows of keyboard and calls "KeyPressed process" section to handle it.
		4- KeyPressed Section: Handles KeyPress events, states and caluclations.
		5- External Libraries: Contains a function to convert a 16-bit binary value to 5 BCDs.
**/

// ==================== EQU : Begin ====================

	// States of Calculation
	.equ FirstNumberState    = 0
	.equ OperatorState       = 1
	.equ SecondNumberState   = 2
	.equ WaitForEqualSign    = 3
	.equ ShowingResult       = 4

	// What operation selected?
	.equ SumOperation        = 5
	.equ MultOperation       = 6
	.equ SubtractOperation   = 7
	.equ DivOperation        = 8

	// Stack Pointer
	.equ SPHValue = $05
	.equ SPLValue = $00

// ==================== EQU : End ====================

// ==================== DEF : Begin ====================

	// Argument of Function
	.def SelectOperatorArgument = R13 // What Operation (Sum, Sub, Mul, Div) shall be passed to SelectOperator Function ?
	.def CurrentKeyNum          = R15 // What Num Key (0..9) shall be passed to NumProcess function ?

	// State related and functionality
	.def CurrentOp              = R14 // What Operation (Sum, Sub, Mul, Div) has user chosen?
	.def CurrentState           = R25 // In which state we are currently at? (First number, Operator, Second number, Equal sign, Show result)
	.def CurrentKey             = R20 // What key pressed?
	.def FirstNumber            = R21 // What is first operand?
	.def SecondNumber           = R22 // What is second operand?
	.def ResultValueL           = R23 // What is the lower 8-bit of the answer?
	.def ResultValueH           = R24 // What is the higher 8-bit of the answer?
	.def ResultValueLTemp       = R16 // A temporary version of "ResultValueL" to do math things on it. (For showing multiply large value in BCD)
	.def ResultValueHTemp       = R17 // A temporary version of "ResultValueH" to do math things on it. (For showing multiply large value in BCD)

	// View related registers
	.def Ones                   = R26 // Number on ones when showing Sum, Sub and Div in BCD
	.def Tens                   = R27 // Number on tens when showing Sum, Sub and Div in BCD
	.def Hundreds               = R28 // Number on hundres when showing Sum, Sub and Div in BCD

	// Binary To BCD Conversation Defs
	.def BCD16Output0           = R4 // BCD1:BCD0 8-bit output of multiply large value in BCD
	.def BCD16Output1           = R5 // BCD3:BCD2 8-bit output of multiply large value in BCD
	.def BCD16Output2           = R6 // 0000:BCD4 8-bit output of multiply large value in BCD
	.def BB16_Count1            = R30 // Counter 1 for 8-bit output of multiply large value in BCD
	.def BB16_Count2            = R31 // Counter 2 for 8-bit output of multiply large value in BCD

// ==================== DEF : End ====================

.include "m32def.inc"

.org     $0200
.db      $ee, $ed, $eb, $e7, $de, $dd, $db, $d7, $be, $bd, $bb, $b7, $7e, $7d, $7b, $77 // Keyboard Table of Key States

.org     0
rjmp     main

.org     $0050
main:
	sei             // Enable Intrupts
	ldi r16, $F0
	out ddra, r16   // PortA [Out | In] // Keyboard in-out
	ldi r16, $FF
	out ddrc, r16   // PortC [Out] // 8bit, 4bit 1st 7Seg, 4bit 2nd 7Seg
	out ddrd, r16   // PortD [Out] // 8bit, 4bit 3rd 7seg, 4bit 4th 7Seg
	ldi r16, SPHValue
	out SPH, r16
	ldi r16, SPLValue
	out SPL, r16   // Stack Pointer set to SPH|SPL
	ldi CurrentState, FirstNumberState // Set Current state to "FirstNumberState"

First:
	ldi r18, $EF // r18 is now 1110|1111 (one Zero to shift for scanning)
	ldi r17, $00

// ==================== KeyBoard Region : Begin ====================
Loop:

	out  porta, r18 // Clear first row
	in   r19, pina // Read PinA signal
	andi r19, $0f // We only need lower part of input value
	cpi  r19, $0F
	brne ValidInput // R18: Row# & R19: Column#
				    // rrrr xxxx     xxxx cccc
	inc  r17
	cpi  r17, 4 // Scanned all rows without any result? Sad. Back top.
	breq First
	
	rol  r18 // Nothing here. Scan next row.
	rjmp Loop

ValidInput:  // When you are here, you have exactly one Zero in keypad's row, and exactly one Zero in keypad's column
	andi r18, $f0
	andi r19, $0f
	or   r18, r19 // R18 = Row | Column
	ldi  r17, $00

	ldi  r31, $04 // Load Address of ((Table of Keyes) x 2) in X(R31:R30)
	ldi  r30, $00
	
Next:
	lpm  r0, z+ // Search all the memory cells, from $0400 to $0400 + 16
	cp   r0,  r18
	breq ExitWithFoundKey
	inc  r17
	cpi  r17, 16
	breq ExitWithoutFoundKey
	rjmp Next

ExitWithFoundKey:
	mov CurrentKey, r17 // Code found, move it to CurrentKey register
	rjmp KeyOp
	
ExitWithoutFoundKey:
	ldi CurrentKey, $00 // Code not found, move zero instead

KeyOp:
	call KeyPressed // Call KeyPress function that handles all the keys on the keyboard, all the math stuff and all the state controlling.
					// Thats annoying, isn't it?

	call Update7Segments // Update the UI
	
Wait:
	in   r19, pina // Wait until the user get his/her finger out of buttons
	andi r19, $0f   
	cpi  r19, $0f
	brne Wait

	rjmp First
// ==================== KeyBoard Region : End ====================

// ==================== Pressed Key Process : Begin ====================
KeyPressed:
	 cpi CurrentKey, 13 // Here is a hard-coded jump table for each key of keypad.
	 breq ZeroPressed   // For example, The key that has lable "=" on it, is 14th element of our keyboard table.
	 cpi CurrentKey, 8
	 breq OnePressed
	 cpi CurrentKey, 9
	 breq TwoPressed
	 cpi CurrentKey, 10
	 breq ThreePressed
	 cpi CurrentKey, 4
	 breq FourPressed
	 cpi CurrentKey, 5
	 breq FivePressed
	 cpi CurrentKey, 6
	 breq SixPressed
	 cpi CurrentKey, 0
	 breq SevenPressed
	 cpi CurrentKey, 1
	 breq EightPressed
	 cpi CurrentKey, 2
	 breq NinePressed
	 cpi CurrentKey, 15
	 breq SumPressed
	 cpi CurrentKey, 11
	 breq SubtractPressed
	 cpi CurrentKey, 7
	 breq MultPressed
	 cpi CurrentKey, 3
	 breq DividePressed
	 cpi CurrentKey, 14
	 breq EqualPressed
	 cpi CurrentKey, 12
	 breq ClearPressed
	 ClearPressed:           // What happens when you click on the reset button?
		ldi ResultValueL, 0
		ldi ResultValueH, 0  // Set result value of former calculation to zero
		ldi CurrentState, FirstNumberState // Change state of calculator to zero
		ldi FirstNumber, 0   // Set First Number to zero
		ldi SecondNumber, 0  // Set second number to zero
		ldi r16, 0
		out portc, r16       // 7-segs all zero
		out portd, r16
		ret

	// From "ZeroPressed" to "NinePressed" events, we set "CurrentKeyNum" argument to specified value, and that jump to "NumberProcess"
	ZeroPressed:
		ldi r16, 0
		mov CurrentKeyNum, r16
		rjmp NumberProcess

	OnePressed:
		ldi r16, 1
		mov CurrentKeyNum, r16
		rjmp NumberProcess

	TwoPressed:
		ldi r16, 2
		mov CurrentKeyNum, r16
		rjmp NumberProcess

	ThreePressed:
		ldi r16, 3
		mov CurrentKeyNum, r16
		rjmp NumberProcess

	FourPressed:
		ldi r16, 4
		mov CurrentKeyNum, r16
		rjmp NumberProcess

	FivePressed:
		ldi r16, 5
		mov CurrentKeyNum, r16
		rjmp NumberProcess

	SixPressed:
		ldi r16, 6
		mov CurrentKeyNum, r16
		rjmp NumberProcess

	SevenPressed:
		ldi r16, 7
		mov CurrentKeyNum, r16
		rjmp NumberProcess

	EightPressed:
		ldi r16, 8
		mov CurrentKeyNum, r16
		rjmp NumberProcess

	NinePressed:
		ldi r16, 9
		mov CurrentKeyNum, r16
		rjmp NumberProcess

	// From "SumPressed" to "DividePressed" events, we set "SelectOperatorArgument" to specified Operation value, and that jump to "SelectOperator" 
	SumPressed:
		ldi r16, SumOperation
		mov SelectOperatorArgument, r16
		rjmp SelectOperator

	SubtractPressed:
		ldi r16, SubtractOperation
		mov SelectOperatorArgument, r16
		rjmp SelectOperator

	MultPressed:
		ldi r16, MultOperation
		mov SelectOperatorArgument, r16
		rjmp SelectOperator

	DividePressed:
		ldi r16, DivOperation
		mov SelectOperatorArgument, r16
		rjmp SelectOperator

	/*
		This function works, and only works when "CurrentState" is "FirstNumberState" or "OperatorState".
		This function reads "SelectOperatorArgument" and then sets CurrentOp.
		Then, state will change to "SecondNumberState"
	*/
	SelectOperator:
		cpi CurrentState, FirstNumberState
		breq ChangeCurrentOperator
		cpi CurrentState, OperatorState
		breq ChangeCurrentOperator
		ret
		ChangeCurrentOperator:
			mov CurrentOp, SelectOperatorArgument
			ldi r16, SecondNumberState
			mov CurrentState, r16
		ret
	
	/*
		This function called when "CurrentState" is "SecondNumberState" or "WaitForEqualSign".
		It evaluates (FirstNumber +=x/ SeconNumber) expression and moves result to "ResultValueH:ResultValueL" registers
	*/
	EqualPressed:
		cpi CurrentState, SecondNumberState
		breq ArithEvaluate
		cpi CurrentState, WaitForEqualSign
		breq ArithEvaluate
		ret
		ArithEvaluate:
			mov r16, CurrentOp  // Decides where to jump
			cpi r16, SumOperation
			breq SumStuff
			cpi r16, SubtractOperation
			breq SubStuff
			cpi r16, MultOperation
			breq MultStuff
			cpi r16, DivOperation
			breq DivStuff

			// FirstNumber + SeconNumber
			SumStuff:
				mov r16, FirstNumber
				add r16, SecondNumber
				ldi ResultValueH, 0
				mov ResultValueL, r16
				ldi CurrentState, ShowingResult
				ret

			// FirstNumber - SeconNumber
			SubStuff:
				mov r16, FirstNumber
				sub r16, SecondNumber
				ldi ResultValueH, 0
				mov ResultValueL, r16
				ldi CurrentState, ShowingResult
				ret

			// FirstNumber x SeconNumber
			MultStuff:
				mul FirstNumber, SecondNumber
				mov ResultValueH, R1
				mov ResultValueL, R0
				ldi CurrentState, ShowingResult
				ret

			// FirstNumber / SeconNumber
			DivStuff:
				cpi  SecondNumber, 0
				breq errorFound
				mov r16, FirstNumber
				ldi r17, 0
				devFinder:
					cp   r16 , SecondNumber
					brlo devFound
					sub  r16, SecondNumber
					inc  r17
					rjmp devFinder
				devFound:
					ldi ResultValueH, 0
					mov ResultValueL, r17
					ldi CurrentState, ShowingResult
					ret
				errorFound:
					ret

	/*
		This function is called when you tap on a Number key of keypad (0..9).
		It only works when you are in "FirstNumberState" or "SecondNumberState".
	*/
	NumberProcess:
		cpi CurrentState, FirstNumberState
		breq ProcessFirstNumberStateEvent
		cpi CurrentState, SecondNumberState
		breq ProcessSecondNumberStateEvent	
		ret
		ProcessFirstNumberStateEvent: // Are we getting first number?
			cpi FirstNumber, 10  // If number is > 10
			brsh PressedGoToFirstOpState // then Go to OnePressedGoToFirstOpState (change state o "OperatorState")
			ldi r16, 10
			mul FirstNumber, r16 // FirstNumber = FirstNumber x 10
			mov FirstNumber, R0
			mov r16, CurrentKeyNum
			add FirstNumber, r16   // FirstNumber = FirstNumber x 10 + (Pressed Key Value)
			cpi FirstNumber, 10
			brsh PressedGoToFirstOpState
			ret

		ProcessSecondNumberStateEvent: // Exactly as same above, but for Second number
			cpi SecondNumber, 10
			brsh PressedGoToSecondOpState
			ldi r16, 10
			mul SecondNumber, r16
			mov SecondNumber, R0
			mov r16, CurrentKeyNum
			add SecondNumber, r16
			cpi SecondNumber, 10
			brsh PressedGoToSecondOpState
			ret
		PressedGoToFirstOpState:  // FirstNumberState --> OperatorState
			ldi CurrentState, OperatorState 
			ret
		PressedGoToSecondOpState: // SecondNumberState --> WaitForEqualSign
			ldi CurrentState, WaitForEqualSign 
			ret
		PressedReturn:
			ret
// ==================== Pressed Key Process : End ====================

// ==================== Update Seven Segments : Begin ====================
Update7Segments:
	cpi CurrentState, FirstNumberState
	breq FirstNumberStateEvent
	cpi CurrentState, OperatorState
	breq FirstNumberStateEvent
	cpi CurrentState, SecondNumberState
	breq SecondNumberStateEvent
	cpi CurrentState, WaitForEqualSign
	breq SecondNumberStateEvent
	cpi CurrentState, ShowingResult
	breq ShowingResultEvent

	// Show FirstNumber in 7-segments
	FirstNumberStateEvent:
		mov r16, FirstNumber
		ldi r17, 0
		findTens1:
			cpi r16 , 10
			brlo showNumbers1
			subi R16, 10
			inc  r17
			rjmp findTens1
		showNumbers1:
			swap r17
			or   r17, r16
			out portc, r17
		ret

	// Show SecondNumber in 7-segments
	SecondNumberStateEvent:
		mov r16, SecondNumber
		ldi r17, 0
		findTens2:
			cpi r16 , 10
			brlo showNumbers2
			subi R16, 10
			inc  r17
			rjmp findTens2
		showNumbers2:
			swap r17
			or   r17, r16
			out portc, r17
		ret

	/*
		This function shows result of math stuff in 7-Segments.
		IF math operation is (Sum, Sub, Div) then we have a Non-Mult Show. (We don't need 16-bit Binary to BCD conversation)
		If math operatiob is Multiply, then we have a Mult show. (Unfortunately, we have to convert a 16-bit binary value to 4 BCD)
	*/
	ShowingResultEvent:
		mov r16, CurrentOp 
		cpi r16, SumOperation
		breq ShowNonMult
		cpi r16, SubtractOperation
		breq ShowNonMult
		cpi r16, DivOperation
		breq ShowNonMult
		cpi r16, MultOperation
		breq ShowMult
		ShowNonMult:
			mov Ones, ResultValueL
			ldi Tens, 0
			ldi Hundreds, 0
			findHundreds3:
				cpi  Ones, 100
				brlo findTens3
				subi Ones, 100
				inc  Hundreds
				rjmp findHundreds3
			findTens3:
				cpi  Ones, 10
				brlo showNumbers3
				subi Ones, 10
				inc  Tens
				rjmp findTens3
			showNumbers3:
				swap Tens
				or   Tens, Ones
				out portc, Tens
				out portd, Hundreds
			ret

		ShowMult:
			call BinarytoBCD
			out portc, BCD16Output0
			out portd, BCD16Output1
			ret

// ==================== Update Seven Segments : End ====================

// ==================== External Libraries and Functions : Begin ====================

BinarytoBCD:
	ldi r30, 0
	ldi r31, 0
	mov ResultValueLTemp, ResultValueL
	mov ResultValueHTemp, ResultValueH
	ser	BB16_Count2
	mov	BCD16Output2,BB16_Count2
	BB16p5_L10k:
		inc		BCD16Output2
		subi	ResultValueLTemp,low(10000)
		sbci	ResultValueHTemp,high(10000)
		brcc	BB16p5_L10k
		subi	ResultValueLTemp,low(-10000)
		sbci	ResultValueHTemp,high(-10000)
		ldi		BB16_Count1,(256-16)
	BB16p5_L1k:
		subi	BB16_Count1,(-16)
		subi	ResultValueLTemp,low(1000)
		sbci	ResultValueHTemp,high(1000)
		brcc	BB16p5_L1k
		subi	ResultValueLTemp,low(-1000)
		sbci	ResultValueHTemp,high(-1000)
		mov		BCD16Output1,BB16_Count2
	BB16p5_L100:
		inc		BCD16Output1
		subi	ResultValueLTemp,low(100)
		sbci	ResultValueHTemp,high(100)
		brcc	BB16p5_L100
		subi	ResultValueLTemp,low(-100)
		or		BCD16Output1,BB16_Count1
		ldi		BB16_Count1,(256-16)
	BB16p5_L10:
		subi	BB16_Count1,(-16)
		subi	ResultValueLTemp,10
		brcc	BB16p5_L10
		subi	ResultValueLTemp,-10
		mov		BCD16Output0,BB16_Count1
		or		BCD16Output0,ResultValueLTemp
	ret

// ==================== External Libraries and Functions : End ====================