

main:
	ldi r16, $20
	call hasti
	rjmp main

hasti:
ldi r17, 5
	ret
