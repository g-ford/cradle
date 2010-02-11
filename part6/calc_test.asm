extern _printf

section .data
    msg1	db	"Result: %i",0xa
    
section	.text		; declaring our .text segment
	global	_main 	; telling where program execution should start
    
_main: 		; this is where code starts getting exec'ed
	push    ebp		; set up stack frame
	mov     ebp,esp
	
	; add the calculation below here
    
    ; print the result
    PUSH eax
    PUSH dword msg1
    call _printf
	add esp, 8 ; clear the params off the stack
	
	mov	eax,0		;  normal, no error, return value
	leave
	ret			; return