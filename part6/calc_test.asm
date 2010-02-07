extern _printf

section .data
    msg1	db	"Result: %i",0xa
    
section	.text		; declaring our .text segment
	global	_main 	; telling where program execution should start
    
_main: 		; this is where code starts getting exec'ed
	pop	ebx	; get first thing off of stack and put into ebx
	dec	ebx	; decrement the value of ebx by one
	pop	ebp	; get next 2 things off stack and put into ebx
	pop	ebp
	
	; add the calculation below here

    
    ; print the result
    PUSH eax
    PUSH dword msg1
    call _printf
	
	; exit 
	mov	ebx,0		; have an exit code of 0
	mov	eax,1		; the exit syscall number
	int	80h		    ; interrupt 80h, the thing that pokes the kernel
    				; and says, "do this"