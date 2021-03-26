.global print
.global println
.global printint
.global printintln
.global newline

.global output # same as print but on stdout, meant for binary compilation output

.global nextch

.global quit

.global this_char
.global prev_char

.global charnum
.global linum

.text

nextch: # save the previous char, load the next char, fix linum/charnum based on previous char being \n or not
	lea this_char(%rip), %rax
	movb (%rax), %al
	lea prev_char(%rip), %rdi
	movb %al, (%rdi)

	xor %rax, %rax # read
	xor %rdi, %rdi # stdin
	lea this_char(%rip), %rsi # what
	xor %rdx, %rdx # how much
	inc %rdx
	syscall
	test %rax, %rax
	jne nextch_next
	syscall # Do it again to check for next line...
	test %rax, %rax # Still empty, probably real EOF
	je nextch_eof

	nextch_eof:
	movb $3, this_char(%rip) # 3 = end of text

	nextch_next:
	incq charnum(%rip)

	lea char_type_tbl(%rip), %ebx
	movb prev_char(%rip), %al
	xlatb

	cmpb ct_nl(%rip), %al
	jne ef_next

	incq linum(%rip)
	movq $-1, charnum(%rip)

	ef_next:
	ret

output: # expects 'what' on %rsi and "how much" on %rdx, outputs to stdout
	xor %rax, %rax # write
	inc %rax
	xor %rdi, %rdi # stdout
	inc %rdi
	syscall
	ret

print: # expects 'what' on %rsi and "how much" on %rdx, outputs to stderr
	xor %rax, %rax # write
	inc %rax
	xor %rdi, %rdi # stderr
	inc %rdi
	inc %rdi
	syscall
	ret

printint:
	mov %rsi, %rax
	xor %rcx, %rcx

	printint_loop:
		xor %rdi, %rdi
		xor %rdx, %rdx
		divq ten(%rip)
		test %rax, %rax
		jnz printint_showchar
		inc %rdi
		#jmp printint_showchar

		printint_showchar:
			lea 48(%rdx), %rdx
			dec %rsp
			inc %rcx
			movb %dl, (%rsp) # 48 = '0'
			test %rdi, %rdi
			je printint_loop

	mov %rsp, %rsi
	mov %rcx, %rdx
	push %rcx
	call print
	pop %rcx

	add %rcx, %rsp

	ret

newline:
	lea nl(%rip), %rsi
	xor %rdx, %rdx
	inc %rdx
	xor %rax, %rax
	inc %rax
	xor %rdi, %rdi
	inc %rdi
	inc %rdi
	syscall

	ret

printintln:
	call printint
	call newline
	ret

println: # print, but add \n
	call print
	call newline
	ret

quit: # expects the error code on %rdi
	mov $60, %rax
	syscall
	ret

.data
prev_char: .ascii "\0"
this_char: .ascii "\0"

charnum: .quad -2
linum: .quad 0

test_msg: .ascii "Hello, world"
nl: .ascii "\n"
test_msg_lgt = . - test_msg
space: .ascii " "

ten: .quad 10

.include "arp0_chars.s"
