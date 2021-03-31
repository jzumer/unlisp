.global print
.global println
.global printint
.global printintln
.global newline

.global output # same as print but on stdout, meant for binary compilation output

.global nextch

.global malloc
#.global free Not actually necessary for bootstrap

.global quit

.global this_char
.global prev_char

.global charnum
.global linum

.text

nextch: # save the previous char, load the next char, fix linum/charnum based on previous char being \n or not. rsi=1 means skip to eol.
	push %rsi
	test %rsi, %rsi
	jnz skip_flip

	lea this_char(%rip), %rax
	movb (%rax), %al
	lea prev_char(%rip), %rdi
	movb %al, (%rdi)

	skip_flip: # Do not push the previous char in the current slot if this is a comment string,
	# just keep skipping chars so as to put the first non-comment char right after the current char.
	xor %rax, %rax # read
	xor %rdi, %rdi # stdin
	lea this_char(%rip), %rsi # what
	xor %rdx, %rdx # how much
	inc %rdx
	syscall
	test %rax, %rax # 0 bytes read: EOF?
	jne nextch_next
	syscall # Do it again to check for next line...
	test %rax, %rax # Still empty, probably real EOF
	jne nextch_next

	movb $3, this_char(%rip) # 3 = end of text

	nextch_next:
	pop %rsi
	incq charnum(%rip)

	lea char_type_tbl(%rip), %ebx
	movb prev_char(%rip), %al
	xlatb

	cmpb ct_nl(%rip), %al
	jne ef_next

	incq linum(%rip)
	movq $-1, charnum(%rip)

	xor %rsi, %rsi

	ef_next:
	cmpb ct_comment(%rip), %al
	jne ef_end

	xor %rsi, %rsi
	inc %rsi

	ef_end:
	test %rsi, %rsi
	jne nextch
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

malloc: # Expects allocation amount in bytes on %rsi; bytes obtained returned in %rax, memstart in %rsi
	mov heap_start(%rip), %rax
	test %rax, %rax
	jnz m_alloc_more

	mov $12, %rax
	xor %rdi, %rdi
	syscall
	mov %rax, heap_start(%rip)

	m_alloc_more:
		mov $12, %rax
		mov %rsi, %rdi
		add heap_start(%rip), %rdi
		syscall

		mov %rax, %rsi
		sub heap_start(%rip), %rax
		add %rax, heap_start(%rip) # We don't dealloc in this program so no extra bookkeeping needed

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
heap_start: .quad 0

.include "arp0_chars.s"
