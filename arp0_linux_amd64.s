.global print
.global println
.global printint
.global printintln
.global newline

.global output # same as print but on stdout, meant for binary compilation output

.global nextch

.global my_malloc
#.global free Not actually necessary for bootstrap

.global quit

.global this_char
.global prev_char

.global charnum
.global linum

.text

nextch: # save the previous char, load the next char, fix linum/charnum based on previous char being \n or not. rsi=1 means skip to eol.
	push %rsi
	xor %rsi, %rsi

	nextch_main:
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
	pop %rsi
	test %rax, %rax # 0 bytes read: EOF?
	jne nextch_next

	movb $3, this_char(%rip) # 3 = end of text

	nextch_next:
	incq charnum(%rip)

	test %rsi, %rsi
	je nc_cont

	lea char_type_tbl(%rip), %rbx
	movsx this_char(%rip), %rax
	movb (%rbx,%rax), %al

	cmpb ct_nl(%rip), %al
	jne nc_end

	xor %rsi, %rsi

	nc_cont:
	lea char_type_tbl(%rip), %rbx
	movsx prev_char(%rip), %rax
	movb (%rbx,%rax), %al

	cmpb ct_nl(%rip), %al
	jne nc_next

	incq linum(%rip)
	movq $0, charnum(%rip)

	nc_next:
	lea char_type_tbl(%rip), %rbx
	movsx this_char(%rip), %rax # use this_char since we want to conserve the pre-comment char
	movb (%rbx,%rax), %al
	cmpb ct_comment(%rip), %al
	jne nc_end

	xor %rsi, %rsi
	inc %rsi

	nc_end:
	test %rsi, %rsi
	jne nextch_main
	pop %rsi
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

my_malloc: # Expects allocation amount in bytes on %rsi; bytes obtained returned in %rax
	mov $0x22, %r10 # MAP_ANONYMOUS | MAP_PRIVATE
	xor %r8, %r8 # fd = 0
	xor %r9, %r9 # offset = 0
	xor %rdi, %rdi # no ptr input
	mov $0x2, %rdx # read-write
	mov $9, %rax
	syscall
	mov %rax, %rdi
	ret

.data
prev_char: .ascii "\0"
this_char: .ascii "\0"

charnum: .quad 0
linum: .quad 0

test_msg: .ascii "Hello, world"
nl: .ascii "\n"
test_msg_lgt = . - test_msg
space: .ascii " "

ten: .quad 10
heap_start: .quad 0

.include "arp0_chars.s"
