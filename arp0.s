.global _start

.text

_start:
	call nextch

	start_wsloop:
		lea char_class_tbl(%rip), %ebx
		movb this_char(%rip), %al
		xlatb
		cmpb cc_white(%rip), %al
		je start_wscont
		cmpb cc_nl(%rip), %al
		jne start_wsdone
		start_wscont:
		call nextch
		jmp start_wsloop
	
	start_wsdone:
	call nextch

	movb form(%rip), %al
	call expect

	xor %rdi, %rdi # code 0
	call quit

expect:
	push %rax

	call accept
	test %rax, %rax
	jne expect_die
	pop %rax
	ret

	expect_die:
		cmp $2, %rax
		je unrec_error
		pop %rax
		jmp expect_error
		unrec_error:
			movsx unrec_err_code(%rip), %rax
			jmp expect_error

accept:
	lea (,%rax,8), %rbx

	mov charnum(%rip), %rax
	mov %rax, last_charnum(%rip)
	mov linum(%rip), %rax
	mov %rax, last_linum(%rip)

	lea accept_tbl(%rip), %rax
	add %rax, %rbx
	jmp *(%rbx)

accept_var: # A var is a sequence of printables. Numbers are allowed only in 2nd position or later.
	xor %r12, %r12
	lea accept_buff(%rip), %r13
	xor %r14, %r14
	inc %r14
	movb $0, accept_lgt(%rip)
	xor %rcx, %rcx
	dec %cl # max 255 chars

	ae_loop:
		lea char_class_tbl(%rip), %ebx
		movb prev_char(%rip), %al
		xlatb

		cmpb cc_lowchar(%rip), %al
		je ae_accept

		cmpb cc_highchar(%rip), %al
		je ae_accept

		cmpb cc_symb(%rip), %al
		je ae_accept

		test %r12, %r12
		je ae_no_num # num not allowed in position 0

		cmpb cc_num(%rip), %al
		je ae_accept

		ae_no_num:
			#jmp ae_reject

		ae_reject:
			jmp ae_end

		ae_accept:
			mov prev_char(%rip), %r14
			movb %r14b, (%r13)
			xor %r14, %r14
			inc %r13
			inc %r12
			push %rcx
			call nextch
			pop %rcx
			loop ae_loop
		
	ae_end:
		movb %r12b, accept_lgt(%rip)
		mov %r14, %rax
		ret

noaccept:
	lea noaccept_error(%rip), %rsi
	mov $noaccept_error_lgt, %rdx
	call println

	mov $2, %rdi
	call quit

	ret

accept_form: # A form is a non-empty s-expression whose head is either a special-form (define) or a variable.
	lea char_class_tbl(%rip), %ebx
	movb prev_char(%rip), %al
	xlatb

	cmpb cc_opar(%rip), %al
	jne ef_incomplete

	ef_wsloop1:
		lea char_class_tbl(%rip), %ebx
		movb this_char(%rip), %al
		xlatb
		cmpb cc_white(%rip), %al
		je ef_wscont1
		cmpb cc_nl(%rip), %al
		jne ef_wsdone1
		ef_wscont1:
		call nextch
		jmp ef_wsloop1
	
	ef_wsdone1:

	call nextch
	xor %rax, %rax
	movb var(%rip), %al
	call accept
	test %rax, %rax
	jne ef_incomplete

	cld
	movsx accept_lgt(%rip), %ecx
	mov $define_str_lgt, %rdx
	cmp %rcx, %rdx
	jne ef_nodef
	lea accept_buff(%rip), %rsi
	lea define_str(%rip), %rdi
	repe cmpsb
	jz ef_def

	ef_nodef:
	mov n_symbols(%rip), %rcx

	ef_callloop:
		mov n_symbols(%rip), %rdx # Load index from 0 into rdx (rcx counts down from the top)
		sub %rcx, %rdx
		push %rcx
		lea symbol_tbl(%rip), %rdi # Load symbol table
		lea (%rdx,%rdx,2), %rcx # %rdx + 2*%rdx
		lea (%rdi,%rcx,8), %rcx # %rdi + 8*(3*%rdx) = %rdi + 24*%rdx
		#lea 0(%rdi), %rdi # [key] is first; no offset
		mov 8(%rdi), %rbx # [length]
		movsx accept_lgt(%rip), %ecx
		cmp %rbx, %rcx
		jne ef_cl_tail

		mov (%rdi), %rdi # This points to a cell: [key, key-length, code address], from which we want [key]
		lea accept_buff(%rip), %rsi
		repe cmpsb
		jz ef_done_cl

		ef_cl_tail:
		pop %rcx
		dec %rcx
		test %rcx, %rcx
		jne ef_callloop
		jmp ef_after_cl

	ef_done_cl:
	pop %rcx
	ef_after_cl:
	test %rcx, %rcx
	je ef_unrecognized

	# Do processing for fn call here
	movsx accept_lgt(%rip), %edx
	lea accept_buff(%rip), %rsi
	call println # stub
	jmp ef_call

	ef_def: # handle 'define' special form
		movsx accept_lgt(%rip), %edx
		lea accept_buff(%rip), %rsi
		call println # stub

		jmp ef_call # stub
	
	ef_call:
		xor %r12, %r12
		xor %rcx, %rcx
		mov $8, %cl

	ef_loop:
		lea char_class_tbl(%rip), %ebx
		movb prev_char(%rip), %al
		xlatb

		cmpb cc_cpar(%rip), %al
		je ef_end

		ef_wsloop2:
			lea char_class_tbl(%rip), %ebx
			movb this_char(%rip), %al
			xlatb
			cmpb cc_white(%rip), %al
			je ef_wscont2
			cmpb cc_nl(%rip), %al
			jne ef_wsdone2
			ef_wscont2:
			call nextch
			jmp ef_wsloop2
		
		ef_wsdone2:
		call nextch
		lea char_class_tbl(%rip), %ebx
		movb prev_char(%rip), %al
		xlatb

		cmpb cc_cpar(%rip), %al
		je ef_end

		xor %rax, %rax
		movb var(%rip), %al
		push %rcx
		call accept

		test %rax, %rax # success
		je ef_cont

		pop %rcx
		jmp ef_incomplete

		ef_cont:
		lea accept_buff(%rip), %rsi
		movb accept_lgt(%rip), %dl
		call println
		pop %rcx

		loop ef_loop

	ef_end:
		#sub $8, %rcx
		#jl ef_incomplete # There were no args in the call
		xor %rax, %rax
		jmp ef_ret

	ef_incomplete:
		xor %rax, %rax
		inc %rax
		jmp ef_ret

	ef_unrecognized:
		xor %rax, %rax
		inc %rax
		inc %rax
		#jmp ef_ret

	ef_ret:
		ret

# error message.
expect_error:
	push %rax
	mov last_linum(%rip), %rsi
	call printint
	lea comma_space(%rip), %rsi
	xor %rdx, %rdx
	inc %rdx
	inc %rdx
	call print
	mov last_charnum(%rip), %rsi
	call printint
	lea colon_space(%rip), %rsi
	xor %rdx, %rdx
	inc %rdx
	inc %rdx
	call print
	pop %rax
	lea expect_error_tbl(%rip), %rsi
	lea (%rsi,%rax,8), %rsi
	mov (%rsi), %rsi

	lea expect_error_lgt_tbl(%rip), %rdx
	lea (%rdx,%rax,8), %rdx
	mov (%rdx), %rdx
	call println

	xor %rdi, %rdi
	inc %rdi
	call quit

	ret

.data
expect_type_tbl:
form: .byte 0
var: .byte 1

noaccept_error: .ascii "Compiler error -- no accept function for current token."
noaccept_error_lgt = . - noaccept_error

accept_tbl:
.quad accept_form
.quad accept_var

expect_form_error: .ascii "Missing 'form' expected"
expect_form_error_lgt = . - expect_form_error
expect_var_error: .ascii "Missing 'var' expected"
expect_var_error_lgt = . - expect_var_error
unrecognized_error: .ascii "Unrecognized input"
unrecognized_error_lgt = . - unrecognized_error

unrec_err_code: .byte 2

expect_error_tbl:
.quad expect_form_error
.quad expect_var_error
.quad unrecognized_error

expect_error_lgt_tbl:
.quad expect_form_error_lgt
.quad expect_var_error_lgt
.quad unrecognized_error_lgt

accept_buff: .space 256, 0
accept_lgt: .byte 0

define_str: .ascii "define"
define_str_lgt = . - define_str
print_str: .ascii "print"
print_str_lgt = . - print_str
newline_str: .ascii "newline"
newline_str_lgt = . - newline_str
SYMBOL_TBL_END: .ascii "\0"

comma_space: .ascii ", "
colon_space: .ascii ": "

print_code: .quad 1 # size
.byte 0x90 # stub = NOP
newline_code: .quad 1
.byte 0x90 # stub = NOP
NULL_CODE: .quad 0

n_symbols: .quad 3
symbol_tbl:
.quad print_str, print_str_lgt, print_code
.quad newline_str, newline_str_lgt, newline_code
.quad SYMBOL_TBL_END, 0, NULL_CODE

last_charnum: .quad 0
last_linum: .quad 0
