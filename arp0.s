.global _start

.text

_start:
	xor %rsi, %rsi
	call nextch

	call skip_ws

	xor %rsi, %rsi
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

skip_ws:
	sw_loop:
		lea char_class_tbl(%rip), %ebx
		movb this_char(%rip), %al
		xlatb
		cmpb cc_white(%rip), %al
		jne sw_done

		xor %rsi, %rsi
		call nextch
		jmp sw_loop
	
	sw_done:
	ret

accept_string: # A string is squote-delimited. prevchar should be the string-quote (squote) character.
	lea char_type_tbl(%rip), %ebx
	movb prev_char(%rip), %al
	xlatb

	cmpb ct_squote, %al
	jne as_reject

	xor %rcx, %rcx
	dec %cl # max 255 chars

	lea accept_buff(%rip), %r12
	movb $0, accept_lgt(%rip)

	as_loop:
		push %rcx
		xor %rsi, %rsi
		call nextch

		lea char_class_tbl(%rip), %ebx
		movb prev_char(%rip), %al
		xlatb

		cmpb cc_symb(%rip), %al
		jne as_accept

		lea char_type_tbl(%rip), %ebx
		movb prev_char(%rip), %al
		xlatb

		cmpb ct_squote(%rip), %al
		jmp as_done

		cmpb ct_sesc(%rip), %al
		jne as_accept

		movb this_char(%rip), %al
		xlatb
		cmpb ct_sesc(%rip), %al
		jne as_esc_squote

		xor %rsi, %rsi
		call nextch
		movb $'\\', prev_char(%rip)
		jmp as_accept

		as_esc_squote:
		cmpb ct_squote(%rip), %al
		jne as_esc_nl

		xor %rsi, %rsi
		call nextch
		movb $'"', prev_char(%rip)
		jmp as_accept

		as_esc_nl:
		movb this_char(%rip), %al
		cmpb $'n', %al
		jne as_esc_tab

		xor %rsi, %rsi
		call nextch
		movb $'\n', prev_char(%rip)
		jmp as_accept

		as_esc_tab:
		cmpb $'t', %al
		jne as_esc_nul

		xor %rsi, %rsi
		call nextch
		movb $'\t', prev_char(%rip)
		jmp as_accept

		as_esc_nul:
		cmpb $'0', %al
		jne as_esc_squote

		xor %rsi, %rsi
		call nextch
		movb $'\0', prev_char(%rip)
		jmp as_accept

		jmp as_reject

		as_accept:
			movb prev_char(%rip), %al
			movb %al, (%r12)
			inc %r12
			incb accept_lgt(%rip)
			pop %rcx
			dec %rcx
			test %rcx, %rcx
			je as_reject
			jmp as_loop

	as_reject:
		xor %rax, %rax
		inc %rax
		ret

	as_done: # only happens when final squote is found, so always good
		pop %rcx
		xor %rsi, %rsi
		call nextch # skip over final squote
		xor %rax, %rax
		ret

accept_int:
	mov $20, %rcx # max base-10 size of 64-bit ints
	lea accept_buff(%rip), %r12
	movb $0, accept_lgt(%rip)

	lea char_class_tbl(%rip), %ebx
	movb prev_char(%rip), %al
	xlatb

	cmpb cc_num(%rip), %al
	jne ai_reject
	jmp ai_accept

	ai_loop:
		xor %rsi, %rsi
		call nextch
		lea char_class_tbl(%rip), %ebx
		movb prev_char(%rip), %al
		xlatb

		cmpb cc_num(%rip), %al
		je ai_accept
		jmp ai_done

		ai_accept:
			movb prev_char(%rip), %al
			movb %al, (%r12)
			inc %r12
			incb accept_lgt(%rip)
			jmp ai_loop

	ai_done:
		xor %rax, %rax
		cmpb $0, accept_lgt(%rip)
		jne ai_done_done
		inc %rax

		ai_done_done:
		ret

	ai_reject:
		xor %rax, %rax
		inc %rax
		ret

accept_float:
	movb $0, float_lgt1(%rip)
	movb $0, float_lgt2(%rip)

	movsx int(%rip), %rax
	call accept

	test %rax, %rax
	jne af_reject

	movsx accept_lgt(%rip), %ecx
	movb %cl, float_lgt1(%rip)
	lea accept_buff(%rip), %rsi
	lea float_buff1(%rip), %rdi
	rep movsb

	movb prev_char(%rip), %al
	cmpb $'.', %al
	jne af_reject

	xor %rsi, %rsi
	call nextch
	movsx int(%rip), %rax
	call accept

	test %rax, %rax
	jne af_reject

	movb prev_char(%rip), %al
	lea char_class_tbl(%rip), %ebx
	xlatb

	cmpb cc_char(%rip), %al
	je af_reject

	cmpb cc_symb(%rip), %al
	jne af_accept # -> it's whitespace or control

	movb prev_char(%rip), %al
	lea char_type_tbl(%rip), %ebx
	xlatb

	cmpb ct_cpar(%rip), %al
	jne af_reject

	movsx accept_lgt(%rip), %ecx
	movb %cl, float_lgt2(%rip)
	lea accept_buff(%rip), %rsi
	lea float_buff2(%rip), %rdi
	rep movsb
	#jmp af_accept

	af_accept:
		xor %rax, %rax
		ret

	af_reject:
		xor %rax, %rax
		inc %rax
		ret

accept_literal:
	movb prev_char(%rip), %al
	lea char_type_tbl(%rip), %ebx
	xlatb

	cmpb ct_squote(%rip), %al
	je al_string

	lea char_class_tbl(%rip), %ebx
	movb prev_char(%rip), %al
	xlatb

	cmpb cc_num(%rip), %al
	je al_int_or_float

	jmp al_reject

	al_string:
		movsx string(%rip), %rax
		call expect
		xor %rax, %rax
		ret

	al_int_or_float:
		movsx float(%rip), %rax
		call accept

		test %rax, %rax
		je al_float

		movsx float_lgt1(%rip), %rax
		test %rax, %rax
		je al_reject
		jmp al_int

	al_float:
		# TODO: do a thing  with the float in the float_buffs
		jmp al_accept

	al_int:
		# TODO: do a thing with the int in the accept_buff
		jmp al_accept

	al_reject:
		xor %rax, %rax
		inc %rax
		ret

	al_accept:
		xor %rax, %rax
		ret

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

		cmpb cc_char(%rip), %al
		je ae_accept

		cmpb cc_symb(%rip), %al
		je ae_checksymbs

		test %r12, %r12
		je ae_no_num # num not allowed in position 0

		cmpb cc_num(%rip), %al
		je ae_accept

		jmp ae_reject

		ae_checksymbs:
			lea char_type_tbl(%rip), %ebx
			mov prev_char(%rip), %al
			xlatb

			cmpb ct_none, %al
			je ae_accept
			#jmp ae_reject

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
			xor %rsi, %rsi
			call nextch
			pop %rcx
			loop ae_loop
		
	ae_end:
		movb %r12b, accept_lgt(%rip)
		mov %r14, %rax
		ret

accept_atom:
	movb prev_char(%rip), %al
	lea char_class_tbl(%rip), %ebx
	xlatb

	cmpb cc_char(%rip), %al
	je aa_var

	movsx literal(%rip), %rax
	call accept

	test %rax, %rax
	jne aa_reject
	jmp aa_accept

	aa_var:
		movsx var(%rip), %rax
		call accept

		test %rax, %rax
		jne aa_reject
		jmp aa_accept

	aa_reject:
		xor %rax, %rax
		inc %rax
		ret
	
	aa_accept:
		xor %rax, %rax
		ret

accept_expr:
	movb prev_char(%rip), %al
	lea char_type_tbl(%rip), %ebx
	xlatb

	cmpb ct_opar(%rip), %al
	je ae2_tryform

	ae2_tryatom:
		movsx atom(%rip), %rax
		jmp ae2_after

	ae2_tryform:
		movsx form(%rip), %rax

	ae2_after:
		jmp accept
		#ret # Return whatever the returned error code was (via %rax)


accept_form: # A form is a non-empty s-expression whose head is either a special-form (define) or a variable.
	lea char_type_tbl(%rip), %ebx
	movb prev_char(%rip), %al
	xlatb

	cmpb ct_opar(%rip), %al
	jne ef_incomplete

	call skip_ws

	xor %rsi, %rsi
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
		lea (%rdx,%rdx,2), %rcx # 4 %rdx
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
		call skip_ws
		xor %rsi, %rsi
		call nextch

		movsx var(%rip), %rax
		call accept
		test %rax, %rax
		jne ef_incomplete

		mov n_symbols(%rip), %r12
		inc %r12
		mov %r12, n_symbols(%rip)

		movsx accept_lgt(%rip), %ecx
		push %rcx

		# malloc new str
		mov %rcx, %rsi
		call malloc

		test %rax, %rax
		jz err_malloc_failed
		# mov %rdi, %rdi
		mov %rdi, %r13

		pop %rcx

		lea accept_buff(%rip), %rsi

		# copy str to new memory location
		rep movsb

		lea symbol_tbl(%rip), %rdi
		lea (%r12,%r12,2), %r12
		lea (%rdi,%r12,8), %rdi
		push %rdi # Save it for when we generate the code (it shall be put in 16(%rdi))
		movq %rcx, 8(%rdi)
		# save str ptr in symbol_tbl
		movq %r13, (%rdi)

		call skip_ws
		xor %rsi, %rsi
		call nextch

		movsx expr(%rip), %rax
		call accept

		test %rax, %rax
		jne ef_incomplete
		mov last_ip(%rip), %rsi
		pop %rdi
		mov %rsi, 16(%rdi)

		call skip_ws
		xor %rsi, %rsi
		call nextch
		lea char_type_tbl(%rip), %ebx
		movb this_char(%rip), %al
		xlatb

		cmpb ct_cpar(%rip), %al
		jne ef_incomplete

		xor %rax, %rax
		ret

		#call println # stub

	ef_call:
		xor %rcx, %rcx
		mov $8, %cl

	ef_loop:
		lea char_type_tbl(%rip), %ebx
		movb prev_char(%rip), %al
		xlatb

		cmpb ct_cpar(%rip), %al
		je ef_end

		call skip_ws

		xor %rsi, %rsi
		call nextch
		lea char_type_tbl(%rip), %ebx
		movb prev_char(%rip), %al
		xlatb

		cmpb ct_cpar(%rip), %al
		je ef_end

		xor %rax, %rax
		movsx expr(%rip), %rax
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

noaccept:
	lea noaccept_error(%rip), %rsi
	mov $noaccept_error_lgt, %rdx
	call println

	mov $2, %rdi
	call quit

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

err_malloc_failed:
	lea malloc_error(%rip), %rsi
	lea malloc_error_lgt(%rip), %rdx
	call println

	xor %rdi, %rdi
	dec %rdi
	call quit

newline_code:
print_code:
	dec %rsp
	movb $0x90, (%rsp)
	mov %rsp, %rsi
	xor %rdx, %rdx
	inc %rdx
	call output
	inc %rsp
	ret

NULL_CODE:
	ret

.data
expect_type_tbl:
form: .byte 0
var: .byte 1
string: .byte 2
int: .byte 3
float: .byte 4
literal: .byte 5
atom: .byte 6
expr: .byte 7

noaccept_error: .ascii "Compiler error -- no accept function for current token."
noaccept_error_lgt = . - noaccept_error

accept_tbl:
.quad accept_form
.quad accept_var
.quad accept_string
.quad accept_int
.quad accept_float
.quad accept_literal
.quad accept_atom
.quad accept_expr

malloc_error: .ascii "FATAL: malloc failed"
malloc_error_lgt = . - malloc_error

expect_form_error: .ascii "Missing 'form' expected"
expect_form_error_lgt = . - expect_form_error
expect_var_error: .ascii "Missing 'var' expected"
expect_var_error_lgt = . - expect_var_error
malformed_string_error: .ascii "Malformed 'string'"
malformed_string_error_lgt = . - malformed_string_error
bad_int_error: .ascii "Bad 'integer'"
bad_int_error_lgt = . - bad_int_error
bad_float_error: .ascii "Bad 'float'"
bad_float_error_lgt = . - bad_float_error
expect_literal_error: .ascii "Missing 'literal' expected"
expect_literal_error_lgt = . - expect_literal_error
expect_atom_error: .ascii "Missing 'atom' expected"
expect_atom_error_lgt = . - expect_atom_error
expect_expr_error: .ascii "Missing 'expr' expected"
expect_expr_error_lgt = . - expect_expr_error
unrecognized_error: .ascii "Unrecognized input"
unrecognized_error_lgt = . - unrecognized_error

unrec_err_code: .byte 8

expect_error_tbl:
.quad expect_form_error
.quad expect_var_error
.quad malformed_string_error
.quad bad_int_error
.quad bad_float_error
.quad expect_literal_error
.quad expect_atom_error
.quad expect_expr_error
.quad unrecognized_error

expect_error_lgt_tbl:
.quad expect_form_error_lgt
.quad expect_var_error_lgt
.quad malformed_string_error_lgt
.quad bad_int_error_lgt
.quad bad_float_error_lgt
.quad expect_literal_error_lgt
.quad expect_atom_error_lgt
.quad expect_expr_error_lgt
.quad unrecognized_error_lgt

accept_buff: .space 256, 0
accept_lgt: .byte 0

# Buffers for float decoding (int part, then decimal part)
float_buff1: .space 20, 0
float_lgt1: .byte 0
float_buff2: .space 20, 0
float_lgt2: .byte 0

define_str: .ascii "define"
define_str_lgt = . - define_str
print_str: .ascii "print"
print_str_lgt = . - print_str
newline_str: .ascii "newline"
newline_str_lgt = . - newline_str
SYMBOL_TBL_END: .ascii "\0"

comma_space: .ascii ", "
colon_space: .ascii ": "

last_ip: .quad 0 # instruction pointer starting at previous insertion point
ip: .quad 0 # instruction pointer at the end of previous insertion

n_symbols: .quad 3

symbol_tbl:
.quad print_str, print_str_lgt, print_code
.quad newline_str, newline_str_lgt, newline_code
.space 65536, 0
.quad SYMBOL_TBL_END, 0, NULL_CODE

program:
.space 65536, 0 # Enough space for the bootstrap program

last_charnum: .quad 0
last_linum: .quad 0
