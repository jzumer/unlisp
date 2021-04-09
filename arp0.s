# Special notes: due to endianness, movw 0xc0ff, * is the correct sequence for inc %rbx, not ffc0.
# Note2: xlatb can't be used anymore because the addresses of the tables are too far down in the program...
.global _start
.global form_ptr

.text

_start:
	xor %rsi, %rsi
	call nextch

	call skip_ws

	xor %rsi, %rsi
	call nextch

	movb form(%rip), %al
	call expect

	main_loop:
	call skip_ws

	movb this_char(%rip), %al
	cmpb $3, %al # EOT
	je main_end

	xor %rsi, %rsi
	call nextch

	movb form(%rip), %al
	call expect
	jmp main_loop

	main_end:
	call make_quit

	call make_header
	call make_footer
	call make_relocations

	movq header_ptr(%rip), %rdx
	lea header(%rip), %rsi

	call output # print out the header

	movq ip(%rip), %rdx
	lea program(%rip), %rsi

	call output # print out the program segment

	movq dp(%rip), %rdx
	lea data(%rip), %rsi

	call output # print out the data segment

	movq footer_ptr(%rip), %rdx
	lea footer(%rip), %rsi

	call output # print out the footer

	xor %rdi, %rdi # all good, code 0
	call quit

make_quit:
	# insert exit(0) at end of program
	# xor %rdi, %rdi
	# mov $60, %rax
	# syscall
	lea program(%rip), %rax
	addq ip(%rip), %rax
	movl $0x48ff3148, (%rax)
	movl $0x003cc0c7, 4(%rax)
	movl $0x050f0000, 8(%rax)
	addq $12, ip(%rip)
	ret

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
		lea char_class_tbl(%rip), %rbx
		movsx this_char(%rip), %rax
		movb (%rbx,%rax), %al
		cmpb cc_white(%rip), %al
		jne sw_done

		xor %rsi, %rsi
		call nextch
		jmp sw_loop
	
	sw_done:
	ret

accept_string: # A string is squote-delimited. prevchar should be the string-quote (squote) character.
	lea char_type_tbl(%rip), %rbx
	movsx prev_char(%rip), %rax
	movb (%rbx,%rax), %al

	cmpb ct_squote(%rip), %al
	jne as_reject

	xor %rcx, %rcx
	dec %cl # max 255 chars

	lea accept_buff(%rip), %r12
	movb $0, accept_lgt(%rip)

	as_loop:
		push %rcx
		xor %rsi, %rsi
		call nextch

		lea char_class_tbl(%rip), %rbx
		movsx prev_char(%rip), %rax
		movb (%rbx,%rax), %al

		cmpb cc_symb(%rip), %al
		jne as_accept

		lea char_type_tbl(%rip), %rbx
		movsx prev_char(%rip), %rax
		movb (%rbx,%rax), %al
		

		cmpb ct_squote(%rip), %al
		je as_done

		cmpb ct_sesc(%rip), %al
		jne as_accept

		lea char_type_tbl(%rip), %rbx
		movsx this_char(%rip), %rax
		movb (%rbx,%rax), %al

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

	lea char_class_tbl(%rip), %rbx
	movsx prev_char(%rip), %rax
	movb (%rbx,%rax), %al

	cmpb cc_num(%rip), %al
	jne ai_reject
	jmp ai_accept

	ai_loop:
		xor %rsi, %rsi
		call nextch
		lea char_class_tbl(%rip), %rbx
		movsx prev_char(%rip), %rax
		movb (%rbx,%rax), %al

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

accept_literal:
	movsx prev_char(%rip), %rax
	lea char_type_tbl(%rip), %rbx
	movb (%rbx,%rax), %al

	cmpb ct_squote(%rip), %al
	je al_string

	lea char_class_tbl(%rip), %rbx
	movsx prev_char(%rip), %rax
	movb (%rbx,%rax), %al

	cmpb cc_num(%rip), %al
	je al_int

	jmp al_reject

	al_string:
		movsx string(%rip), %rax
		call expect

		# Copy string into program verbatim
		movsx accept_lgt(%rip), %ecx
		mov dp(%rip), %rax
		mov %rax, prev_dp(%rip)
		lea accept_buff(%rip), %rsi
		lea data(%rip), %rdi
		add dp(%rip), %rdi
		mov %rcx, (%rdi)
		add $8, %rdi # start after the str length
		rep movsb

		addb accept_lgt(%rip), %al
		add $8, %rax
		mov %rax, dp(%rip)

		mov prev_dp(%rip), %rsi
		xor %rax, %rax
		ret

	al_int:
		movsx int(%rip), %rax
		call expect

		movsx accept_lgt(%rip), %ecx
		lea accept_buff(%rip), %rsi
		xor %rbx, %rbx
		xor %rdx, %rdx

		al_ins_int:
			movsx (%rsi), %ebx
			sub $'0', %bl
			mulq ten(%rip)
			add %rbx, %rax
			inc %rsi
			loop al_ins_int

		mov dp(%rip), %rbx
		mov %rbx, prev_dp(%rip)
		mov %rbx, %rcx
		lea data(%rip), %rbx
		add %rcx, %rbx
		movq %rax, (%rbx)
		add $8, %rbx
		sub data(%rip), %rbx
		mov %rbx, dp(%rip)
		mov prev_dp(%rip), %rsi

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
		lea char_class_tbl(%rip), %rbx
		movsx prev_char(%rip), %rax
		movb (%rbx,%rax), %al

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
			lea char_type_tbl(%rip), %rbx
			movsx prev_char(%rip), %rax
			movb (%rbx,%rax), %al

			cmpb ct_none(%rip), %al
			je ae_accept
			#jmp ae_reject

		ae_no_num:
			#jmp ae_reject

		ae_reject:
			jmp ae_end

		ae_accept:
			mov prev_char(%rip), %r14
			movb %r14b, (%r13)
			inc %r13
			inc %r12
			push %rcx
			push %r12
			push %r13
			xor %rsi, %rsi
			call nextch
			xor %r14, %r14
			pop %r13
			pop %r12
			pop %rcx
			loop ae_loop
		
	ae_end:
		movb %r12b, accept_lgt(%rip)
		mov %r14, %rax
		test %rax, %rax
		jnz ae_ret
		call find_symbol

		ae_ret:
		ret

accept_atom:
	movsx prev_char(%rip), %rax
	lea char_class_tbl(%rip), %ebx
	movb (%rbx,%rax), %al

	cmpb cc_char(%rip), %al
	je aa_var

	movsx literal(%rip), %rax
	call accept
	xor %rbx, %rbx
	inc %rbx
	ret

	aa_var:
		movsx var(%rip), %rax
		call accept
		xor %rbx, %rbx
		inc %rbx
		ret

accept_expr:
	movsx prev_char(%rip), %rax
	lea char_type_tbl(%rip), %ebx
	movb (%rbx,%rax), %al

	xor %rbx, %rbx

	cmpb ct_opar(%rip), %al
	je ae2_tryform

	ae2_tryatom:
		inc %rbx
		movsx atom(%rip), %rax
		jmp ae2_after

	ae2_tryform:
		movsx form(%rip), %rax

	ae2_after:
		push %rbx
		call accept
		pop %rbx # %rbx is 1 if this was an atom, 0  if it was a form (i.e. a function call of some sort)
		ret # Return whatever the returned error code was (via %rax)

find_symbol: # Returns matched code address on %rsi, %rax is error code
	mov n_symbols(%rip), %rcx

	fs_callloop:
		mov n_symbols(%rip), %rdx # Load index from 0 into rdx (rcx counts down from the top)
		sub %rcx, %rdx
		push %rcx
		lea symbol_tbl(%rip), %rdi # Load symbol table
		lea (%rdx,%rdx,2), %rcx # 3 %rdx
		lea (%rdi,%rcx,8), %rcx # %rdi + 8*(3*%rdx) = %rdi + 24*%rdx
		#lea 0(%rdi), %rdi # [key] is first; no offset
		movq 8(%rdi), %rbx # [length]
		movsx accept_lgt(%rip), %ecx
		cmp %rbx, %rcx
		jne fs_cl_tail

		lea 16(%rdi), %r12
		mov (%rdi), %rdi # This points to a cell: [key, key-length, code address], from which we want [key]
		lea accept_buff(%rip), %rsi
		repe cmpsb
		jz fs_done_cl

		fs_cl_tail:
		pop %rcx
		dec %rcx
		test %rcx, %rcx
		jne fs_callloop
		jmp fs_after_cl

	fs_done_cl:
	#mov 16(%r12), %rsi
	mov (%r12), %rsi
	pop %rcx
	fs_after_cl:
	test %rcx, %rcx
	je fs_unrecognized

	xor %rax, %rax
	ret

	fs_unrecognized:
		xor %rax, %rax
		add $2, %rax

		ret

accept_form: # A form is a non-empty s-expression whose head is either a special-form (define) or a variable.
	lea char_type_tbl(%rip), %rbx
	movsx prev_char(%rip), %rax
	movb (%rbx,%rax), %al

	cmpb ct_opar(%rip), %al
	jne ef_incomplete

	call skip_ws

	xor %rsi, %rsi
	call nextch
	xor %rax, %rax
	movb var(%rip), %al
	call accept
	dec %rax # check for code 1 as opposed to 0 (good) or 2 (var exists and is unbound -- good)
	test %rax, %rax
	je ef_incomplete

	cld
	movsx accept_lgt(%rip), %ecx
	mov define_str_lgt(%rip), %rdx
	cmp %rcx, %rdx
	jne ef_nodef
	lea accept_buff(%rip), %rsi
	lea define_str(%rip), %rdi
	repe cmpsb
	jz ef_def

	ef_nodef:
	call find_symbol # returns code address on rsi
	test %rax, %rax
	jnz ef_unrecognized

	jmp ef_call

	ef_def: # handle 'define' special form
		call skip_ws
		xor %rsi, %rsi
		call nextch

		movsx var(%rip), %rax
		call accept
		test %rax, %rax
		jz ef_redef # symbol exist so it's being redefined

		cmp $2, %rax # symbol does not exist, so a new one is being defined (this is OK, too)
		jnz ef_incomplete # a different error code is present, no good!

		ef_redef:
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
		mov prev_dp(%rip), %rsi
		pop %rdi
		mov %rsi, 16(%rdi)

		lea char_type_tbl(%rip), %rbx
		movsx prev_char(%rip), %rax
		movb (%rbx,%rax), %al

		cmpb ct_cpar(%rip), %al
		je ef_def_done

		call skip_ws
		xor %rsi, %rsi
		call nextch
		lea char_type_tbl(%rip), %rbx
		movsx prev_char(%rip), %rax
		movb (%rbx,%rax), %al

		cmpb ct_cpar(%rip), %al
		jne ef_incomplete

		ef_def_done:
		xor %rax, %rax
		ret

	ef_call:
		push %rsi # Save function's address for the call at the end
		mov $8, %cl
		mov ip(%rip), %rax
		mov %rax, prev_ip(%rip)
		lea program(%rip), %rax
		add ip(%rip), %rax

	ef_loop:
		lea char_type_tbl(%rip), %rbx
		movsx prev_char(%rip), %rax
		movb (%rbx,%rax), %al

		cmpb ct_cpar(%rip), %al
		je ef_postloop

		call skip_ws

		push %rcx
		xor %rsi, %rsi
		call nextch
		pop %rcx
		lea char_type_tbl(%rip), %rbx
		movsx prev_char(%rip), %rax
		movb (%rbx,%rax), %al

		cmpb ct_cpar(%rip), %al
		je ef_postloop

		push %rcx
		xor %rax, %rax
		movsx expr(%rip), %rax
		call accept

		test %rax, %rax # success
		je ef_cont

		pop %rcx
		jmp ef_incomplete

		ef_cont:
		pop %rcx # pop cnt

		push %rsi
		push %rbx

		loop ef_loop

	ef_postloop:
	mov ip(%rip), %r8
	mov %r8, prev_ip(%rip)
	lea program(%rip), %r8
	add ip(%rip), %r8

	cmp $7, %rcx # i.e. 1 arg
	jnz ef_dopost # in that case, don't generate code as the ret is already on %rax. Oherwise, go.
	
	pop %rbx
	pop %rsi
	test %rbx, %rbx
	jz ef_gencall

	push %rsi
	push %rbx # otherwise, put it back since it's a literal

	ef_dopost:
	mov form_ptr(%rip), %rax # if it's 0, this has not been set yet.
	test %rax, %rax # if it's not 0, the entry point is already set, just skip.
	jnz ef_no_point

	lea program(%rip), %rax
	mov %r8, %r9
	sub %rax, %r9
	mov %r9, form_ptr(%rip)

	ef_no_point:
	mov %rcx, %rdx # offset from code tables is as much as leftover count
	mov $8, %rax
	sub %cl, %al
	mov %al, %cl # counter
	lea reg_imm_codes(%rip), %r13
	lea reg_shuffle_codes(%rip), %r14
	lea reloc(%rip), %r9
	add reloc_ptr(%rip), %r9

	ef_loop2:
		pop %rbx
		pop %rsi
		test %rbx, %rbx
		jz ef_arg_not_lit

		# insert literal mov
		movw (%r13,%rdx,2), %ax
		mov %ax, (%r8)
		movq %rsi, 2(%r8)
		#addq $0x601000, 2(%r8) # start of data address
		lea 2(%r8), %r10
		mov %r10, (%r9)
		add $8, %r9
		add $10, %r8
		jmp ef_arg_processed

		ef_arg_not_lit:
		# insert mov %rax, this_reg [rdx = mov code, 3 bytes each]
		lea (%r14,%rdx,2), %rdi
		lea (%rdi,%rdx), %rdi
		movw (%rdi), %ax
		movw %ax, (%r8)
		movb 2(%rdi), %al
		movb %al, 2(%r8)
		add $3, %r8

		ef_arg_processed:
		inc %rdx
		loop ef_loop2

	ef_gencall:
		# save reloc_ptr first
		lea reloc(%rip), %r10
		sub %r10, %r9
		mov %r9, reloc_ptr(%rip)

		movb $0xe8, (%r8) # call
		add $1, %r8
		pop %r12 # the function
		mov %r8, %r9
		addq $4, %r9
		lea program(%rip), %r10
		sub %r10, %r9
		sub %r9, %r12 # address is relative from the END of the full callq opcode, so we +4 %r9 to account for the upcoming insertion of the jump address.
		# (%r12 - ip=%r8) is enough.
		movl %r12d, (%r8)
		add $4, %r8
		lea program(%rip), %r9
		sub %r9, %r8
		movq %r8, ip(%rip)

	ef_end:
		#sub $8, %rcx
		#jl ef_incomplete # There were no args in the call
		xor %rax, %rax
		jmp ef_ret

	ef_incomplete:
		xor %rax, %rax
		inc %rax
		# Stack may be borked, it's too complicated to unwind so just bail directly
		jmp expect_error

	ef_unrecognized:
		mov $2, %rax
		# As above
		jmp expect_error
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
	add $2, %rdx
	call print
	mov last_charnum(%rip), %rsi
	call printint
	lea colon_space(%rip), %rsi
	xor %rdx, %rdx
	add $2, %rdx
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

NULL_CODE:
	ret

.data
expect_type_tbl:
form: .byte 0
var: .byte 1
string: .byte 2
int: .byte 3
literal: .byte 4
atom: .byte 5
expr: .byte 6

noaccept_error: .ascii "Compiler error -- no accept function for current token."
noaccept_error_lgt = . - noaccept_error

accept_tbl:
.quad accept_form
.quad accept_var
.quad accept_string
.quad accept_int
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
expect_literal_error: .ascii "Missing 'literal' expected"
expect_literal_error_lgt = . - expect_literal_error
expect_atom_error: .ascii "Missing 'atom' expected"
expect_atom_error_lgt = . - expect_atom_error
expect_expr_error: .ascii "Missing 'expr' expected"
expect_expr_error_lgt = . - expect_expr_error
unrecognized_error: .ascii "Unrecognized input"
unrecognized_error_lgt = . - unrecognized_error

unrec_err_code: .byte 7

expect_error_tbl:
.quad expect_form_error
.quad expect_var_error
.quad malformed_string_error
.quad bad_int_error
.quad expect_literal_error
.quad expect_atom_error
.quad expect_expr_error
.quad unrecognized_error

expect_error_lgt_tbl:
.quad expect_form_error_lgt
.quad expect_var_error_lgt
.quad malformed_string_error_lgt
.quad bad_int_error_lgt
.quad expect_literal_error_lgt
.quad expect_atom_error_lgt
.quad expect_expr_error_lgt
.quad unrecognized_error_lgt

accept_buff: .space 256, 0
accept_lgt: .byte 0

comma_space: .ascii ", "
colon_space: .ascii ": "

form_ptr: .quad 0 # ip at the time the last form is evaluated, i.e. entry-point

n_symbols: .quad 1

reg_shuffle_codes:
.byte 0b01001001, 0b10001001, 0b11000001 # rax -> r9
.byte 0b01001001, 0b10001001, 0b11000000 # rax -> r8
.byte 0b01001000, 0b10001001, 0b11000111 # rax -> rsi
.byte 0b01001000, 0b10001001, 0b11000110 # rax -> rdi
.byte 0b01001000, 0b10001001, 0b11000011 # rax -> rbx
.byte 0b01001000, 0b10001001, 0b11000010 # rax -> rdx
.byte 0b01001000, 0b10001001, 0b11000001 # rax -> rcx
.byte 0b01001000, 0b10001001, 0b11000000 # rax -> rax, in fact should never be generated

reg_imm_codes: # load this, then append literal val
.byte 0b1001001, 0b10111001
.byte 0b1001001, 0b10111000
.byte 0b1001000, 0b10111111
.byte 0b1001000, 0b10111110
.byte 0b1001000, 0b10111011
.byte 0b1001000, 0b10111010
.byte 0b1001000, 0b10111001
.byte 0b1001000, 0b10111000

last_charnum: .quad 0
last_linum: .quad 0

ten: .quad 10
