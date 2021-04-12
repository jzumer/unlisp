# Special notes: due to endianness, movw 0xc0ff, * is the correct sequence for inc %rbx, not ffc0.
# Note2: xlatb can't be used anymore because the addresses of the tables are too far down in the program...
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

		as_accept:
			movb prev_char(%rip), %al
			movb %al, (%r12)
			inc %r12
			incb accept_lgt(%rip)
			pop %rcx
			dec %rcx
			test %rcx, %rcx
			jne as_loop

	as_reject:
		xor %rax, %rax
		inc %rax
		ret

	as_done: # only happens when final squote is found, so always good
		pop %rcx
		xor %rsi, %rsi
		call nextch # skip over final squote
		xor %rax, %rax
		xor %rbx, %rbx
		inc %rbx
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
		xor %rbx, %rbx
		inc %rbx
		cmpb $0, accept_lgt(%rip)
		jne ai_done_done
		inc %rax

		ai_done_done:
		ret

	ai_reject:
		xor %rax, %rax
		inc %rax
		xor %rbx, %rbx
		inc %rbx
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

		call find_env
		test %rax, %rax
		jnz ef_check_global

		# %rsi = register, %rdi = env start
		mov $2, %rbx # 2 = register var

		jmp ae_ret

		ef_check_global:
		call find_symbol

		xor %rbx, %rbx
		inc %rbx

		ae_ret:
		ret

accept_atom:
	movsx prev_char(%rip), %rax
	lea char_class_tbl(%rip), %rbx
	movb (%rbx,%rax), %al

	cmpb cc_char(%rip), %al
	je aa_var

	movsx literal(%rip), %rax
	call accept
	ret

	aa_var:
		movsx var(%rip), %rax
		call accept
		ret

accept_expr:
	movsx prev_char(%rip), %rax
	lea char_type_tbl(%rip), %rbx
	movb (%rbx,%rax), %al

	#xor %rbx, %rbx

	cmpb ct_opar(%rip), %al
	je ae2_tryform

	ae2_tryatom:
		#inc %rbx
		movsx atom(%rip), %rax
		jmp ae2_after

	ae2_tryform:
		movsx form(%rip), %rax

	ae2_after:
		#push %rbx
		call accept
		#pop %rbx # %rbx is 1 if this was an atom, 0  if it was a form (i.e. a function call of some sort)

		ret # Return whatever the returned error code was (via %rax)

find_symbol: # Returns matched code address on %rsi, %rax is error code
	mov n_symbols(%rip), %rcx

	fs_callloop:
		mov n_symbols(%rip), %rdx # Load index from 0 into rdx (rcx counts down from the top)
		sub %rcx, %rdx
		push %rcx
		lea symbol_tbl(%rip), %rdi # Load symbol table
		lea (%rdx,%rdx,2), %rcx # 3 %rdx
		lea (%rdi,%rcx,8), %rdi # %rdi + 8*(3*%rdx) = %rdi + 24*%rdx
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

find_env: # Like find_symbol but on the environment, all the way to the root.
	# matched register is on %rsi, %rdi is %rsp-relative offset, %rax is error code.
	mov curr_env(%rip), %rcx
	xor %r12, %r12 # %rsp offset (-1 = already in register, otherwise offset(%rsp))
	dec %r12
	xor %r13, %r13 # how many vars we have passed in this env
	xor %r14, %r14 # whether we are looking at stacks or current registers
	# (0 = current registers)

	fe_callloop:
		test %rcx, %rcx
		je fe_unrecognized

		mov 8(%rcx), %rax
		test %rax, %rax
		je fe_next_env

		inc %r13

		push %rcx
		movq 8(%rcx), %rbx # lgt
		movsx accept_lgt(%rip), %ecx
		cmp %rbx, %rcx
		jne fe_next_var

		movq %rcx, %rdi # str
		lea accept_buff(%rip), %rsi
		repe cmpsb

		jz fe_done_cl # found!

		fe_next_var:
			pop %rcx
			addq $16, %rcx
			addq $8, %r13
			jmp fe_callloop

		fe_next_env:
			test %r14, %r14
			jne fe_env_next_env
			
			inc %r14
			mov %r13, %r12
			jmp fe_real_next_env

			fe_env_next_env:
			add %r13, %r12

			fe_real_next_env:
			xor %r13, %r13
			mov 0x80(%rcx), %rcx
			jmp fe_callloop

	fe_done_cl:
		#xor %rdi, %rdi
		#sub %r12, %rdi
		mov %r12, %rdi
		movq 16(%rcx), %rsi

		xor %rax, %rax
		ret

	fe_unrecognized:
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
	movb expr(%rip), %al
	call accept
	# The required invariants are:
	# %rax is not 1 if it succeeded;
	# %rsi contains the address for the call
	# %rbx indicates if it's a form (i.e. a call) or something else

	dec %rax # check for code 1 as opposed to 0 (good) or 2 (var exists and is unbound -- good)
	test %rax, %rax
	je ef_incomplete

	test %rbx, %rbx # 0 = form, 1 = otherwise (assume var)
	je ef_incomplete # XXX: not yet accepted (basically just need to move ret to r10 and call or similar)

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
	movsx accept_lgt(%rip), %ecx
	mov nop_str_lgt(%rip), %rdx
	cmp %rcx, %rdx
	jne ef_no_nop
	lea accept_buff(%rip), %rsi
	lea nop_str(%rip), %rdi
	repe cmpsb
	jz ef_nop

	ef_no_nop:
	movsx accept_lgt(%rip), %ecx
	mov if_str_lgt(%rip), %rdx
	cmp %rcx, %rdx
	jne ef_no_if
	lea accept_buff(%rip), %rsi
	lea if_str(%rip), %rdi
	repe cmpsb
	jz ef_if

	ef_no_if:
	movsx accept_lgt(%rip), %ecx
	mov lambda_str_lgt(%rip), %rdx
	cmp %rcx, %rdx
	jne ef_no_lambda
	lea accept_buff(%rip), %rsi
	lea lambda_str(%rip), %rdi
	repe cmpsb
	jz ef_lambda

	ef_no_lambda:
	# Potential TODO to support 1st-class functions
	#call find_env
	#test %rax, %rax
	#jnz ef_no_env1

	#jmp ef_call

	#ef_no_env1:
	call find_symbol # returns data address on rsi
	test %rax, %rax
	jnz ef_unrecognized

	jmp ef_call

	ef_lambda:
		mov ip(%rip), %rax
		mov %rax, prev_ip(%rip)
		lea program(%rip), %rax
		add ip(%rip), %rax
		movb $0xe9, (%rax) # jmp
		movl $0x000000, 1(%rax) # placeholder
		# used to jmp past the lambda code
		add $1, %rax
		push %rax
		mov %rax, jmp1(%rip)
		mov ip(%rip), %rax
		add $5, %rax
		mov %rax, jmp_val1(%rip)
		mov %rax, ip(%rip)

		call skip_ws
		xor %rsi, %rsi
		call nextch

		movsx prev_char(%rip), %rax
		lea char_type_tbl(%rip), %rbx
		movb (%rbx, %rax), %al

		cmpb ct_opar(%rip), %al
		jne ef_incomplete

		pop %rax
		sub $0x88, %rsp # format is (string:8, str lgt:8). Last entry is (prev env:8).
		mov curr_env(%rip), %rbx
		mov %rbx, 0x80(%rsp)
		mov %rsp, curr_env(%rip)
		push %rax

		mov $8, %ecx # max 8 args

		ef_l_argloop:
			push %rcx

			call skip_ws
			xor %rsi, %rsi
			call nextch

			pop %rcx

			movsx prev_char(%rip), %rax
			lea char_type_tbl(%rip), %rbx
			movb (%rbx, %rax), %al

			cmpb ct_cpar(%rip), %al
			je ef_l_argdone

			push %rcx

			movb var(%rip), %al
			call accept # if it's not closing parens, it MUST be a var, but it's fine if it's unbound...

			dec %rax
			test %rax, %rax
			je ef_incomplete # if %rax is 1, it means not-a-var, otherwise it's either bound or not.

			# on accept_buff we now have the varname.
			# Put the var in the env and set its mapping to the next available register.
			movsx accept_lgt(%rip), %rsi
			call malloc

			test %rax, %rax
			jz err_malloc_failed

			mov %rdi, %r13

			lea accept_buff(%rip), %rsi
			movsx accept_lgt(%rip), %ecx
			rep movsb

			mov (%rsp), %rcx

			xor %rdi, %rdi
			lea (,%rcx,8), %rcx
			lea (%rcx,%rcx), %rcx
			sub %rcx, %rdi
			lea 16(%rsp,%rdi), %rdi # two item on top of stack

			movsx accept_lgt(%rip), %rcx
			movl %ecx, 8(%rdi)
			# save str ptr in symbol_tbl
			movq %r13, (%rdi)

			pop %rcx
			dec %rcx
			jnz ef_l_argloop

		jmp ef_incomplete # failed to jump to ef_l_argdone before count was done

		ef_l_argdone:
		pop %rax
		mov $8, %rbx
		sub %rax, %rbx # rbx = param count

		mov dp(%rip), %rcx
		mov %rcx, prev_dp(%rip)
		lea data(%rip), %rcx
		add dp(%rip), %rcx
		movq %rbx, (%rcx)
		movq (%rsp), %rbx
		movq %rbx, 8(%rcx) # format is n_params:8, address:8
		addq $16, dp(%rip)

		movb expr(%rip), %al
		call expect

		lea program(%rip), %rax
		addq ip(%rip), %rax

		test %rbx, %rbx
		jz ef_l_skipmov

		#mov mem_to_reg_codes(%rip), %rcx
		#mov %rcx, (%rax)
		#add $8, %rax
		#addq $8, ip(%rip)

		ef_l_skipmov:
		# ??. implement recursion somehow? either jmp1 is filled and def fills it
		#	or we receive our symbol and patch it ourselves at start.

		# Teardown env; note that this leaks the string we had allocated...
		movq 0x80(%rsp), %rax
		movq %rax, curr_env(%rip)
		add $0x88, %rsp

		mov ip(%rip), %rax
		mov jmp1(%rip), %rbx
		mov jmp_val1(%rip), %rcx
		sub %rax, %rcx
		movl %ecx, (%rbx)

		lea program(%rip), %rax
		pop %rsi
		sub %rax, %rsi # the ip to for the code we just inserted

		xor %rax, %rax
		xor %rbx, %rbx
		ret

	ef_if:
		call skip_ws
		movb expr(%rip), %al
		call expect

		mov ip(%rip), %rax
		mov %rax, prev_ip(%rip)
		lea program(%rip), %rax
		addq ip(%rip), %rax
		movb $0x50, (%rax) # push %rax
		inc %rax
		movq mem_to_reg_codes(%rip), %rbx
		movq %rbx, (%rax) # mov ret(%rip), %rax
		add $8, %rax
		movb $0x48, (%rax)
		movb $0x85, 1(%rax)
		movb $0xc0, 2(%rax) # test %rax, %rax
		add $3, %rax
		movb $0x0f, (%rax)
		movb $0x85, 1(%rax) # jne [4 bytes offset]
		add $2, %rax

		# save the jump to inject the false path's address
		mov %rax, jmp1(%rip)
		add $4, %rax
		lea program(%rip), %rbx
		sub %rbx, %rax
		mov %rax, ip(%rip)
		mov %rax, jmp_val1(%rip)

		movb expr(%rip), %al
		call expect

		mov ip(%rip), %rax
		mov jmp1(%rip), %rbx
		mov jmp_val1(%rip), %rcx
		sub %rax, %rcx
		movl %ecx, (%rbx)

		lea program(%rip), %rax
		add ip(%rip), %rax
		movb $0x0f, (%rax)
		movb $0x85, 1(%rax)
		add $2, %rax

		mov %rax, jmp1(%rip)
		add $4, %rax
		lea program(%rip), %rbx
		sub %rax, %rbx
		mov %rax, ip(%rip)
		mov %rax, jmp_val1(%rip)

		movb expr(%rip), %al
		call expect

		mov ip(%rip), %rax
		mov jmp1(%rip), %rbx
		mov jmp_val1(%rip), %rcx
		sub %rax, %rcx
		movl %ecx, (%rbx)

		lea program(%rip), %rax
		add ip(%rip), %rax
		movb $0x58, (%rax) # pop %rax
		inc %rax
		lea program(%rip), %rbx
		sub %rbx, %rax
		mov %rax, ip(%rip)

		call skip_ws
		xor %rsi, %rsi
		call nextch
		xor %rax, %rax
		movb prev_char(%rip), %al
		lea char_type_tbl(%rip), %rbx
		movb (%rax,%rbx), %al

		cmpb ct_cpar(%rip), %al
		jne ef_incomplete

		xor %rax, %rax
		xor %rbx, %rbx
		ret

	ef_nop: # just move the prev_ip forward, no actual code is output here
		mov ip(%rip), %rax
		mov %rax, prev_ip(%rip)
		mov %rax, %rsi
		push %rsi

		call skip_ws
		xor %rsi, %rsi
		call nextch

		xor %rax, %rax
		movb prev_char(%rip), %al
		lea char_type_tbl(%rip), %rsi
		movb (%rax,%rsi), %al
		
		cmpb ct_cpar(%rip), %al
		jne ef_incomplete

		pop %rsi
		xor %rax, %rax
		xor %rbx, %rbx
		ret

	ef_def: # handle 'define' special form
		call skip_ws
		xor %rsi, %rsi
		call nextch

		movsx var(%rip), %rax
		call accept
		test %rax, %rax
		jz ef_redef # symbol exist so it's being redefined

		cmp $2, %rax # symbol does not exist, so a new one is being defined (this is OK, too)
		jne ef_incomplete # a different error code is present, no good!

		ef_redef:
		mov n_symbols(%rip), %r12
		inc %r12
		mov %r12, n_symbols(%rip)
		dec %r12

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
		movsx accept_lgt(%rip), %rcx
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
		xor %rbx, %rbx
		ret

	ef_call:
		# Potential TODO: accept functions in lambdas by this mechanism
		# This is only reached right after a find_symbol

		mov ip(%rip), %rax
		mov %rax, prev_ip(%rip)
		lea program(%rip), %rax
		add ip(%rip), %rax

		lea data(%rip), %rdi # a function is represented in data as [n_args, address]
		add %rdi, %rsi

		mov (%rsi), %rdi
		pushq 8(%rsi) # function address
		pushq %rdi # arg count
		pushq %rdi # arg count that will be used as counter

		lea reg_codes(%rip), %rbx

		mov $8, %rcx
		sub %rdi, %rcx
		mov %rcx, %rdi
		xor %rcx, %rcx

		ef_pushloop:
			cmpb $8, %dil
			je ef_pre_loop
			cmp $2, %rdi
			jb ef_push_rex

			movb $0x50, (%rax) # push
			movb -1(%rbx,%rdi), %cl # index is off by one since 1 arg = option 0
			addb %cl, (%rax) # register
			inc %rax
			jmp ef_end_pushloop

			ef_push_rex:
			movb $0x41, (%rax)
			movb -1(%rbx,%rdi), %cl
			addb $0x50, %cl
			movb %cl, 1(%rax) # push+rex register
			add $2, %rax

			ef_end_pushloop:
			incb %dil
			jmp ef_pushloop

	# top of stack is the count
	ef_pre_loop:
		lea program(%rip), %rbx
		sub %rbx, %rax
		movq %rax, ip(%rip)

		ef_loop:
		lea char_type_tbl(%rip), %rbx
		movsx prev_char(%rip), %rax
		movb (%rbx,%rax), %al

		cmpb ct_cpar(%rip), %al
		je ef_postloop

		call skip_ws

		xor %rsi, %rsi
		call nextch
		lea char_type_tbl(%rip), %rbx
		movsx prev_char(%rip), %rax
		movb (%rbx,%rax), %al

		cmpb ct_cpar(%rip), %al
		je ef_postloop

		xor %rax, %rax
		movsx expr(%rip), %rax
		call accept

		test %rax, %rax # success
		je ef_cont

		pop %rcx
		jmp ef_incomplete

		ef_cont:
		pop %rcx # running cnt
		pop %rdx # total cnt

		push %rsi
		push %rdi
		push %rbx

		push %rdx
		dec %rcx
		push %rcx

		jnz ef_loop

	ef_postloop:
	pop %rcx # decremented value in above loop, i.e. 0 -- drop

	mov (%rsp), %rax # arg count
	mov $8, %rcx
	sub %rax, %rcx
	cmp $8, %rcx
	je ef_gencall

	ef_dopost:
	mov %rcx, %rdx
	# offset from code tables is as much as "leftover" count
	# i.e. inverted index
	lea reg_imm_codes(%rip), %r13
	lea reg_shuffle_codes(%rip), %r14
	lea reloc(%rip), %r9
	add reloc_ptr(%rip), %r9

	lea program(%rip), %r8
	add ip(%rip), %r8

	ef_loop2:
		pop %rax # cnt

		pop %rbx
		pop %rdi
		pop %rsi

		push %rax # cnt back on top

		test %rbx, %rbx
		jz ef_arg_call

		dec %rbx
		test %rbx, %rbx
		jnz ef_arg_env # if it's not 1 or 0, it must be an env arg from lambda

		# insert literal mov
		movw (%r13,%rdx,2), %ax
		mov %ax, (%r8)
		movq %rsi, 2(%r8)
		lea 2(%r8), %r10
		movq $1, (%r9)
		mov %r10, 8(%r9)
		add $16, %r9
		add $10, %r8
		jmp ef_arg_processed

		ef_arg_env:
		# rdi = rsp offset
		# rsi = register it came from (not used if rdi > -1)
		inc %rdi
		test %rdi, %rdi
		jz ef_env_direct

		# This was not directly in a reg, so emit mov (%rsp,[%rdi]), [%rcx->reg]
		dec %rdi
		lea reg_rsp_codes(%rip), %r10
		movl (%r10,%rdx,4), %eax
		movl %eax, (%r8)
		movl %edi, 4(%r8)
		addq $8, %r8

		jmp ef_arg_processed

		ef_env_direct:
		# This was directly in a reg whose index is in %rsi, emit the mov
		lea reg_reg_codes(%rip), %rax
		lea (,%rsi,8), %rsi
		lea (%rax,%rsi,4), %rsi
		mov (%rsi,%rdx), %rax # address is reg_reg_codes + 24*rsi + rcx
		movb %al, (%r8)
		inc %rax
		movw %ax, 1(%r8)
		add $3, %r8
		jmp ef_arg_processed

		ef_arg_call: # then it was a call, and ret val is at 0 in data, so mov it to our reg
		# mov 0x0 = ret mem addr, %reg
		lea mem_to_reg_codes(%rip), %rax
		add %rdx, %rax
		movq (%rax), %rbx
		movq %rbx, (%r8)

		# register reloc
		add $4, %r8
		movq $0, (%r9)
		movq %r8, 8(%r9)
		add $4, %r8
		add $16, %r9

		ef_arg_processed:
		inc %rdx
		cmp $8, %rdx
		jne ef_loop2

	ef_gencall:
		# save reloc_ptr first
		lea reloc(%rip), %r10
		sub %r10, %r9
		mov %r9, reloc_ptr(%rip)

		movb $0xe8, (%r8) # call
		inc %r8
		pop %rax # cnt
		pop %r12 # the function
		push %rax
		mov %r8, %r9
		addq $4, %r9
		lea program(%rip), %r10
		sub %r10, %r9
		sub %r9, %r12 # address is relative from the END of the full callq opcode, so we +4 %r9 to account for the upcoming insertion of the jump address.
		movl %r12d, (%r8)
		add $4, %r8

	mov $8, %rdi
	sub (%rsp), %rdi
	add $8, %rsp
	# cnt is on top of stack and we want 8 - cnt

	lea reg_codes(%rip), %rbx
	xor %rcx, %rcx

	ef_poploop:
		cmpb $8, %dil
		je ef_end
		cmp $2, %rdi
		jb ef_pop_rex

		movb $0x58, (%r8) # pop
		movb -1(%rbx,%rdi), %cl # dil = 0 is impossible since we have at least 1 arg or we wouldn't be here
		addb %cl, (%r8) # register
		inc %r8
		jmp ef_end_poploop

		ef_pop_rex:
		movb $0x41, (%r8)
		movb -1(%rbx,%rdi), %cl
		addb $0x58, %cl
		movb %cl, 1(%r8) # pop + rex register
		add $2, %r8

		ef_end_poploop:
		incb %dil
		jmp ef_poploop

	ef_end:
		lea program(%rip), %r9
		sub %r9, %r8
		movq %r8, ip(%rip)

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
		xor %rbx, %rbx
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

curr_env: .quad 0 # set to 0 if only globals exist, otherwise set to current env on stack.
# At the time an env is popped off the stack, the pointer to the previous env is placed here.
# If the result is 0, it means we've popped all the stacks off.

# jump locations for post-read redirects e.g. if and inline defs
# jmp_val* is the initial value since the jumps are relative
jmp1: .quad 0
jmp_val1: .quad 0
jmp2: .quad 0
jmp_val2: .quad 0

comma_space: .ascii ", "
colon_space: .ascii ": "

n_symbols: .quad 1

reg_codes:
# non-rex
.byte 0b0 # rax
.byte 0b1 # rcx
.byte 0b10 # rdx
.byte 0b11 # rbx
.byte 0b110 # rdi
.byte 0b111 # rsi
# rex
.byte 0b0 # r8
.byte 0b1 # r9

mem_to_reg_codes:
.byte 0x4c, 0x8b, 0x0c, 0x25, 0x00, 0x00, 0x00, 0x00 # mov 0x0, %r9
.byte 0x4c, 0x8b, 0x04, 0x25, 0x00, 0x00, 0x00, 0x00 # mov 0x0, %r8
.byte 0x48, 0x8b, 0x3c, 0x25, 0x00, 0x00, 0x00, 0x00 # mov 0x0, %rdi
.byte 0x48, 0x8b, 0x34, 0x25, 0x00, 0x00, 0x00, 0x00 # mov 0x0, %rsi
.byte 0x48, 0x8b, 0x1c, 0x25, 0x00, 0x00, 0x00, 0x00 # mov 0x0, %rbx
.byte 0x48, 0x8b, 0x14, 0x25, 0x00, 0x00, 0x00, 0x00 # mov 0x0, %rdx
.byte 0x48, 0x8b, 0x0c, 0x25, 0x00, 0x00, 0x00, 0x00 # mov 0x0, %rcx
.byte 0x48, 0x8b, 0x04, 0x25, 0x00, 0x00, 0x00, 0x00 # mov 0x0, %rax

reg_shuffle_codes:
.byte 0b01001001, 0b10001001, 0b11000001 # rax -> r9
.byte 0b01001001, 0b10001001, 0b11000000 # rax -> r8
.byte 0b01001000, 0b10001001, 0b11000111 # rax -> rsi
.byte 0b01001000, 0b10001001, 0b11000110 # rax -> rdi
.byte 0b01001000, 0b10001001, 0b11000011 # rax -> rbx
.byte 0b01001000, 0b10001001, 0b11000010 # rax -> rdx
.byte 0b01001000, 0b10001001, 0b11000001 # rax -> rcx
.byte 0b01001000, 0b10001001, 0b11000000 # rax -> rax, in fact should never be generated

reg_reg_codes: # idx = source * 24 + dest
.byte 0b01001101, 0b10001001, 0b11001001 # r9 -> r9
.byte 0b01001101, 0b10001001, 0b11001000 # r9 -> r8
.byte 0b01001100, 0b10001001, 0b11001111 # r9 -> rsi
.byte 0b01001100, 0b10001001, 0b11001110 # r9 -> rdi
.byte 0b01001100, 0b10001001, 0b11001011 # r9 -> rbx
.byte 0b01001100, 0b10001001, 0b11001010 # r9 -> rdx
.byte 0b01001100, 0b10001001, 0b11001001 # r9 -> rcx
.byte 0b01001100, 0b10001001, 0b11001000 # r9 -> rax

.byte 0b01001101, 0b10001001, 0b11000001 # r8 -> r9
.byte 0b01001101, 0b10001001, 0b11000000 # r8 -> r8
.byte 0b01001100, 0b10001001, 0b11000111 # r8 -> rsi
.byte 0b01001100, 0b10001001, 0b11000110 # r8 -> rdi
.byte 0b01001100, 0b10001001, 0b11000011 # r8 -> rbx
.byte 0b01001100, 0b10001001, 0b11000010 # r8 -> rdx
.byte 0b01001100, 0b10001001, 0b11000001 # r8 -> rcx
.byte 0b01001100, 0b10001001, 0b11000000 # r8 -> rax

.byte 0b01001001, 0b10001001, 0b11111001 # rsi -> r9
.byte 0b01001001, 0b10001001, 0b11111000 # rsi -> r8
.byte 0b01001000, 0b10001001, 0b11111111 # rsi -> rsi
.byte 0b01001000, 0b10001001, 0b11111110 # rsi -> rdi
.byte 0b01001000, 0b10001001, 0b11111011 # rsi -> rbx
.byte 0b01001000, 0b10001001, 0b11111010 # rsi -> rdx
.byte 0b01001000, 0b10001001, 0b11111001 # rsi -> rcx
.byte 0b01001000, 0b10001001, 0b11111000 # rsi -> rax

.byte 0b01001001, 0b10001001, 0b11110001 # rdi -> r9
.byte 0b01001001, 0b10001001, 0b11110000 # rdi -> r8
.byte 0b01001000, 0b10001001, 0b11110111 # rdi -> rsi
.byte 0b01001000, 0b10001001, 0b11110110 # rdi -> rdi
.byte 0b01001000, 0b10001001, 0b11110011 # rdi -> rbx
.byte 0b01001000, 0b10001001, 0b11110010 # rdi -> rdx
.byte 0b01001000, 0b10001001, 0b11110001 # rdi -> rcx
.byte 0b01001000, 0b10001001, 0b11110000 # rdi -> rax

.byte 0b01001001, 0b10001001, 0b1101001 # rbx -> r9
.byte 0b01001001, 0b10001001, 0b1101000 # rbx -> r8
.byte 0b01001000, 0b10001001, 0b1101111 # rbx -> rsi
.byte 0b01001000, 0b10001001, 0b1101110 # rbx -> rdi
.byte 0b01001000, 0b10001001, 0b1101011 # rbx -> rbx
.byte 0b01001000, 0b10001001, 0b1101010 # rbx -> rdx
.byte 0b01001000, 0b10001001, 0b1101001 # rbx -> rcx
.byte 0b01001000, 0b10001001, 0b1101000 # rbx -> rax

.byte 0b01001001, 0b10001001, 0b11010001 # rdx -> r9
.byte 0b01001001, 0b10001001, 0b11010000 # rdx -> r8
.byte 0b01001000, 0b10001001, 0b11010111 # rdx -> rsi
.byte 0b01001000, 0b10001001, 0b11010110 # rdx -> rdi
.byte 0b01001000, 0b10001001, 0b11010011 # rdx -> rbx
.byte 0b01001000, 0b10001001, 0b11010010 # rdx -> rdx
.byte 0b01001000, 0b10001001, 0b11010001 # rdx -> rcx
.byte 0b01001000, 0b10001001, 0b11010000 # rdx -> rax

.byte 0b01001001, 0b10001001, 0b11001001 # rcx -> r9
.byte 0b01001001, 0b10001001, 0b11001000 # rcx -> r8
.byte 0b01001000, 0b10001001, 0b11001111 # rcx -> rsi
.byte 0b01001000, 0b10001001, 0b11001110 # rcx -> rdi
.byte 0b01001000, 0b10001001, 0b11001011 # rcx -> rbx
.byte 0b01001000, 0b10001001, 0b11001010 # rcx -> rdx
.byte 0b01001000, 0b10001001, 0b11001001 # rcx -> rcx
.byte 0b01001000, 0b10001001, 0b11001000 # rcx -> rax

.byte 0b01001001, 0b10001001, 0b11000001 # rax -> r9
.byte 0b01001001, 0b10001001, 0b11000000 # rax -> r8
.byte 0b01001000, 0b10001001, 0b11000111 # rax -> rsi
.byte 0b01001000, 0b10001001, 0b11000110 # rax -> rdi
.byte 0b01001000, 0b10001001, 0b11000011 # rax -> rbx
.byte 0b01001000, 0b10001001, 0b11000010 # rax -> rdx
.byte 0b01001000, 0b10001001, 0b11000001 # rax -> rcx
.byte 0b01001000, 0b10001001, 0b11000000 # rax -> rax

reg_imm_codes: # load this, then append literal val
.byte 0b01001001, 0b10111001
.byte 0b01001001, 0b10111000
.byte 0b01001000, 0b10111111
.byte 0b01001000, 0b10111110
.byte 0b01001000, 0b10111011
.byte 0b01001000, 0b10111010
.byte 0b01001000, 0b10111001
.byte 0b01001000, 0b10111000

reg_rsp_codes: # offset(%rsp) -> reg. append 32-bit offset at end.
.byte 0b01001001, 0b10001011, 0b10001100, 0b00100100 # -> r9
.byte 0b01001001, 0b10001011, 0b10000100, 0b00100100 # -> r8
.byte 0b01001000, 0b10001011, 0b10111100, 0b00100100 # -> rsi
.byte 0b01001000, 0b10001011, 0b10110100, 0b00100100 # -> rdi
.byte 0b01001000, 0b10001011, 0b10011100, 0b00100100 # -> rbx
.byte 0b01001000, 0b10001011, 0b10010100, 0b00100100 # -> rdx
.byte 0b01001000, 0b10001011, 0b10001100, 0b00100100 # -> rcx
.byte 0b01001000, 0b10001011, 0b10000100, 0b00100100 # -> rax

last_charnum: .quad 0
last_linum: .quad 0

ten: .quad 10
