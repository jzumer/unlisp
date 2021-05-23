.global _start
.text

_start:
	call nextch
	call nextch

	call skip_ws

	movb form(%rip), %al
	call expect

	main_loop:
	call skip_ws

	movb prev_char(%rip), %al
	cmpb $3, %al # EOT
	je main_end

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

insert_load: # expects %rax = place to load into, %rcx = n_args
	test %rcx, %rcx
	jz skip_insert

	# mov %r15, %r14
	movb $0x4d, (%rax)
	movb $0x89, 1(%rax)
	movb $0xfe, 2(%rax)
	add $3, %rax

	# mov %r15, %r13
	movb $0x4d, (%rax)
	movb $0x89, 1(%rax)
	movb $0xfd, 2(%rax)
	add $3, %rax

	insert_main:
		# mov (%rsp,%r13,8), %r14
		movb $0x4e, (%rax)
		movb $0x8b, 1(%rax)
		movb $0x34, 2(%rax)
		movb $0xec, 3(%rax)
		add $4, %rax

		# inc %r14
		movb $0x49, (%rax)
		movb $0xff, 1(%rax)
		movb $0xc6, 2(%rax)
		add $3, %rax
		# inc %r14
		movb $0x49, (%rax)
		movb $0xff, 1(%rax)
		movb $0xc6, 2(%rax)
		add $3, %rax

		# add %r14, %r13
		movb $0x4d, (%rax)
		movb $0x01, 1(%rax)
		movb $0xf5, 2(%rax)
		add $3, %rax

		loop insert_main

	finish_insert:
	# dec %r13
	movb $0x49, (%rax)
	movb $0xff, 1(%rax)
	movb $0xcd, 2(%rax)
	add $3, %rax

	skip_insert:
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
		movsx prev_char(%rip), %rax
		movb (%rbx,%rax), %al
		cmpb cc_white(%rip), %al
		jne sw_done

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

		call nextch
		movb $'\\', prev_char(%rip)
		jmp as_accept

		as_esc_squote:
		cmpb ct_squote(%rip), %al
		jne as_esc_nl

		call nextch
		movb $'"', prev_char(%rip)
		jmp as_accept

		as_esc_nl:
		movb this_char(%rip), %al
		cmpb $'n', %al
		jne as_esc_tab

		call nextch
		movb $'\n', prev_char(%rip)
		jmp as_accept

		as_esc_tab:
		cmpb $'t', %al
		jne as_esc_nul

		call nextch
		movb $'\t', prev_char(%rip)
		jmp as_accept

		as_esc_nul:
		cmpb $'0', %al
		jne as_esc_squote

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
		xor %rbx, %rbx
		inc %rbx
		ret

	al_int:
		movsx int(%rip), %rax
		call expect

		movsx accept_lgt(%rip), %ecx
		lea accept_buff(%rip), %rsi

		xor %rbx, %rbx
		xor %rdx, %rdx
		xor %rax, %rax

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
		add $8, dp(%rip)
		mov prev_dp(%rip), %rsi

		jmp al_accept

	al_reject:
		xor %rax, %rax
		inc %rax
		xor %rbx, %rbx
		inc %rbx
		ret

	al_accept:
		xor %rax, %rax
		xor %rbx, %rbx
		inc %rbx
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
			movsx prev_char(%rip), %r14
			movb %r14b, (%r13)
			inc %r13
			inc %r12
			push %rcx
			push %r12
			push %r13

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
	cmpb cc_symb(%rip), %al
	jne aa_lit

	movsx prev_char(%rip), %rax
	lea char_type_tbl(%rip), %rbx
	movb (%rbx,%rax), %al
	cmpb ct_none(%rip), %al
	je aa_var
	cmpb ct_squote(%rip), %al
	je aa_lit

	xor %rax, %rax
	inc %rax # no good, this is a special character!
	ret

	aa_lit:
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
		call accept

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

		mov 16(%rdi), %r12
		mov (%rdi), %rdi # This points to a cell: [key, key-length, code address], from which we want [key]
		lea accept_buff(%rip), %rsi
		repe cmpsb
		jz fs_done_cl

		fs_cl_tail:
		pop %rcx
		dec %rcx
		test %rcx, %rcx
		jne fs_callloop
		jmp fs_unrecognized

	fs_done_cl:
	mov %r12, %rsi
	pop %rcx
	xor %rax, %rax
	ret

	fs_unrecognized:
		xor %rax, %rax
		add $2, %rax

		ret

find_env: # Like find_symbol but on the environment, all the way to the root.
	# matched register is on %rsi, %rdi is %rsp-relative offset, %rax is error code.
	movq curr_env(%rip), %rcx
	mov %rcx, %r14 # current env base
	xor %r12, %r12 # %rsp offset (-1 = already in register, otherwise offset(%rsp))
	dec %r12
	xor %r13, %r13 # register index if applicable
	xor %r15, %r15 # how many envs we passed, for runtime r13 calculation

	fe_callloop:
		test %rcx, %rcx
		je fe_unrecognized

		mov (%rcx), %rax
		test %rax, %rax
		je fe_next_env

		push %rcx
		movq 8(%rcx), %rdi
		movq (%rcx), %rbx # lgt
		movsx accept_lgt(%rip), %ecx
		cmp %rbx, %rcx
		pop %rcx
		jne fe_next_var

		push %rcx
		movsx accept_lgt(%rip), %ecx

		#movq %rcx, %rdi # str
		lea accept_buff(%rip), %rsi
		repe cmpsb
		pop %rcx

		jz fe_done_cl # found!

		fe_next_var:
			addq $16, %rcx
			inc %r13

			cmp $-1, %r12
			je fe_callloop

			inc %r12
			jmp fe_callloop

		fe_next_env:
			inc %r15

			cmp $-1, %r12
			jne fe_cont

			xor %r12, %r12

			fe_cont:
			xor %r13, %r13
			movq 0x80(%r14), %rcx
			mov %rcx, %r14
			jmp fe_callloop

	fe_done_cl:
		mov %r12, %rdi
		mov %r13, %rsi
		cmp $-1, %r12
		cmovne %r15, %rsi # if we're on stack, %rsi must be how many envs we passed
		xor %rax, %rax
		ret

	fe_unrecognized:
		mov $2, %rax
		ret

accept_form: # A form is a non-empty s-expression whose head is either a special-form (define) or a variable.
	lea char_type_tbl(%rip), %rbx
	movsx prev_char(%rip), %rax
	movb (%rbx,%rax), %al

	cmpb ct_opar(%rip), %al
	jne ef_incomplete

	call nextch # skip over '('
	call skip_ws

	lea char_type_tbl(%rip), %rbx
	movsx prev_char(%rip), %rax
	movb (%rbx,%rax), %al

	cmpb ct_opar(%rip), %al
	jne af_normal

	af_normal:
	xor %rax, %rax
	movb expr(%rip), %al
	call accept

	dec %rax # check for code 1 as opposed to 0 (good) or 2 (var exists and is unbound -- good)
	test %rax, %rax
	je ef_incomplete

	dec %rax
	test %rax, %rax
	jne ef_call # if it is 0, then the code was 2, so it can't be a call. Otherwise it's either invalid or a call.

	test %rbx, %rbx # 0 = form, 1 = otherwise (assume var)
	je ef_incomplete

	cld
	movsx accept_lgt(%rip), %ecx
	mov lambda_str_lgt(%rip), %rdx
	cmp %rcx, %rdx
	jne ef_no_lambda
	lea accept_buff(%rip), %rsi
	lea lambda_str(%rip), %rdi
	repe cmpsb
	jne ef_no_lambda

	ef_lambda:
		mov pending_fn(%rip), %rsi
		test %rsi, %rsi
		je ef_not_a_def

		lea symbol_tbl(%rip), %rax
		lea (%rsi,%rsi,2), %rsi
		lea (%rax,%rsi,8), %rax
		mov dp(%rip), %rsi
		mov %rsi, 16(%rax)
		push %rsi
		movq $0, pending_fn(%rip)
		jmp ef_after_def

		ef_not_a_def:
		mov dp(%rip), %rsi
		push %rsi

		ef_after_def:
		mov ip(%rip), %rax
		mov %rax, prev_ip(%rip)
		lea program(%rip), %rax
		add ip(%rip), %rax
		movb $0xe9, (%rax) # jmp
		movl $0, 1(%rax) # placeholder
		# used to jmp past the lambda code
		inc %rax
		push %rax
		add $4, %rax
		push %rax
		add $5, ip(%rip)

		# push %r15
		movb $0x41, (%rax)
		movb $0x57, 1(%rax)
		add $2, %rax

		# mov $0, %r15
		movb $0x49, (%rax)
		movb $0xc7, 1(%rax)
		movb $0xc7, 2(%rax)
		movb $0x00, 3(%rax)
		movb $0x00, 4(%rax)
		movb $0x00, 5(%rax)
		movb $0x00, 6(%rax)
		add $7, %rax
		#add $7, ip(%rip)

		call skip_ws

		movsx prev_char(%rip), %rax
		lea char_type_tbl(%rip), %rbx
		movb (%rbx, %rax), %al

		cmpb ct_opar(%rip), %al
		jne ef_incomplete

		call nextch # skip '('

		mov $8, %rcx # format is (string:8, str lgt:8). Last entry is (prev env:8).
		ef_clear_stack:
			pushq $0
			pushq $0
			loop ef_clear_stack
		pushq $0 # next env
		pushq $0 # n_args

		mov curr_env(%rip), %rbx
		mov %rbx, 0x80(%rsp)
		mov %rsp, curr_env(%rip)

		mov $8, %rcx # max 8 args

		ef_l_argloop:
			push %rcx
			call skip_ws
			pop %rcx

			movsx prev_char(%rip), %rax
			lea char_type_tbl(%rip), %rbx
			movb (%rbx, %rax), %al

			cmpb ct_cpar(%rip), %al
			je ef_l_argdone

			push %rcx

			movb var(%rip), %al
			call accept # if it's not closing parens, it MUST be a var, but it's fine if it's unbound...
			pop %rcx

			dec %rax
			test %rax, %rax
			je ef_incomplete # if %rax is 1, it means not-a-var, otherwise it's either bound or not.
			push %rcx

			# on accept_buff we now have the varname.
			# Put the var in the env and set its mapping to the next available register.
			movsx accept_lgt(%rip), %rsi
			call my_malloc

			pop %rcx

			cmp $0, %rax
			jb err_malloc_failed

			push %rcx

			mov %rdi, %r13

			lea accept_buff(%rip), %rsi
			movsx accept_lgt(%rip), %ecx
			rep movsb

			pop %rcx
			push %rcx

			mov $0x80, %rdi
			lea (,%rcx,8), %rdx
			lea (%rdx,%rdx), %rdx
			sub %rdx, %rdi
			mov curr_env(%rip), %rdx
			lea (%rdi,%rdx), %rdi

			movsx accept_lgt(%rip), %rcx
			movq %rcx, (%rdi)
			# save str ptr in stack
			movq %r13, 8(%rdi)

			pop %rcx
			dec %rcx
			jnz ef_l_argloop

		jmp ef_incomplete # failed to jump to ef_l_argdone before count was done

		ef_l_argdone:
		# skip over ')' after the args
		push %rcx
		call nextch
		call skip_ws
		pop %rcx

		mov $8, %rbx
		sub %rcx, %rbx # rbx = param count
		mov %rbx, 0x88(%rsp)

		push %rbx

		mov dp(%rip), %rcx
		mov %rcx, prev_dp(%rip)
		lea data(%rip), %rcx
		add dp(%rip), %rcx
		movq %rbx, (%rcx)
		movq ip(%rip), %rbx
		movq %rbx, 8(%rcx) # format is n_params:8, address:8
		addq $16, dp(%rip)

		addq $9, ip(%rip) # from the r15 reinit sequence ABOVE

		lea reloc(%rip), %rdx
		add reloc_ptr(%rip), %rdx
		movq $2, (%rdx) # 64-bit program reloc
		movq %rcx, 8(%rdx)
		addq $16, reloc_ptr(%rip)

		xor %rsi, %rsi

		movsx prev_char(%rip), %rax
		lea char_type_tbl(%rip), %rbx
		movb (%rbx, %rax), %al

		cmpb ct_cpar(%rip), %al
		je ef_incomplete # no expr in the lambda

		movsx expr(%rip), %rax
		call expect

		mov ip(%rip), %rax
		mov %rax, prev_ip(%rip)
		lea program(%rip), %rax
		add ip(%rip), %rax
		test %rbx, %rbx # 0 = call
		# data is on %r12, nothing to do
		jz ef_l_skipmov

		ef_l_checklit:
		dec %rbx # 1 = lit (2 = env)
		jz ef_l_literal

		inc %rdi
		jz ef_l_reg
		dec %rdi
		jmp ef_l_rsp

		ef_l_literal:
		movq mem_to_ret_code(%rip), %rcx
		movq %rcx, (%rax)
		add $3, %rax
		push %rax
		movl %esi, (%rax) #-> mov [literal addr], %r12 (= ret register)
		add $4, %rax
		addq $7, ip(%rip)
		jmp ef_l_reloc

		ef_l_reg:
		lea reg_reg_codes(%rip), %rbx
		# rsi codes are actually backward compared to the convention, so flip it first
		mov %rsi, %rdi
		mov $7, %rsi
		sub %rdi, %rsi
		lea (%rsi,%rsi,2), %rsi
		lea 168(%rbx,%rsi), %rbx # 3*7*8 = 21
		movw (%rbx), %di
		movw %di, (%rax)
		add $2, %rax
		movb 2(%rbx), %dil
		movb %dil, (%rax)
		inc %rax
		jmp ef_l_skipmov

		ef_l_rsp:
		neg %rdi
		lea (,%rdi,8), %rdi
		lea reg_rsp_codes(%rip), %rbx

		mov %rsi, %rcx

		call insert_load

		movl 28(%rbx), %ebx # 4*7=28
		add $3, %rax
		movl %ebx, (%rax) # mov offset(%rsp), %rax
		movl %edi, 4(%rax)
		add $8, %rax
		jmp ef_l_skipmov

		ef_l_reloc:
		pop %rdx
		lea reloc(%rip), %rbx
		add reloc_ptr(%rip), %rbx
		movq $0, (%rbx) # 32-bit reloc
		movq %rdx, 8(%rbx)
		addq $16, reloc_ptr(%rip)

		ef_l_skipmov:
		# Teardown env; note that this leaks the string we had allocated...
		pop %rbx # n_args

		movq curr_env(%rip), %rcx
		movq 0x80(%rcx), %rcx
		movq %rcx, curr_env(%rip)
		add $0x90, %rsp

		# pop %r15
		movb $0x41, (%rax)
		movb $0x5f, 1(%rax)
		add $2, %rax

		movb $0xc3, (%rax) # ret
		inc %rax
		add $3, ip(%rip)

		pop %rcx
		pop %rbx
		sub %rcx, %rax
		movl %eax, (%rbx)

		pop %rsi

		xor %rax, %rax
		xor %rbx, %rbx
		inc %rbx # functions count as literals
		jmp ef_ret

	ef_no_lambda:
	movq $0, pending_fn(%rip) # If this is not a definition for a lambda
	#(which is a special case to enable recursion), disable the early-registration
	#mechanism

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
	jne ef_incomplete
	lea accept_buff(%rip), %rsi
	lea if_str(%rip), %rdi
	repe cmpsb
	jne ef_incomplete

	ef_if:
		call skip_ws

		movsx expr(%rip), %rax
		call expect

		mov ip(%rip), %rax
		mov %rax, prev_ip(%rip)
		lea program(%rip), %rax
		addq ip(%rip), %rax
		movb $0x50, (%rax) # push %rax
		movb $0x49, 1(%rax)
		movb $0xff, 2(%rax)
		movb $0xc7, 3(%rax) # inc %r15
		add $4, %rax

		test %rbx, %rbx # 0 = call, 1 = literal, 2 = env
		je ef_if_from_mem_go

		cmp $1, %rbx
		jne ef_if_from_mem

		lea mem_to_reg_codes(%rip), %rbx
		movq 49(%rbx), %rbx # 7*7=49
		movq %rbx, (%rax) # mov 0x0, %rax
		add $3, %rax
		movl %esi, (%rax) # set the literal's address there to mov that literal into %rax
		push %rax
		add $4, %rax
		jmp ef_if_do_reloc0

		ef_if_from_mem:
		inc %rdi
		jz ef_if_from_mem_doreg
		dec %rdi
		jmp ef_if_from_rsp_go

		ef_if_from_mem_doreg:
		mov $7, %rbx
		sub %rsi, %rbx
		mov %rbx, %rsi
		lea reg_reg_codes(%rip), %rbx
		lea (%rsi,%rsi,2), %rsi
		lea 168(%rbx,%rsi), %rbx # 3*7*8 = 168; + 3*rsi
		movw (%rbx), %di
		movw %di, (%rax)
		add $2, %rax
		movb 2(%rbx), %dil
		movb %dil, (%rax)
		inc %rax
		jmp ef_if_dojmp

		ef_if_from_mem_go:
		# %r12 should contain the right thing already, just mov it to %rax
		lea call_to_reg(%rip), %rbx
		movl (%rbx), %ecx
		movl %ecx, (%rax)
		movb 2(%rbx), %cl
		movb %cl, 2(%rax)
		add $3, %rax
		jmp ef_if_dojmp

		ef_if_from_rsp_go:
		lea reg_rsp_codes(%rip), %rbx

		mov %rsi, %rcx

		call insert_load

		movl 28(%rbx), %ebx # 4*7=28
		add $3, %rax
		movl %ebx, (%rax) # mov offset(%rsp), %rax
		mov $8, %rbx
		sub %rdi, %rbx
		neg %rdi
		lea 8(,%rbx,8), %rdi # +8 because we `push rax` to start with
		movl %edi, 4(%rax)
		add $8, %rax
		jmp ef_if_dojmp

		# add the reloc
		ef_if_do_reloc0:
		pop %rdx
		lea reloc(%rip), %rbx
		add reloc_ptr(%rip), %rbx
		movq $0, (%rbx) # 32-bit reloc
		movq %rdx, 8(%rbx)
		addq $16, reloc_ptr(%rip)

		ef_if_dojmp:
		movb $0x48, (%rax)
		movb $0x8b, 1(%rax)
		movb $0x00, 2(%rax) # mov (%rax), %rax
		add $3, %rax
		# dec %r15 -- has to be done before test but after we eval'd the first form
		movb $0x49, (%rax)
		movb $0xff, 1(%rax)
		movb $0xcf, 2(%rax)
		add $3, %rax
		movb $0x48, (%rax)
		movb $0x85, 1(%rax)
		movb $0xc0, 2(%rax) # test %rax, %rax
		movb $0x58, 3(%rax) # pop %rax -- we no longer need the test var
		add $4, %rax
		movb $0x0f, (%rax)
		movb $0x84, 1(%rax) # je [4 bytes offset]
		add $2, %rax

		# save the jump to inject the false path's address
		push %rax
		add $4, %rax
		push %rax
		lea program(%rip), %rbx
		sub %rbx, %rax
		mov %rax, ip(%rip)

		call skip_ws

		movsx expr(%rip), %rax
		call expect

		lea program(%rip), %rax
		add ip(%rip), %rax

		test %rbx, %rbx # 0 = call, 1 = literal, 2 = env
		jz ef_if_do_jmp2

		cmp $1, %rbx
		jne ef_if_env_true

		lea mem_to_reg_codes(%rip), %rbx
		movq 49(%rbx), %rbx # 7*7 = 49
		movq %rbx, (%rax)
		add $3, %rax
		movl %esi, (%rax)
		push %rax
		add $4, %rax
		lea reg_to_mem_codes(%rip), %rbx
		movq 56(%rbx), %rbx
		movq %rbx, (%rax)
		add $4, %rax

		lea reloc(%rip), %rbx
		add reloc_ptr(%rip), %rbx
		movq $0, (%rbx)
		movq %rax, 8(%rbx)
		addq $16, reloc_ptr(%rip)

		add $4, %rax

		jmp ef_if_do_reloc

		ef_if_env_true:
		inc %rdi
		jz ef_if_true_savereg
		dec %rdi
		jmp ef_if_true_savemem

		ef_if_true_savereg:
		lea reg_to_mem_codes(%rip), %rbx
		movq (%rbx,%rsi,8), %rbx
		movq %rbx, (%rax) # mov %reg, ret(%rip)
		add $4, %rax
		push %rax
		add $4, %rax
		jmp ef_if_do_reloc

		ef_if_true_savemem:
		movb $0x50, (%rax) # push %rax
		inc %rax
		lea reg_rsp_codes(%rip), %rbx

		mov %rsi, %rcx

		call insert_load

		movl 28(%rbx), %ebx # 4*7=28; rsp -> rax
		add $3, %rax
		movl %ebx, (%rax)
		add $4, %rax
		neg %rdi
		lea (,%rdi,8), %rdi
		movl %edi, (%rax)
		add $4, %rax # rsp + offset -> rax
		lea reg_to_mem_codes(%rip), %rbx
		movq 56(%rbx), %rbx # 8*7=56; rax -> ret(%rip)
		movq %rbx, (%rax)
		add $4, %rax
		push %rax
		add $4, %rax
		movb $0x58, (%rax) # pop %rax
		inc %rax

		ef_if_do_reloc:
		pop %rdx

		lea reloc(%rip), %rbx
		add reloc_ptr(%rip), %rbx
		movq $0, (%rbx)
		movq %rdx, 8(%rbx)
		addq $16, reloc_ptr(%rip)

		ef_if_do_jmp2:
		mov %rax, %rdx
		pop %rcx
		pop %rbx
		sub %rcx, %rdx
		add $5, %rdx # offset for the finishing 'jmp [after-condition]' that will be
		# inserted momentarily
		movl %edx, (%rbx)

		movb $0xe9, (%rax)
		inc %rax

		push %rax
		add $4, %rax
		push %rax
		lea program(%rip), %rbx
		sub %rbx, %rax
		mov %rax, ip(%rip)

		call skip_ws

		movsx expr(%rip), %rax
		call expect

		lea program(%rip), %rax
		add ip(%rip), %rax

		test %rbx, %rbx # 0 = call, 1 = literal, 2 = env
		jz ef_if_finish

		cmp $1, %rbx
		jne ef_if_env_false

		lea mem_to_reg_codes(%rip), %rbx
		movq 49(%rbx), %rbx # 7*7 = 49
		movq %rbx, (%rax)
		add $3, %rax
		movl %esi, (%rax)
		push %rax # for reloc
		add $4, %rax
		lea reg_to_mem_codes(%rip), %rbx
		movq 56(%rbx), %rbx
		movq %rbx, (%rax)
		add $4, %rax

		lea reloc(%rip), %rbx # do immediate reloc here since our reloc code
		# only processes one reloc from stack
		add reloc_ptr(%rip), %rbx
		movq $0, (%rbx)
		movq %rax, 8(%rbx)
		addq $16, reloc_ptr(%rip)

		add $4, %rax

		jmp ef_if_do_reloc2

		ef_if_env_false:
		inc %rdi
		jz ef_if_false_savereg
		dec %rdi
		jmp ef_if_false_savemem

		ef_if_false_savereg:
		lea reg_to_mem_codes(%rip), %rbx
		movq (%rbx,%rsi,8), %rbx
		movq %rbx, (%rax) # mov %reg, ret(%rip)
		add $4, %rax
		push %rax
		add $4, %rax
		jmp ef_if_do_reloc2

		ef_if_false_savemem:
		movb $0x50, (%rax) # push %rax
		inc %rax
		lea reg_rsp_codes(%rip), %rbx

		mov %rsi, %rcx

		call insert_load

		movl 28(%rbx), %ebx # 4*7=28; rsp -> rax
		add $3, %rax
		movl %ebx, (%rax)
		add $4, %rax
		neg %rdi
		lea (,%rdi,8), %rdi
		movl %edi, (%rax)
		add $4, %rax # rsp + offset -> rax
		lea reg_to_mem_codes(%rip), %rbx
		movq 56(%rbx), %rbx # 8*7=56; rax -> ret(%rip)
		movq %rbx, (%rax)
		add $4, %rax
		push %rax # Save relocation start relocation
		add $4, %rax
		movb $0x58, (%rax) # pop %rax
		inc %rax

		ef_if_do_reloc2:
		pop %rdx

		lea reloc(%rip), %rbx
		add reloc_ptr(%rip), %rbx
		movq $0, (%rbx)
		movq %rdx, 8(%rbx)
		addq $16, reloc_ptr(%rip)

		ef_if_finish:
		mov %rax, %rdx
		pop %rcx
		pop %rbx
		sub %rcx, %rdx
		movl %edx, (%rbx)
		lea program(%rip), %rbx
		sub %rbx, %rax
		mov %rax, ip(%rip)

		lea program(%rip), %rax
		add ip(%rip), %rax
		#movb $0x58, (%rax) # pop %rax
		#inc %rax
		lea program(%rip), %rbx
		sub %rbx, %rax
		mov %rax, ip(%rip)

		call skip_ws

		movsx prev_char(%rip), %rax
		lea char_type_tbl(%rip), %rbx
		movb (%rax,%rbx), %al

		cmpb ct_cpar(%rip), %al
		jne ef_incomplete

		xor %rax, %rax
		xor %rbx, %rbx
		jmp ef_ret

	ef_nop: # just move the prev_ip forward, no actual code is output here
		mov ip(%rip), %rax
		mov %rax, prev_ip(%rip)
		mov %rax, %rsi
		push %rsi

		call skip_ws

		movsx prev_char(%rip), %rax
		lea char_type_tbl(%rip), %rsi
		movb (%rax,%rsi), %al
		cmpb ct_cpar(%rip), %al
		je ef_end_nop

		jmp ef_incomplete

		ef_end_nop:
		pop %rsi
		xor %rax, %rax
		xor %rbx, %rbx
		jmp ef_ret

	ef_def: # handle 'define' special form
		call skip_ws

		movsx var(%rip), %rax
		call accept

		test %rax, %rax
		jz ef_redef # symbol exist so it's being redefined

		cmp $2, %rax # symbol does not exist, so a new one is being defined (this is OK, too)
		jne ef_incomplete # a different error code is present, no good!

		ef_redef:
		mov n_symbols(%rip), %r12
		mov %r12, pending_fn(%rip)
		incq n_symbols(%rip)

		movsx accept_lgt(%rip), %ecx
		push %rcx

		# malloc new str
		mov %rcx, %rsi
		call my_malloc

		cmp $0, %rax
		jb err_malloc_failed
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

		movsx expr(%rip), %rax
		call accept
	
		test %rax, %rax
		jne ef_incomplete

		push %rsi

		call skip_ws

		pop %rsi
		pop %rdi
		mov %rsi, 16(%rdi)

		movq $0, pending_fn(%rip)

		lea char_type_tbl(%rip), %rbx
		movsx prev_char(%rip), %rax
		movb (%rbx,%rax), %al

		cmpb ct_cpar(%rip), %al
		je ef_def_done

		jmp ef_incomplete

		ef_def_done:
		xor %rax, %rax
		xor %rbx, %rbx
		jmp ef_ret

	ef_call:
		mov ip(%rip), %rax
		mov %rax, prev_ip(%rip)
		lea program(%rip), %rax
		add ip(%rip), %rax

		lea data(%rip), %rdi
		add %rdi, %rsi

		mov (%rsi), %rdi
		pushq 8(%rsi) # function address
		pushq %rdi # count
		pushq %rdi # counter (-> 0)

	# top of stack is the count
	ef_pre_loop:
		lea program(%rip), %rbx
		sub %rbx, %rax
		movq %rax, ip(%rip)

		ef_loop:
		call skip_ws

		lea char_type_tbl(%rip), %rbx
		movsx prev_char(%rip), %rax
		movb (%rbx,%rax), %al

		cmpb ct_cpar(%rip), %al
		je ef_postloop

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
	lea program(%rip), %r8
	addq ip(%rip), %r8
	lea reloc(%rip), %r9
	add reloc_ptr(%rip), %r9

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
	lea reloc(%rip), %r9
	add reloc_ptr(%rip), %r9

	lea program(%rip), %r8
	add ip(%rip), %r8

	push %rcx

	lea reg_codes(%rip), %rbx

	mov curr_env(%rip), %rcx
	test %rcx, %rcx
	je skip_push
	mov 0x88(%rcx), %rcx # cnt (-> idx)
	#mov $8, %rdx
	#sub %rcx, %rdx
	test %rcx, %rcx
	jz skip_addnargs

	# add [n_args], %r15
	movb $0x49, (%r8)
	movb $0x81, 1(%r8)
	movb $0xc7, 2(%r8)
	#mov %rdx, %rcx
	movl %ecx, 3(%r8)
	add $7, %r8

	skip_addnargs:
	mov $8, %rsi
	sub %rcx, %rsi # 8 - cnt
	ef_pushloop:
		lea (%rsi,%rcx), %rdi # (cnt - idx) + (8 - cnt) = 8 - cnt + [idx -> cnt], i.e. [8-cnt to 8]
		dec %rdi

		cmp $6, %rcx
		jb ef_norex

		movb $0x41, (%r8)
		inc %r8

		ef_norex:
		movb $0x50, (%r8) # push
		movb (%rbx,%rdi), %dil
		addb %dil, (%r8) # register
		inc %r8
		jmp ef_end_pushloop

		ef_end_pushloop:
		loop ef_pushloop
	skip_push:

	pop %rcx
	mov %rcx, %rdx
	xor %rcx, %rcx

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
		# rsi = register it came from
		inc %rdi
		test %rdi, %rdi
		jz ef_env_direct
		dec %rdi

		# This was not directly in a reg, so emit mov (%rsp,[%rdi+offset * 8]), [reg]
		lea reg_rsp_codes(%rip), %r10
		movl (%r10,%rdx,4), %eax

		mov %rsi, %rcx
		push %rax
		mov %r8, %rax
		call insert_load
		mov %rax, %r8
		pop %rax

		movl %eax, (%r8)
		neg %rdi
		lea (,%rdi,8), %rdi
		movl %edi, 4(%r8)
		add $8, %r8

		jmp ef_arg_processed

		ef_env_direct:
		# This was directly in a reg whose index is in %rsi, emit the mov
		# note that this isn't a reg-reg mov in case previous args overwrote reg values.
		# The correct effect is to look into the immediate stack (rooted at r15+rsp).

		lea (,%rsi,8), %rsi
		neg %rsi

		lea reg_codes(%rip), %rdi
		inc %rcx
		add %rcx, %rdi
		movsx (%rdi), %rdi

		# mov [-%rsi](%rsp, %r15, 8), [%rcx]
		movb $0x4a, %r11b
		movb $0x4e, %r12b
		cmp $6, %rcx # whether we need the REX.W byte
		cmova %r12, %r11
		movb %r11b, (%r8)
		movb $0x8b, 1(%r8)
		movb $0x44, 2(%r8)
		shl $3, %dil # needed for code compat
		addb %dil, 2(%r8) # + rcx (see above)
		movb $0xfc, 3(%r8)
		sub $8, %sil # offset by 1 stack pos
		movb %sil, 4(%r8) # + offset
		add $5, %r8

		jmp ef_arg_processed

		ef_arg_call: # then it was a call, and ret val is on %r12 so mov it to our reg
		# mov %r12, %rax
		movb $0x4c, (%r8)
		movb $0x89, 1(%r8)
		movb $0xe0, 2(%r8)

		cmp $6, %rbx
		jb ef_arg_norex

		movb $0x4d, (%r8)

		ef_arg_norex:
		lea reg_codes(%rip), %rax
		mov (%rax,%rdx), %rbx
		addq %rbx, 2(%r8)

		add $3, %r8

		ef_arg_processed:
		inc %rdx
		inc %rcx
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

	pop %r9 # cnt

	lea reg_codes(%rip), %rbx
	mov curr_env(%rip), %rcx
	test %rcx, %rcx
	je skip_pop
	mov 0x88(%rcx), %rcx # cnt
	#mov %r9, %rcx

	test %rcx, %rcx
	jz skip_subnargs

	# sub [n_args], %r15
	movb $0x49, (%r8)
	movb $0x81, 1(%r8)
	movb $0xef, 2(%r8)
	movl %ecx, 3(%r8)
	add $7, %r8

	skip_subnargs:
	ef_poploop:
		mov $8, %rdi
		sub %rcx, %rdi # 8 - [idx -> 0] i.e. [8 to (8-cnt)]

		cmp $6, %rcx
		jb ef_norex2

		movb $0x41, (%r8)
		inc %r8

		ef_norex2:
		movb $0x58, (%r8) # pop
		movb (%rbx,%rdi), %sil
		addb %sil, (%r8) # register
		inc %r8
		jmp ef_end_poploop

		ef_end_poploop:
		loop ef_poploop
	skip_pop:

	ef_end:
		lea program(%rip), %r9
		sub %r9, %r8
		movq %r8, ip(%rip)
		xor %rax, %rax
		xor %rbx, %rbx
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
		push %rbx
		push %rax
		push %rsi

		call skip_ws
		call nextch # skip over final ')'

		pop %rsi
		pop %rax
		pop %rbx
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
	mov $malloc_error_lgt, %rdx
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

comma_space: .ascii ", "
colon_space: .ascii ": "

reg_codes:
# rex
.byte 0b1 # r9
.byte 0b0 # r8
# non-rex
.byte 0b111 # rsi
.byte 0b110 # rdi
.byte 0b11 # rbx
.byte 0b10 # rdx
.byte 0b1 # rcx
.byte 0b0 # rax

mem_to_ret_code:
.byte 0x49, 0xc7, 0xc4, 0x00, 0x00, 0x00, 0x00

mem_to_reg_codes:
.byte 0x49, 0xc7, 0xc1, 0x00, 0x00, 0x00, 0x00 # mov 0x0, %r9
.byte 0x49, 0xc7, 0xc0, 0x00, 0x00, 0x00, 0x00 # mov 0x0, %r8
.byte 0x48, 0xc7, 0xc7, 0x00, 0x00, 0x00, 0x00 # mov 0x0, %rdi
.byte 0x48, 0xc7, 0xc6, 0x00, 0x00, 0x00, 0x00 # mov 0x0, %rsi
.byte 0x48, 0xc7, 0xc3, 0x00, 0x00, 0x00, 0x00 # mov 0x0, %rbx
.byte 0x48, 0xc7, 0xc2, 0x00, 0x00, 0x00, 0x00 # mov 0x0, %rdx
.byte 0x48, 0xc7, 0xc1, 0x00, 0x00, 0x00, 0x00 # mov 0x0, %rcx
.byte 0x48, 0xc7, 0xc0, 0x00, 0x00, 0x00, 0x00 # mov 0x0, %rax

reg_to_mem_codes:
.byte 0x49, 0x89, 0x0c, 0x25, 0x00, 0x00, 0x00, 0x00 # mov %r9, 0x0
.byte 0x49, 0x89, 0x04, 0x25, 0x00, 0x00, 0x00, 0x00 # mov %r8, 0x0
.byte 0x48, 0x89, 0x3c, 0x25, 0x00, 0x00, 0x00, 0x00 # mov %rdi, 0x0
.byte 0x48, 0x89, 0x34, 0x25, 0x00, 0x00, 0x00, 0x00 # mov %rsi, 0x0
.byte 0x48, 0x89, 0x1c, 0x25, 0x00, 0x00, 0x00, 0x00 # mov %rbx, 0x0
.byte 0x48, 0x89, 0x14, 0x25, 0x00, 0x00, 0x00, 0x00 # mov %rdx, 0x0
.byte 0x48, 0x89, 0x0c, 0x25, 0x00, 0x00, 0x00, 0x00 # mov %rcx, 0x0
.byte 0x48, 0x89, 0x04, 0x25, 0x00, 0x00, 0x00, 0x00 # mov %rax, 0x0

call_to_reg:
.byte 0x4c, 0x89, 0xe0 # r9 -> r9

reg_reg_codes: # idx = source * 3 * 8 + dest * 3
.byte 0x49, 0x89, 0xc9 # r9 -> r9
.byte 0x49, 0x89, 0xc8 # r9 -> r8
.byte 0x48, 0x89, 0xcf # r9 -> rdi
.byte 0x48, 0x89, 0xce # r9 -> rsi
.byte 0x48, 0x89, 0xcb # r9 -> rbx
.byte 0x48, 0x89, 0xca # r9 -> rdx
.byte 0x48, 0x89, 0xc9 # r9 -> rcx
.byte 0x48, 0x89, 0xc8 # r9 -> rax

.byte 0x49, 0x89, 0xc1 # r8 -> r9
.byte 0x49, 0x89, 0xc0 # r8 -> r8
.byte 0x48, 0x89, 0xc6 # r8 -> rdi
.byte 0x48, 0x89, 0xc7 # r8 -> rsi
.byte 0x48, 0x89, 0xc3 # r8 -> rbx
.byte 0x48, 0x89, 0xc2 # r8 -> rdx
.byte 0x48, 0x89, 0xc1 # r8 -> rcx
.byte 0x48, 0x89, 0xc0 # r8 -> rax

.byte 0x49, 0x89, 0xf9 # rdi -> r9
.byte 0x49, 0x89, 0xf8 # rdi -> r8
.byte 0x48, 0x89, 0xff # rdi -> rdi
.byte 0x48, 0x89, 0xfe # rdi -> rsi
.byte 0x48, 0x89, 0xfb # rdi -> rbx
.byte 0x48, 0x89, 0xfa # rdi -> rdx
.byte 0x48, 0x89, 0xf9 # rdi -> rcx
.byte 0x48, 0x89, 0xf8 # rdi -> rax

.byte 0x49, 0x89, 0xf1 # rsi -> r9
.byte 0x49, 0x89, 0xf0 # rsi -> r8
.byte 0x48, 0x89, 0xf6 # rsi -> rdi
.byte 0x48, 0x89, 0xf7 # rsi -> rsi
.byte 0x48, 0x89, 0xf3 # rsi -> rbx
.byte 0x48, 0x89, 0xf2 # rsi -> rdx
.byte 0x48, 0x89, 0xf1 # rsi -> rcx
.byte 0x48, 0x89, 0xf0 # rsi -> rax

.byte 0x49, 0x89, 0xd9 # rbx -> r9
.byte 0x49, 0x89, 0xd8 # rbx -> r8
.byte 0x48, 0x89, 0xdf # rbx -> rdi
.byte 0x48, 0x89, 0xde # rbx -> rsi
.byte 0x48, 0x89, 0xdb # rbx -> rbx
.byte 0x48, 0x89, 0xda # rbx -> rdx
.byte 0x48, 0x89, 0xd9 # rbx -> rcx
.byte 0x48, 0x89, 0xd8 # rbx -> rax

.byte 0x49, 0x89, 0xd1 # rdx -> r9
.byte 0x49, 0x89, 0xd0 # rdx -> r8
.byte 0x48, 0x89, 0xd6 # rdx -> rdi
.byte 0x48, 0x89, 0xd7 # rdx -> rsi
.byte 0x48, 0x89, 0xd3 # rdx -> rbx
.byte 0x48, 0x89, 0xd2 # rdx -> rdx
.byte 0x48, 0x89, 0xd1 # rdx -> rcx
.byte 0x48, 0x89, 0xd0 # rdx -> rax

.byte 0x49, 0x89, 0xc9 # rcx -> r9
.byte 0x49, 0x89, 0xc8 # rcx -> r8
.byte 0x48, 0x89, 0xcf # rcx -> rdi
.byte 0x48, 0x89, 0xce # rcx -> rsi
.byte 0x48, 0x89, 0xcb # rcx -> rbx
.byte 0x48, 0x89, 0xca # rcx -> rdx
.byte 0x48, 0x89, 0xc9 # rcx -> rcx
.byte 0x48, 0x89, 0xc8 # rcx -> rax

.byte 0x49, 0x89, 0xc1 # rax -> r9
.byte 0x49, 0x89, 0xc0 # rax -> r8
.byte 0x48, 0x89, 0xc6 # rax -> rdi
.byte 0x48, 0x89, 0xc7 # rax -> rsi
.byte 0x48, 0x89, 0xc3 # rax -> rbx
.byte 0x48, 0x89, 0xc2 # rax -> rdx
.byte 0x48, 0x89, 0xc1 # rax -> rcx
.byte 0x48, 0x89, 0xc0 # rax -> rax

reg_imm_codes: # load this, then append literal val
.byte 0b01001001, 0b10111001
.byte 0b01001001, 0b10111000
.byte 0b01001000, 0b10111111
.byte 0b01001000, 0b10111110
.byte 0b01001000, 0b10111011
.byte 0b01001000, 0b10111010
.byte 0b01001000, 0b10111001
.byte 0b01001000, 0b10111000

reg_rsp_codes: # mov offset(%rsp,%r13,8), [reg]. append 32-bit offset at end.
.byte 0x4e, 0x8b, 0x8c, 0xec # -> r9
.byte 0x4e, 0x8b, 0x84, 0xec # -> r8
.byte 0x4a, 0x8b, 0xbc, 0xec # -> rdi
.byte 0x4a, 0x8b, 0xb4, 0xec # -> rsi
.byte 0x4a, 0x8b, 0x9c, 0xec # -> rbx
.byte 0x4a, 0x8b, 0x94, 0xec # -> rdx
.byte 0x4a, 0x8b, 0x8c, 0xec # -> rcx
.byte 0x4a, 0x8b, 0x84, 0xec # -> rax

last_charnum: .quad 0
last_linum: .quad 0

ten: .quad 10
