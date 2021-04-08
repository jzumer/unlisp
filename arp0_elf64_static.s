.global make_header
.global make_footer

.text

make_header:
	lea header(%rip), %rax
	add header_ptr(%rip), %rax
	
	# File header
	movb $0x7f, (%rax)
	movb $'E', 1(%rax)
	movb $'L', 2(%rax)
	movb $'F', 3(%rax)
	add $4, %rax
	movb $2, (%rax) # 64-bit
	movb $1, 1(%rax) # little endian
	add $2, %rax
	movb $1, (%rax) # always 1
	movb $3, 1(%rax) # platform/OS ABI -- 3 = linux
	add $2, %rax
	movq $0, (%rax) # abi version -- mostly unused except glibc 2.12+ for dynlink; followed by 7 bits of padding
	add $8, %rax
	movw $0x02, (%rax) # Relocatable = 1, exec = 2, dynamic = 3. gcc seems to prefer dyn for relocatable assemblies. Exec is for non-reloc.
	add $2, %rax
	movw $0x3e, (%rax) # Target ISA = amd64
	add $2, %rax
	movl $1, (%rax) # always 1
	add $4, %rax
	#movq $0xB0, %rbx
	movq $0x400000, %rbx
	addq form_ptr(%rip), %rbx
	addq $0xb0, %rbx
	#movq $0x0, %rbx
	#addq form_ptr(%rip), %rbx #  + last function = entry point, good or bad? probably bad, XXX
	movq %rbx, (%rax) # This is he entry point -- 0x40 + 0x38 * 2 p_headers = 0xB0
	add $8, %rax
	movq $0x40, (%rax) # offset to program header -- this follows the ELF header and is thus at 0x40 in 64-bit (0x34 in 32-bit).
	add $8, %rax
	movq $0xb0, %rbx
	addq ip(%rip), %rbx
	addq dp(%rip), %rbx
	addq $section_names_size, %rbx
	movq %rbx, (%rax) # section table start, i.e. end of program, thus headers size + program size + data size + strings size.
	#movq $0, (%rax) # disable section table
	add $8, %rax
	movl $0x0, (%rax) # arch-specific (here: skip)
	add $4, %rax
	movw $0x40, (%rax) # size of this file header: 64 for 64-bit, 52 for 32-bit
	add $2, %rax
	movw $0x38, (%rax) # size of program header entry: 0x38 for 64-bit, 0x20 for 32-bit.
	movw $2, 2(%rax) # how many entries (.text and .data only here since we are static, dynamic also needs data reloc, text reloc at least)
	movw $0x40, 4(%rax) # size of section header entry: 0x40 for 64-bit, 0x28 for 32-bit.
	movw $4, 6(%rax) # how many sections: <null>, .text, .data, .shrtrtab
	movw $3, 8(%rax) # index in section table where section names are located, i.e. index of .shrtrtab in the string table
	add $10, %rax

	# Program headers
	# .text
	movl $1, (%rax) # Segment type, here PT_LOAD
	add $4, %rax
	movl $5, (%rax) # PF_W | PF_X i.e. .text
	add $4, %rax
	#movq $0xB0, (%rax) # offset from start of file until first byte of segment, i.e. until right after all the program headers
	#movq $0xb0, (%rax)
	movq $0x00, (%rax)
	add $8, %rax
	#movq $0x4000b0, (%rax) # vmem start address
	movq $0x400000, (%rax) # vmem start address
	#movq $0x0, (%rax)
	movq $0x400000, 8(%rax) # physical memory -- basically ignored
	add $16, %rax
	movq ip(%rip), %rbx
	movq %rbx, (%rax) # size of segment on disk
	movq %rbx, 8(%rax) # size of segement in ram (same as above in practice)
	add $16, %rax
	movq $0x0, (%rax) # no alignment required
	add $8, %rax

	# .data
	movl $1, (%rax) # Segment type, here PT_LOAD
	add $4, %rax
	movl $6, (%rax) # PF_W | PF_R i.e. .data
	add $4, %rax
	#movq $0xb0, %rbx
	#movq $0x00, %rbx
	movq $0x1000, %rbx
	#addq ip(%rip), %rbx
	movq %rbx, (%rax) # offset: same as previous header, but add size of .text as well
	add $8, %rax
	#movq ip(%rip), %rbx
	movq $0x601000, %rbx
	movq %rbx, (%rax) # XXX add size of .text
	movq %rbx, 8(%rax) # physical memory -- basically ignored
	add $16, %rax
	movq dp(%rip), %rbx
	movq %rbx, (%rax) # size of segment on disk
	movq %rbx, 8(%rax) # size of segement in ram (same as above in practice, no .bss for now)
	add $16, %rax
	movq $0x0, (%rax) # no alignment required
	add $8, %rax

	lea header(%rip), %rbx
	sub %rbx, %rax
	movq %rax, header_ptr(%rip)

	ret

make_footer:
	lea footer(%rip), %rax
	add footer_ptr(%rip), %rax
	mov %rax, %rdi

	mov $null_str_lgt, %ecx
	lea null_str(%rip), %rsi
	rep movsb

	mov $text_str_lgt, %ecx
	lea text_str(%rip), %rsi
	rep movsb

	mov $data_str_lgt, %ecx
	lea data_str(%rip), %rsi
	rep movsb

	mov $shstrtab_str_lgt, %ecx
	lea shstrtab_str(%rip), %rsi
	rep movsb

	mov %rdi, %rax

	# section headers
	# <null>
	#movl $0, (%rax)
	#movl $0, 4(%rax)
	#movq $0, 12(%rax)
	#movq $0, 20(%rax)
	#movq $0, 28(%rax)
	#movq $0, 32(%rax)
	#movq $0, 40(%rax)
	#movq $0, 48(%rax)
	#movq $0, 56(%rax)

	add $0x40, %rax
	mov $null_str_lgt, %rcx # string pointer

	# .text
	movl %ecx, (%rax) # name offset:  ".text" is right after <null>
	movl $1, 4(%rax) # section type: program data
	add $8, %rax
	movq $6, (%rax) # ALLOC | EXEC
	add $8, %rax
	movq $0x4000b0, (%rax) # vaddr start
	#movq $0x0, (%rax)
	movq $0xb0, 8(%rax) # section offset in image (same as in header XXX verify)
	movq ip(%rip), %rbx
	movq %rbx, 16(%rax) # section size
	add $24, %rax
	movq $0, (%rax) # sh_link, sh_info -- not used
	add $8, %rax
	movq $1, (%rax) # no alignment
	movq $0, 8(%rax) # not a table of fixed-size elements
	add $16, %rax

	add $text_str_lgt, %rcx

	# .data
	movl %ecx, (%rax) # name offset for ".data" -- relative to shstrtab start
	movl $1, 4(%rax) # section type: program data
	add $8, %rax
	movq $3, (%rax) # ALLOC | WRITE
	add $8, %rax
	#movq ip(%rip), %rbx
	movq $0x601000, %rbx
	movq %rbx, (%rax) # vaddr start
	movq $0xb0, %rbx
	addq ip(%rip), %rbx
	#movq $0x1000, %rbx
	movq %rbx, 8(%rax) # same as previous header, but add size of .text as well (XXX: verify)
	movq dp(%rip), %rbx
	movq %rbx, 16(%rax) # size of section
	add $24, %rax
	movq $0, (%rax) # sh_link, sh_info -- not used
	add $8, %rax
	movq $1, (%rax) # no alignment
	movq $0, 8(%rax) # not a table of fixed-size elements
	add $16, %rax

	add $data_str_lgt, %rcx

	# .shstrtab
	movl %ecx, (%rax) # name offset for ".shstrtab" -- relative to shstrtab start
	movl $0x3, 4(%rax) # section type: string table
	add $8, %rax
	#movq $0x20, (%rax) # Contains strings
	movq $0x0, (%rax) # Apparently correct flag here is null and not 'contains strings'...
	add $8, %rax
	movq $0, (%rax) # vaddr start -- no alloc so irrelevant
	movq $0xb0, %rbx
	addq ip(%rip), %rbx
	addq dp(%rip), %rbx
	movq %rbx, 8(%rax) # section offset in image (same as in header XXX verify)
	movq $section_names_size, 16(%rax) # size of section
	add $24, %rax
	movq $0, (%rax) # sh_link, sh_info -- not used
	add $8, %rax
	movq $1, (%rax) # no alignment
	movq $0, 8(%rax) # not a table of fixed-size elements
	add $16, %rax

	#add shstrtab_str_lgt, %rcx

	lea footer(%rip), %rbx
	sub %rbx, %rax
	movq %rax, footer_ptr(%rip)

	ret

.data
section_names:
null_str: .asciz ""
null_str_lgt = . - null_str
text_str: .ascii ".", "text\0"
text_str_lgt = . - text_str
data_str: .ascii ".", "data\0"
data_str_lgt = . - data_str
shstrtab_str: .ascii ".", "shstrtab\0"
shstrtab_str_lgt = . - shstrtab_str
section_names_end:
section_names_size = . - section_names

test_str: .ascii "TEST"
test_str_lgt = . - test_str
