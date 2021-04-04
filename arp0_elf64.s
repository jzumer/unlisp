.global make_header
.global make_footer

.text

make_header:
	mov ip(%rip), %rax
	mov %rax, prev_ip(%rip)
	mov %rax, %rbx
	lea program(%rip), %rax
	add %rbx, %rax
	
	# File header
	movb $0x7f, (%rax)
	movb $'E', 1(%rax)
	movb $'L', 2(%rax)
	movb $'F', 3(%rax)
	add $4, %rax
	movb $2, (%rax) # 64-bit
	inc %rax
	movb $1, (%rax) # little endian
	inc %rax
	movb $1, (%rax) # always 1
	inc %rax
	movb $0, (%rax) # platform -- 0 = disregard/no special extension
	inc %rax
	movb $0, (%rax) # abi version -- mostly unused except glibc 2.12+ for dynlink
	inc %rax
	movl $0x00000000, (%rax)
	movw $0x0000, 4(%rax)
	movb $0x00, 6(%rax) # padding -- 7 bytes total
	add $7, %rax
	movw $0x01, (%rax) # Relocatable
	add $2, %rax
	movw $0x3e, (%rax) # Target ISA = amd64
	movl $1, (%rax) # always 1
	add $4, %rax
	movq $0, (%rax) # XXX: This is he entry point
	add $8, %rax
	movq $0x40, (%rax) # offset to program header -- this follows the ELF header and is thus at 0x40 in 64-bit (0x34 in 32-bit).
	add $8, %rax
	movq $0x0, (%rax) # XXX: start of section header table
	add $8, %rax
	movl $0x0, (%rax) # arch-specific (here: skip)
	add $4, %rax
	movw $64, (%rax) # size of header: 64 for 64-bit, 52 for 32-bit
	add $2, (%rax)
	movw $0x38, (%rax) # size of program header entry: 0x38 for 64-bit, 0x20 for 32-bit.
	movw $2, 2(%rax) # how many entries (.text and .data only here)
	movw $0x40, 4(%rax) # size of section header entry: 0x40 for 64-bit, 0x28 for 32-bit.
	movw $4, 6(%rax) # how many sections: .text, .data, .shrtrtab, .reloc.tab XXX: will need symb and str for reloc.
	movw $2, 8(%rax) # index in section table where section names are located, i.e. index of .shrtrtab in the string table
	add $8, %rax

	# Program headers
	# .text
	movl $1, (%rax) # Segment type, here PT_LOAD
	add $4, %rax
	movl $5, (%rax) # PF_W | PF_X i.e. .text
	add $4, %rax
	# v 0xB0 if 2, 0xE8 if 3, 0x78 if 1, i.e. 0x40 for file header and 0x38 per program header
	movq $0xB0, (%rax) # offset from start of file until first byte of segment, i.e. until right after all the program headers
	add $8, %rax
	movq $0, (%rax) # XXX: vmem start address = 0 i.e. immediate program start.
	#XXX: have to fix the way we generate code to either setup a trampoline of sorts
	# or split data and code
	movq $0, 8(%rax) # physical memory -- basically ignored
	add $16, %rax
	movq $0, (%rax) # XXX: size of segment on disk
	movq $0, 8(%rax) # XXX: size of segement in ram (same as above in practice)
	add $16, %rax
	movq $0, (%rax) # no alignment required
	add $8, %rax

	# .data
	movl $1, (%rax) # Segment type, here PT_LOAD
	add $4, %rax
	movl $6, (%rax) # PF_W | PF_R i.e. .data
	add $4, %rax
	movq $0xB0, (%rax) # XXX same as previous header, but add size of .text as well
	add $8, %rax
	movq $0, (%rax) # XXX add size of .text
	movq $0, 8(%rax) # physical memory -- basically ignored
	add $16, %rax
	movq $0, (%rax) # XXX: size of segment on disk
	movq $0, 8(%rax) # XXX: size of segement in ram (same as above in practice)
	add $16, %rax
	movq $0, (%rax) # no alignment required
	add $8, %rax

	mov %rax, ip(%rip)

	ret

make_footer:
	mov ip(%rip), %rax
	mov %rax, prev_ip(%rip)
	mov %rax, %rbx
	lea program(%rip), %rax
	add %rbx, %rax

	movsx text_str_lgt(%rip), %ecx
	lea text_str(%rip), %rsi
	mov %rax, %rdi
	rep movsb

	add text_str_lgt(%rip), %rax

	movsx data_str_lgt(%rip), %ecx
	lea data_str(%rip), %rsi
	mov %rax, %rdi
	rep movsb

	add data_str_lgt(%rip), %rax

	movsx shstrtab_str_lgt(%rip), %ecx
	lea shstrtab_str(%rip), %rsi
	mov %rax, %rdi
	rep movsb

	add shstrtab_str_lgt(%rip), %rax

	movsx reloctab_str_lgt(%rip), %ecx
	lea reloctab_str(%rip), %rsi
	mov %rax, %rdi
	rep movsb

	add reloctab_str_lgt(%rip), %rax

	# XXX: insert relocations here

	# section headers
	# .text
	movl $0, (%rax) # name offset: 0 for ".text" -- relative to shstrtab start
	movl $1, 4(%rax) # section type: program data
	add $8, %rax
	movq $6, (%rax) # ALLOC | EXEC
	add $8, %rax
	movq $0, (%rax) # vaddr start
	movq $0xB0, 8(%rax) # section offset in image (same as in header XXX verify)
	movq $0, 16(%rax) # XXX size of section
	add $24, %rax
	movq $0, (%rax) # sh_link, sh_info -- not used
	add $8, %rax
	movq $0, (%rax) # no alignment
	movq $0, 8(%rax) # not a table of fixed-size elements
	add $16, %rax

	mov text_str_lgt, %rcx # rcx is the current offset

	# .data
	movl %ecx, (%rax) # name offset for ".data" -- relative to shstrtab start
	movl $1, 4(%rax) # section type: program data
	add $8, %rax
	movq $2, (%rax) # ALLOC
	add $8, %rax
	movq $0, (%rax) # XXX vaddr start
	movq $0xB0, 8(%rax) # XXX: section offset in image (same as in header XXX verify)
	movq $0, 16(%rax) # XXX size of section
	add $24, %rax
	movq $0, (%rax) # sh_link, sh_info -- not used
	add $8, %rax
	movq $0, (%rax) # no alignment
	movq $0, 8(%rax) # not a table of fixed-size elements
	add $16, %rax

	add data_str_lgt, %rcx

	# .shstrtab
	movl %ecx, (%rax) # name offset for ".shstrtab" -- relative to shstrtab start
	movl $0x3, 4(%rax) # section type: string table
	add $8, %rax
	movq $0x20, (%rax) # Contains strings
	add $8, %rax
	movq $0, (%rax) # XXX vaddr start
	movq $0xB0, 8(%rax) # XXX section offset in image (same as in header XXX verify)
	movq $0, 16(%rax) # XXX size of section
	add $24, %rax
	movq $0, (%rax) # sh_link, sh_info -- not used
	add $8, %rax
	movq $0, (%rax) # no alignment
	movq $0, 8(%rax) # not a table of fixed-size elements
	add $16, %rax

	add shstrtab_str_lgt, %rcx

	# .reloc.tab
	movl %ecx, (%rax) # name offset 0 for ".reloc.tab" -- relative to shstrtab start
	movl $9, 4(%rax) # section type: relocation data without addend
	add $8, %rax
	movq $6, (%rax) # ALLOC
	add $8, %rax
	movq $0, (%rax) # XXX vaddr start
	movq $0xB0, 8(%rax) # XXX section offset in image (same as in header XXX verify)
	movq $0, 16(%rax) # XXX size of section
	add $24, %rax
	movq $0, (%rax) # XXX: sh_link, sh_info -- should point to symtbl and reloc target.
	add $8, %rax
	movq $0, (%rax) # no alignment
	movq $0, 8(%rax) # not a table of fixed-size elements
	add $16, %rax

	# XXX: will require an extra symtbl and its associated strtbl for relocations.

	ret

.data
section_names:
text_str: .ascii ".", "text\0"
text_str_lgt = . - text_str
data_str: .asciz ".", "data\0"
data_str_lgt = . - data_str
shstrtab_str: .asciz ".", "shstrtab\0"
shstrtab_str_lgt = . - shstrtab_str
reloctab_str: .asciz ".", "reloc.tab\0"
reloctab_str_lgt = . - reloctab_str
section_names_end:
