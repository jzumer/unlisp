.global footer
.global header
.global footer_ptr
.global header_ptr
.global program
.global data
.global ip
.global prev_ip
.global dp
.global prev_dp

.global reloc
.global reloc_ptr

.global symbol_tbl

.global define_str
.global define_str_lgt
.global print_str
.global print_str_lgt

.data

prev_ip: .quad 0 # instruction pointer starting at previous insertion point
ip: .quad last_code_rel - program # instruction pointer at the end of previous insertion

prev_dp: .quad 0 # data pointer starting at previous insertion point
dp: .quad 0 # current data pointer

define_str: .ascii "def"
define_str_lgt: .quad . - define_str
print_str: .ascii "print"
print_str_lgt: .quad . - print_str

symbol_tbl:
.quad print_str, print_str_lgt - print_str, print_code_rel - program
.space 2048, 0

program:
print_code_rel: # 'what' in %rax with format (quad)size, actual_str. For sys_write, 'what' is in %rsi and 'how much' in %rdx.
.byte 0x48, 0x8b, 0x10 # mov (%rax), %rdx
.byte 0x48, 0x8d, 0x70, 0x08 # lea 8(%rax), %rsi
.byte 0x48, 0x31, 0xc0 # xor %rax, %rax
.byte 0x48, 0xff, 0xc0 # inc %rax -> sys_write
.byte 0x48, 0x31, 0xff # xor %rdi, %rdi
.byte 0x48, 0xff, 0xc7 # inc %rdi -> stdout
.byte 0x0f, 0x05 # syscall
.byte 0xc3 # ret
last_code_rel:
.space 65536, 0 # Enough space for the bootstrap program

header_ptr: .quad 0
footer_ptr: .quad 0

reloc_ptr: .quad 0

.bss
data:
.space 65536, 0 # Enough space for the bootstrap data

header: .space 1024
footer: .space 1024

reloc: .space 65536
