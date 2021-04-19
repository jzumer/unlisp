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
.global n_symbols

.global define_str
.global define_str_lgt
.global print_str
.global print_str_lgt
.global lambda_str
.global lambda_str_lgt
.global nop_str
.global nop_str_lgt
.global if_str
.global if_str_lgt

.data

prev_ip: .quad 0 # instruction pointer starting at previous insertion point
ip: .quad last_code_rel - program # instruction pointer at the end of previous insertion

prev_dp: .quad 24 # data pointer starting at previous insertion point
dp: .quad 24 # current data pointer -- dp = 0 reserved for functions' returned values, dp = 8 for print function

define_str: .ascii "def"
define_str_lgt: .quad . - define_str
print_str: .ascii "print"
print_str_lgt: .quad . - print_str
lambda_str: .ascii "fn"
lambda_str_lgt: .quad . - lambda_str
nop_str: .ascii "nop"
nop_str_lgt: .quad . - nop_str
if_str: .ascii "if"
if_str_lgt: .quad . - if_str

n_symbols: .quad 1

symbol_tbl:
.quad print_str, print_str_lgt - print_str, print_code_data - data
.space 2048, 0

program:
.byte 0xe9 # "entry point": jmp to start of program, i.e. past the predefined functions
.long 34 #XXX Careful, this must point to 1 past the last code
print_code_rel: # 'what' in %rax with format (quad)size, actual_str. For sys_write, 'what' is in %rsi and 'how much' in %rdx.
.byte 0x48, 0x8b, 0x10 # mov (%rax), %rdx
.byte 0x48, 0x8d, 0x70, 0x08 # lea 8(%rax), %rsi
.byte 0x48, 0x31, 0xc0 # xor %rax, %rax
.byte 0x48, 0xff, 0xc0 # inc %rax -> sys_write
.byte 0x48, 0x31, 0xff # xor %rdi, %rdi
.byte 0x48, 0xff, 0xc7 # inc %rdi -> stdout
.byte 0x0f, 0x05 # syscall
.byte 0x48, 0xc7, 0x04, 0x25, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00 # mov $0, ret
.byte 0xc3 # ret
last_code_rel:
.space 65536 # Enough space for the bootstrap program

header_ptr: .quad 0
footer_ptr: .quad 0

reloc_ptr: .quad 16
reloc: .quad 0x0, program + 0x1e # 32-bit reloc in the print above for the ret
.space 65536 # format is (type, address) where type is 0 if 32-bit and 1 otherwise

data:
.quad 0 # ret
print_code_data:
.quad 0x1, print_code_rel - program
.space 65536 # Enough space for the bootstrap data

.bss
header: .space 1024
footer: .space 1024
