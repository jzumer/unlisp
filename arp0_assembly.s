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
.global pending_fn

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
.global do_str
.global do_str_lgt

.data

prev_ip: .quad 0 # instruction pointer starting at previous insertion point
ip: .quad last_code_rel - program # instruction pointer at the end of previous insertion

prev_dp: .quad 104 # data pointer starting at previous insertion point
dp: .quad 104 # current data pointer -- dp = 0 reserved for functions' returned values, dp = 8 for print function, etc.

define_str: .ascii "def"
define_str_lgt: .quad . - define_str
lambda_str: .ascii "fn"
lambda_str_lgt: .quad . - lambda_str
nop_str: .ascii "nop"
nop_str_lgt: .quad . - nop_str
if_str: .ascii "if"
if_str_lgt: .quad . - if_str
do_str: .ascii "do"
do_str_lgt: .quad . - do_str

print_str: .ascii "print"
print_str_lgt: .quad . - print_str
dec_str: .ascii "dec"
dec_str_lgt: .quad . - dec_str
inc_str: .ascii "inc"
inc_str_lgt: .quad . - inc_str
not_str: .ascii "not"
not_str_lgt: .quad . - not_str
less_str: .ascii "<"
less_str_lgt: .quad . - less_str
greater_str: .ascii ">"
greater_str_lgt: .quad . - greater_str

n_symbols: .quad 6

symbol_tbl:
.quad print_str, print_str_lgt - print_str, print_code_data - data
.quad dec_str, dec_str_lgt - dec_str, dec_code_data - data
.quad inc_str, inc_str_lgt - inc_str, inc_code_data - data
.quad not_str, not_str_lgt - not_str, not_code_data - data
.quad less_str, less_str_lgt - less_str, less_code_data - data
.quad greater_str, greater_str_lgt - greater_str, greater_code_data - data
.space 2400, 0 # space for 100 symbols

program:
.byte 0xe9 # "entry point": jmp to start of program, i.e. past the predefined functions
.long 149 #XXX Careful, this must point to 1 past the last code. Can't be calculated...
print_code_rel: # 'what' in %rax with format (quad)size, actual_str. For sys_write, 'what' is in %rsi and 'how much' in %rdx.
.byte 0x52, 0x56, 0x57 # push %rdx, push %rdi, push %rsi
.byte 0x48, 0x8b, 0x10 # mov (%rax), %rdx
.byte 0x48, 0x8d, 0x70, 0x08 # lea 8(%rax), %rsi
.byte 0x48, 0x31, 0xc0 # xor %rax, %rax
.byte 0x48, 0xff, 0xc0 # inc %rax -> sys_write
.byte 0x48, 0x31, 0xff # xor %rdi, %rdi
.byte 0x48, 0xff, 0xc7 # inc %rdi -> stdout
.byte 0x0f, 0x05 # syscall
.byte 0x48, 0xc7, 0x04, 0x25, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00 # mov $0, ret
.byte 0x5f, 0x5e, 0x5a # pop %rsi, pop %rdi, pop %rdx
.byte 0xc3 # ret
dec_code_rel:
.byte 0x48, 0x8b, 0x00 # mov (%rax), %rax
.byte 0x48, 0xff, 0xc8 # dec %rax
.byte 0x48, 0x89, 0x04, 0x25, 0x00, 0x00, 0x00, 0x00 # mov %rax, ret
.byte 0xc3 # ret
inc_code_rel:
.byte 0x48, 0x8b, 0x00 # mov (%rax), %rax
.byte 0x48, 0xff, 0xc0 # inc %rax
.byte 0x48, 0x89, 0x04, 0x25, 0x00, 0x00, 0x00, 0x00 # mov %rax, ret
.byte 0xc3 # ret
not_code_rel:
.byte 0x48, 0x8b, 0x00 # mov (%rax), %rax
.byte 0x48, 0xf7, 0xd0 # not %rax
.byte 0x48, 0x89, 0x04, 0x25, 0x00, 0x00, 0x00, 0x00 # mov %rax, ret
.byte 0xc3 # ret
less_code_rel:
.byte 0x48, 0x8b, 0x00 # mov (%rax), %rax
.byte 0x48, 0x8b, 0x09 # mov (%rcx), %rcx
.byte 0x48, 0x39, 0xc8 # cmp %rcx, %rax
.byte 0x48, 0x31, 0xc0 # xor %rax, %rax
.byte 0x48, 0xc7, 0xc1, 0x01, 0x00, 0x00, 0x00 # mov $1, %rcx
.byte 0x48, 0x0f, 0x4c, 0xc1 # cmovl %rcx, %rax
.byte 0x48, 0x89, 0x04, 0x25, 0x00, 0x00, 0x00, 0x00 # mov %rax, ret
.byte 0xc3 # ret
greater_code_rel:
.byte 0x48, 0x8b, 0x00 # mov (%rax), %rax
.byte 0x48, 0x8b, 0x09 # mov (%rcx), %rcx
.byte 0x48, 0x39, 0xc8 # cmp %rcx, %rax
.byte 0x48, 0x31, 0xc0 # xor %rax, %rax
.byte 0x48, 0xc7, 0xc1, 0x01, 0x00, 0x00, 0x00 # mov $1, %rcx
.byte 0x48, 0x0f, 0x4f, 0xc1 # cmovg %rcx, %rax
.byte 0x48, 0x89, 0x04, 0x25, 0x00, 0x00, 0x00, 0x00 # mov %rax, ret
.byte 0xc3 # ret
last_code_rel:
.space 65536 # Enough space for the bootstrap program

header_ptr: .quad 0
footer_ptr: .quad 0

reloc_ptr: .quad 96
reloc:
.quad 0x0, print_code_rel + 0x1c # 32-bit reloc in the print above for the ret
.quad 0x0, dec_code_rel + 0x0a
.quad 0x0, inc_code_rel + 0x0a
.quad 0x0, not_code_rel + 0x0a
.quad 0x0, less_code_rel + 0x1b
.quad 0x0, greater_code_rel + 0x1b
.space 65536 # format is (type, address) where type is 0 if 32-bit and 1 otherwise

pending_fn: .quad 0 # if !=0, the index in the symbol table where the lambda will
# preemptively insert itself.

data:
.quad 0 # ret
print_code_data:
.quad 0x1, print_code_rel - program
dec_code_data:
.quad 0x1, dec_code_rel - program
inc_code_data:
.quad 0x1, inc_code_rel - program
not_code_data:
.quad 0x1, not_code_rel - program
less_code_data:
.quad 0x2, less_code_rel - program
greater_code_data:
.quad 0x2, greater_code_rel - program
.space 65536 # Enough space for the bootstrap data

.bss
header: .space 2048
footer: .space 2048
