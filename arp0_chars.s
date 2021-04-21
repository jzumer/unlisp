.global char_class_tbl
.global char_type_tbl

.global char_classes
.global char_types

.global ct_none
.global ct_opar
.global ct_cpar
.global ct_nl
.global ct_squote # string quote
.global ct_sesc # string escape
.global ct_comment # comment character

.global cc_symb
.global cc_white
.global cc_char
.global cc_num
.global cc_ctrl

.data
char_classes:
cc_ctrl: .byte 0
cc_white: .byte 1
cc_num: .byte 2
cc_char: .byte 3
cc_symb: .byte 4
cc_eot: .byte 255

char_types:
ct_none: .byte 0
ct_opar: .byte 1
ct_cpar: .byte 2
ct_nl: .byte 3
ct_squote: .byte 4
ct_sesc: .byte 5
ct_comment: .byte 6

char_class_tbl:
.byte 0 #\NUL
.byte 0 #\SOH
.byte 0 #\STX
.byte 0 #\ETX
.byte 255 #\EOT
.byte 0 #\ENQ
.byte 0 #\ACK
.byte 0 #\BEL
.byte 1 #\BS
.byte 1 #\TAB
.byte 1 #\LF
.byte 1 #\VTAB
.byte 1 #\FF
.byte 1 #\CR
.byte 0 #\SO
.byte 0 #\SI
.byte 0 #\DLE
.byte 0 #\DC1
.byte 0 #\DC2
.byte 0 #\DC3
.byte 0 #\DC4
.byte 0 #\NAK
.byte 0 #\SYN
.byte 0 #\ETB
.byte 0 #\CAN
.byte 0 #\EM
.byte 0 #\SUB
.byte 0 #\ESC
.byte 0 #\FS
.byte 0 #\GS
.byte 0 #\RS
.byte 0 #\US
.byte 1 #\space
.byte 4 #exclam
.byte 4 #"
.byte 4 ##
.byte 4 #$
.byte 4 #%
.byte 4 #&
.byte 4 #'
.byte 4 #(
.byte 4 #)
.byte 4 #*
.byte 4 #+
.byte 4 #,
.byte 4 #-
.byte 4 #.
.byte 4 #/
.byte 2 #0
.byte 2
.byte 2
.byte 2
.byte 2
.byte 2
.byte 2
.byte 2
.byte 2
.byte 2 #9
.byte 4 #:
.byte 4 #;
.byte 4 #<
.byte 4 #=
.byte 4 #>
.byte 4 #?
.byte 4 #@
.byte 3 #A
.byte 3
.byte 3
.byte 3
.byte 3
.byte 3
.byte 3
.byte 3
.byte 3
.byte 3
.byte 3
.byte 3
.byte 3
.byte 3
.byte 3
.byte 3
.byte 3
.byte 3
.byte 3
.byte 3
.byte 3
.byte 3
.byte 3
.byte 3
.byte 3
.byte 3 #Z
.byte 4 #[
.byte 4 #\
.byte 4 #]
.byte 4 #^
.byte 4	#_
.byte 4 #`
.byte 3 #a
.byte 3
.byte 3
.byte 3
.byte 3
.byte 3
.byte 3
.byte 3
.byte 3
.byte 3
.byte 3
.byte 3
.byte 3
.byte 3
.byte 3
.byte 3
.byte 3
.byte 3
.byte 3
.byte 3
.byte 3
.byte 3
.byte 3
.byte 3
.byte 3
.byte 3 #z
.byte 4 #{
.byte 4 #|
.byte 4 #}
.byte 4 #~
.byte 0 #\Del

char_type_tbl:
.byte 0 #\NUL
.byte 0 #\SOH
.byte 0 #\STX
.byte 0 #\ETX
.byte 0 #\EOT
.byte 0 #\ENQ
.byte 0 #\ACK
.byte 0 #\BEL
.byte 0 #\BS
.byte 0 #\TAB
.byte 3 #\LF
.byte 0 #\VTAB
.byte 3 #\FF
.byte 3 #\CR
.byte 0 #\SO
.byte 0 #\SI
.byte 0 #\DLE
.byte 0 #\DC1
.byte 0 #\DC2
.byte 0 #\DC3
.byte 0 #\DC4
.byte 0 #\NAK
.byte 0 #\SYN
.byte 0 #\ETB
.byte 0 #\CAN
.byte 0 #\EM
.byte 0 #\SUB
.byte 0 #\ESC
.byte 0 #\FS
.byte 0 #\GS
.byte 0 #\RS
.byte 0 #\US
.byte 0 #\space
.byte 0 #excl
.byte 4 #"
.byte 0 ##
.byte 0 #$
.byte 0 #%
.byte 0 #&
.byte 0 #'
.byte 1 #(
.byte 2 #)
.byte 0 #*
.byte 0 #+
.byte 0 #,
.byte 0 #-
.byte 0 #.
.byte 0 #/
.byte 0 #0
.byte 0
.byte 0
.byte 0
.byte 0
.byte 0
.byte 0
.byte 0
.byte 0
.byte 0 #9
.byte 0 #:
.byte 6 #;
.byte 0 #<
.byte 0 #=
.byte 0 #>
.byte 0 #?
.byte 0 #@
.byte 0 #A
.byte 0
.byte 0
.byte 0
.byte 0
.byte 0
.byte 0
.byte 0
.byte 0
.byte 0
.byte 0
.byte 0
.byte 0
.byte 0
.byte 0
.byte 0
.byte 0
.byte 0
.byte 0
.byte 0
.byte 0
.byte 0
.byte 0
.byte 0
.byte 0
.byte 0 #Z
.byte 0 #[
.byte 5 #\
.byte 0 #]
.byte 0 #^
.byte 0	#_
.byte 0 #`
.byte 0 #a
.byte 0
.byte 0
.byte 0
.byte 0
.byte 0
.byte 0
.byte 0
.byte 0
.byte 0
.byte 0
.byte 0
.byte 0
.byte 0
.byte 0
.byte 0
.byte 0
.byte 0
.byte 0
.byte 0
.byte 0
.byte 0
.byte 0
.byte 0
.byte 0
.byte 0 #z
.byte 0 #{
.byte 0 #|
.byte 0 #}
.byte 0 #~
.byte 0 #\Del
