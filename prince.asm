;NOTE
; pointer to start of input (in SRAM):		r25:r24
; pointer to start of the key (in SRAM):	r23:r22	
; pointer to start of SRAM:					r21:r20

.org 0000

#define round		r0

#define counter		r17
#define input_byte	r18
#define tmp			r19

#define key_byte	r26

; sbox substitution
#define sbox_addr_l	r5
#define sbox_addr_h	r6
#define sbox_byte	r26

; key2SRAM
#define k_7			r16		; msb
#define k_6			r15
#define k_5			r14
#define k_4			r13
#define k_3			r12
#define k_2			r11
#define k_1			r10
#define k_0			r9		; lsb
#define k_msb		r17

; matrix multiplication
#define cnt			r3
#define i_1			r2
#define i_0			r1
#define res_3		r18
#define res_2		r17
#define res_1		r16
#define res_0		r26
#define m			r9

; add round constant
#define rc_byte		r26

; shift rows
#define m_h			r16
#define m_l			r18

; save state
#define res			r26


;################################################################# CONSTANTS #################################################################

#define rc_offset			0x40	; round constants R1 to R11
#define sbox_offset			0x98	; sbox
#define sr_offset			0x198
#define k1_offset			0x08
.cseg
jmp		simulation

matrix:
	.db 0x7b, 0xde, 0xbd, 0xe7, 0xde, 0x7b, 0xe7, 0xbd	; ^m0
	.db 0xbd, 0xe7, 0xde, 0x7b, 0xe7, 0xbd, 0x7b, 0xde	; ^m1
	.db 0xbd, 0xe7, 0xde, 0x7b, 0xe7, 0xbd, 0x7b, 0xde	; ^m1
	.db 0x7b, 0xde, 0xbd, 0xe7, 0xde, 0x7b, 0xe7, 0xbd	; ^m0

round_constants:
	.db 0x13, 0x19, 0x8a, 0x2e, 0x03, 0x70, 0x73, 0x44	; RC1
	.db 0xa4, 0x09, 0x38, 0x22, 0x29, 0x9f, 0x31, 0xd0
	.db 0x08, 0x2e, 0xfa, 0x98, 0xec, 0x4e, 0x6c, 0x89
	.db 0x45, 0x28, 0x21, 0xe6, 0x38, 0xd0, 0x13, 0x77
	.db 0xbe, 0x54, 0x66, 0xcf, 0x34, 0xe9, 0x0c, 0x6c
	.db 0x7e, 0xf8, 0x4f, 0x78, 0xfd, 0x95, 0x5c, 0xb1
	.db 0x85, 0x84, 0x08, 0x51, 0xf1, 0xac, 0x43, 0xaa
	.db 0xc8, 0x82, 0xd3, 0x2f, 0x25, 0x32, 0x3c, 0x54
	.db 0x64, 0xa5, 0x11, 0x95, 0xe0, 0xe3, 0x61, 0x0d
	.db 0xd3, 0xb5, 0xa3, 0x99, 0xca, 0x0c, 0x23, 0x99
	.db 0xc0, 0xac, 0x29, 0xb7, 0xc9, 0x7c, 0x50, 0xdd	; RC11
	; [ 88 byte ]

sbox:
	.db 0xbb, 0xbf, 0xb3, 0xb2, 0xba, 0xbc, 0xb9, 0xb1, 0xb6, 0xb7, 0xb8, 0xb0, 0xbe, 0xb5, 0xbd, 0xb4
	.db 0xfb, 0xff, 0xf3, 0xf2, 0xfa, 0xfc, 0xf9, 0xf1, 0xf6, 0xf7, 0xf8, 0xf0, 0xfe, 0xf5, 0xfd, 0xf4
	.db 0x3b, 0x3f, 0x33, 0x32, 0x3a, 0x3c, 0x39, 0x31, 0x36, 0x37, 0x38, 0x30, 0x3e, 0x35, 0x3d, 0x34
	.db 0x2b, 0x2f, 0x23, 0x22, 0x2a, 0x2c, 0x29, 0x21, 0x26, 0x27, 0x28, 0x20, 0x2e, 0x25, 0x2d, 0x24
	.db 0xab, 0xaf, 0xa3, 0xa2, 0xaa, 0xac, 0xa9, 0xa1, 0xa6, 0xa7, 0xa8, 0xa0, 0xae, 0xa5, 0xad, 0xa4
	.db 0xcb, 0xcf, 0xc3, 0xc2, 0xca, 0xcc, 0xc9, 0xc1, 0xc6, 0xc7, 0xc8, 0xc0, 0xce, 0xc5, 0xcd, 0xc4
	.db 0x9b, 0x9f, 0x93, 0x92, 0x9a, 0x9c, 0x99, 0x91, 0x96, 0x97, 0x98, 0x90, 0x9e, 0x95, 0x9d, 0x94
	.db 0x1b, 0x1f, 0x13, 0x12, 0x1a, 0x1c, 0x19, 0x11, 0x16, 0x17, 0x18, 0x10, 0x1e, 0x15, 0x1d, 0x14
	.db 0x6b, 0x6f, 0x63, 0x62, 0x6a, 0x6c, 0x69, 0x61, 0x66, 0x67, 0x68, 0x60, 0x6e, 0x65, 0x6d, 0x64
	.db 0x7b, 0x7f, 0x73, 0x72, 0x7a, 0x7c, 0x79, 0x71, 0x76, 0x77, 0x78, 0x70, 0x7e, 0x75, 0x7d, 0x74
	.db 0x8b, 0x8f, 0x83, 0x82, 0x8a, 0x8c, 0x89, 0x81, 0x86, 0x87, 0x88, 0x80, 0x8e, 0x85, 0x8d, 0x84
	.db 0x0b, 0x0f, 0x03, 0x02, 0x0a, 0x0c, 0x09, 0x01, 0x06, 0x07, 0x08, 0x00, 0x0e, 0x05, 0x0d, 0x04
	.db 0xeb, 0xef, 0xe3, 0xe2, 0xea, 0xec, 0xe9, 0xe1, 0xe6, 0xe7, 0xe8, 0xe0, 0xee, 0xe5, 0xed, 0xe4
	.db 0x5b, 0x5f, 0x53, 0x52, 0x5a, 0x5c, 0x59, 0x51, 0x56, 0x57, 0x58, 0x50, 0x5e, 0x55, 0x5d, 0x54
	.db 0xdb, 0xdf, 0xd3, 0xd2, 0xda, 0xdc, 0xd9, 0xd1, 0xd6, 0xd7, 0xd8, 0xd0, 0xde, 0xd5, 0xdd, 0xd4
	.db 0x4b, 0x4f, 0x43, 0x42, 0x4a, 0x4c, 0x49, 0x41, 0x46, 0x47, 0x48, 0x40, 0x4e, 0x45, 0x4d, 0x44
	; [ 256 byte ]
	
sbox_inv:
	.db 0xbb, 0xb7, 0xb3, 0xb2, 0xbf, 0xbd, 0xb8, 0xb9, 0xba, 0xb6, 0xb4, 0xb0, 0xb5, 0xbe, 0xbc, 0xb1
	.db 0x7b, 0x77, 0x73, 0x72, 0x7f, 0x7d, 0x78, 0x79, 0x7a, 0x76, 0x74, 0x70, 0x75, 0x7e, 0x7c, 0x71
	.db 0x3b, 0x37, 0x33, 0x32, 0x3f, 0x3d, 0x38, 0x39, 0x3a, 0x36, 0x34, 0x30, 0x35, 0x3e, 0x3c, 0x31
	.db 0x2b, 0x27, 0x23, 0x22, 0x2f, 0x2d, 0x28, 0x29, 0x2a, 0x26, 0x24, 0x20, 0x25, 0x2e, 0x2c, 0x21
	.db 0xfb, 0xf7, 0xf3, 0xf2, 0xff, 0xfd, 0xf8, 0xf9, 0xfa, 0xf6, 0xf4, 0xf0, 0xf5, 0xfe, 0xfc, 0xf1
	.db 0xdb, 0xd7, 0xd3, 0xd2, 0xdf, 0xdd, 0xd8, 0xd9, 0xda, 0xd6, 0xd4, 0xd0, 0xd5, 0xde, 0xdc, 0xd1
	.db 0x8b, 0x87, 0x83, 0x82, 0x8f, 0x8d, 0x88, 0x89, 0x8a, 0x86, 0x84, 0x80, 0x85, 0x8e, 0x8c, 0x81
	.db 0x9b, 0x97, 0x93, 0x92, 0x9f, 0x9d, 0x98, 0x99, 0x9a, 0x96, 0x94, 0x90, 0x95, 0x9e, 0x9c, 0x91
	.db 0xab, 0xa7, 0xa3, 0xa2, 0xaf, 0xad, 0xa8, 0xa9, 0xaa, 0xa6, 0xa4, 0xa0, 0xa5, 0xae, 0xac, 0xa1
	.db 0x6b, 0x67, 0x63, 0x62, 0x6f, 0x6d, 0x68, 0x69, 0x6a, 0x66, 0x64, 0x60, 0x65, 0x6e, 0x6c, 0x61
	.db 0x4b, 0x47, 0x43, 0x42, 0x4f, 0x4d, 0x48, 0x49, 0x4a, 0x46, 0x44, 0x40, 0x45, 0x4e, 0x4c, 0x41
	.db 0x0b, 0x07, 0x03, 0x02, 0x0f, 0x0d, 0x08, 0x09, 0x0a, 0x06, 0x04, 0x00, 0x05, 0x0e, 0x0c, 0x01
	.db 0x5b, 0x57, 0x53, 0x52, 0x5f, 0x5d, 0x58, 0x59, 0x5a, 0x56, 0x54, 0x50, 0x55, 0x5e, 0x5c, 0x51
	.db 0xeb, 0xe7, 0xe3, 0xe2, 0xef, 0xed, 0xe8, 0xe9, 0xea, 0xe6, 0xe4, 0xe0, 0xe5, 0xee, 0xec, 0xe1
	.db 0xcb, 0xc7, 0xc3, 0xc2, 0xcf, 0xcd, 0xc8, 0xc9, 0xca, 0xc6, 0xc4, 0xc0, 0xc5, 0xce, 0xcc, 0xc1
	.db 0x1b, 0x17, 0x13, 0x12, 0x1f, 0x1d, 0x18, 0x19, 0x1a, 0x16, 0x14, 0x10, 0x15, 0x1e, 0x1c, 0x11
	; [ 256 byte ]


;################################################################# SIMULATION ################################################################

; the key
key:
	.db 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00
	.db 0xFE, 0xDC, 0xBA, 0x98, 0x76, 0x54, 0x32, 0x10

; plaintext or ciphertext
input:
	;.db 0x01, 0x23, 0x45, 0x67, 0x89, 0xab, 0xcd, 0xef
	.db 0xae, 0x25, 0xad, 0x3c, 0xa8, 0xfa, 0x9c, 0xcf

simulation:

	; stack initialization
	ldi		r16, high( ramend )
	out		sph, r16

	ldi		r16, low( ramend )
	out		spl, r16

	;--------------------------------------------------------------------------------

	; load address of input data in program memory into z-pointer
	ldi		zl, low( input << 1 )
	ldi		zh, high( input << 1 )

	; load address of SRAM where input data will be placed into r25:r24
	ldi		r24, 0x00
	ldi		r25, 0x01

	; load input data from program memory into SRAM
	mov		yl, r24
	mov		yh, r25

	ldi		r17, 8

	init_loop1:
		lpm		r16, z+
		st		y+, r16
		dec		r17
		brne	init_loop1

	;--------------------------------------------------------------------------------

	; load address of key in program memory into z-pointer
	ldi		zl, low( key << 1 )
	ldi		zh, high( key << 1 )

	; load address of SRAM where key will be placed into r23:r22
	ldi		r22, 0x10
	ldi		r23, 0x01

	; load input data from program memory into SRAM
	mov		yl, r22
	mov		yh, r23

	ldi		r17, 16

	init_loop2:
		lpm		r16, z+
		st		y+, r16
		dec		r17
		brne	init_loop2

	;--------------------------------------------------------------------------------

	; save address of start of free SRAM
	mov		r20, yl
	mov		r21, yh

jmp		Project_dec

;################################################################# Project_enc ###############################################################
Project_enc:
	call	load_matrix
	call	load_round_constants
	call	load_sbox

	; use k0 as whitening key
	call	add_k0

;################################################################ PRINCE_core ################################################################
	; initialize round counter
	eor		round, round
	inc		round

	call	add_k1

	; round 1
	call	sbox_substitution
	call	matrix_mult
	call	shift_rows
	call	add_round_constant
	call	add_k1
	inc		round

	; round 2
	call	sbox_substitution
	call	matrix_mult
	call	shift_rows
	call	add_round_constant
	call	add_k1
	inc		round

	; round 3
	call	sbox_substitution
	call	matrix_mult
	call	shift_rows
	call	add_round_constant
	call	add_k1
	inc		round

	; round 4
	call	sbox_substitution
	call	matrix_mult
	call	shift_rows
	call	add_round_constant
	call	add_k1
	inc		round

	; round 5
	call	sbox_substitution
	call	matrix_mult
	call	shift_rows
	call	add_round_constant
	call	add_k1
	inc		round

	; middle part
	call	sbox_substitution
	call	matrix_mult
	call	load_sbox_inv
	call	sbox_substitution

	; round 6
	call	add_round_constant
	call	add_k1
	call	shift_rows_inv
	call	matrix_mult
	call	sbox_substitution
	inc		round

	; round 7
	call	add_k1
	call	add_round_constant
	call	shift_rows_inv
	call	matrix_mult
	call	sbox_substitution
	inc		round

	; round 8
	call	add_k1
	call	add_round_constant
	call	shift_rows_inv
	call	matrix_mult
	call	sbox_substitution
	inc		round

	; round 9
	call	add_k1
	call	add_round_constant
	call	shift_rows_inv
	call	matrix_mult
	call	sbox_substitution
	inc		round

	; round 10
	call	add_k1
	call	add_round_constant
	call	shift_rows_inv
	call	matrix_mult
	call	sbox_substitution
	inc		round

	call	add_round_constant
	call	add_k1

	; use k0' as whitening key
	call	add_k0_bar

	ret


;################################################################# Project_dec ###############################################################
Project_dec:
	call	load_matrix
	call	load_round_constants
	call	load_sbox

	; use k0' as whitening key
	call	add_k0_bar

;################################################################ PRINCE_core ################################################################
	; initialize round counter
	ldi		tmp, 11
	mov		round, tmp

	call	add_k1
	call	add_round_constant
	dec		round

	; round 1
	call	sbox_substitution
	call	matrix_mult
	call	shift_rows
	call	add_round_constant
	call	add_k1
	dec		round

	; round 2
	call	sbox_substitution
	call	matrix_mult
	call	shift_rows
	call	add_round_constant
	call	add_k1
	dec		round

	; round 3
	call	sbox_substitution
	call	matrix_mult
	call	shift_rows
	call	add_round_constant
	call	add_k1
	dec		round

	; round 4
	call	sbox_substitution
	call	matrix_mult
	call	shift_rows
	call	add_round_constant
	call	add_k1
	dec		round

	; round 5
	call	sbox_substitution
	call	matrix_mult
	call	shift_rows
	call	add_round_constant
	call	add_k1
	dec		round

	; middle part
	call	sbox_substitution
	call	matrix_mult
	call	load_sbox_inv
	call	sbox_substitution

	; round 6
	call	add_round_constant
	call	add_k1
	call	shift_rows_inv
	call	matrix_mult
	call	sbox_substitution
	dec		round

	; round 7
	call	add_k1
	call	add_round_constant
	call	shift_rows_inv
	call	matrix_mult
	call	sbox_substitution
	dec		round

	; round 8
	call	add_k1
	call	add_round_constant
	call	shift_rows_inv
	call	matrix_mult
	call	sbox_substitution
	dec		round

	; round 9
	call	add_k1
	call	add_round_constant
	call	shift_rows_inv
	call	matrix_mult
	call	sbox_substitution
	dec		round

	; round 10
	call	add_k1
	call	add_round_constant
	call	shift_rows_inv
	call	matrix_mult
	call	sbox_substitution
	dec		round

	call	add_k1

	; use k0 as whitening key
	call	add_k0

	ret


;########################################################### store matrix in SRAM ############################################################
load_matrix:
	; load address of matrix in program memory into y-pointer
	ldi		zl, low( matrix << 1 )
	ldi		zh, high( matrix << 1 )

	; load address of SRAM where matrix will be placed into z-pointer
	mov		yl, r20
	mov		yh, r21

	ldi		counter, 64

	load_matrix_loop:
		lpm		tmp, z+
		st		y+, tmp

		dec		counter
		brne	load_matrix_loop
	
	ret


;####################################################### store round constants in SRAM #######################################################
load_round_constants:
	; load address of rc in program memory into y-pointer
	ldi		zl, low( round_constants << 1 )
	ldi		zh, high( round_constants << 1 )

	; load address of SRAM where rc will be placed into z-pointer
	mov		yl, r20
	mov		yh, r21
	ldi		tmp, low( rc_offset )
	add		yl, tmp
	ldi		tmp, high( rc_offset )
	adc		yh, tmp

	ldi		counter, 88

	load_round_constants_loop:
		lpm		tmp, z+
		st		y+, tmp

		dec		counter
		brne	load_round_constants_loop

	ret


;############################################################ store sbox in SRAM #############################################################
load_sbox:
	; load address of sbox in program memory into y-pointer
	ldi		zl, low( sbox << 1 )
	ldi		zh, high( sbox << 1 )

	; load address of SRAM where sbox will be placed into z-pointer
	mov		yl, r20
	mov		yh, r21
	ldi		tmp, low( sbox_offset )
	add		yl, tmp
	ldi		tmp, high( sbox_offset )
	adc		yh, tmp

	ldi		counter, 255

	load_sbox_loop:
		lpm		tmp, z+
		st		y+, tmp

		dec		counter
		brne	load_sbox_loop
	
	lpm		tmp, z
	st		y, tmp

	ret


;####################################################### store inverted sbox in SRAM #########################################################
load_sbox_inv:
	; load address of inverted sbox in program memory into y-pointer
	ldi		zl, low( sbox_inv << 1 )
	ldi		zh, high( sbox_inv << 1 )

	; load address of SRAM where inverted sbox will be placed into z-pointer
	mov		yl, r20
	mov		yh, r21
	ldi		tmp, low( sbox_offset )
	add		yl, tmp
	ldi		tmp, high( sbox_offset )
	adc		yh, tmp

	ldi		counter, 255

	load_sbox_inv_loop:
		lpm		tmp, z+
		st		y+, tmp

		dec		counter
		brne	load_sbox_inv_loop

	lpm		tmp, z
	st		y, tmp

	ret


;############################################################ sbox substitution ##############################################################
sbox_substitution:
	; load address of SRAM where input is stored into y-pointer
	mov		yl, r24
	mov		yh, r25

	; load address of SRAM where sbox is stored
	mov		sbox_addr_l, r20
	mov		sbox_addr_h, r21
	ldi		tmp, low( sbox_offset )
	add		sbox_addr_l, tmp
	ldi		tmp, high( sbox_offset )
	adc		sbox_addr_h, tmp

	; initialize counter
	ldi		counter, 8

	sbox_substitution_loop:
		; load input byte
		ld		input_byte, y

		; place z-pointer on respective sbox byte
		mov		zl, sbox_addr_l
		mov		zh, sbox_addr_h

		eor		tmp, tmp
		add		zl, input_byte
		adc		zh, tmp

		; update input byte with sbox byte
		ld		sbox_byte, z+
		
		st		y+, sbox_byte

		dec		counter
		brne	sbox_substitution_loop

	ret


;########################################################## matrix multiplication ############################################################
matrix_mult:
	; load address of SRAM where input is stored into y-pointer
 	mov		yl, r24
	mov		yh, r25

	; load address of SRAM where matrix is stored into z-pointer
	mov		zl, r20
	mov		zh, r21

	call	matrix_processing
	call	matrix_processing
	call	matrix_processing
	call	matrix_processing

	ret

	matrix_processing:
		; load 2 input bytes
		ld		i_1, y
		ldd		i_0, y+1

		eor		res_3, res_3	; clear result register
		
		ld		m, z+			; load matrix entry
		mov		tmp, i_1
		and		tmp, m
		eor		res_3, tmp

		ld		m, z+			; load matrix entry
		mov		tmp, i_0
		and		tmp, m
		eor		res_3, tmp

		mov		tmp, res_3
		andi	res_3, 0xf0
		lsl		tmp
		lsl		tmp
		lsl		tmp
		lsl		tmp

		eor		res_3, tmp
		;-----------------------------------------------------

		eor		res_2, res_2

		ld		m, z+			; load matrix entry
		mov		tmp, i_1
		and		tmp, m
		eor		res_2, tmp

		ld		m, z+			; load matrix entry
		mov		tmp, i_0
		and		tmp, m
		eor		res_2, tmp

		mov		tmp, res_2
		andi	res_2, 0x0f
		lsr		tmp
		lsr		tmp
		lsr		tmp
		lsr		tmp

		eor		res_2, tmp
		;-----------------------------------------------------

		eor		res_1, res_1

		ld		m, z+			; load matrix entry
		mov		tmp, i_1
		and		tmp, m
		eor		res_1, tmp

		ld		m, z+			; load matrix entry
		mov		tmp, i_0
		and		tmp, m
		eor		res_1, tmp

		mov		tmp, res_1
		andi	res_1, 0xf0
		lsl		tmp
		lsl		tmp
		lsl		tmp
		lsl		tmp

		eor		res_1, tmp
		;-----------------------------------------------------

		eor		res_0, res_0

		ld		m, z+			; load matrix entry
		mov		tmp, i_1
		and		tmp, m
		eor		res_0, tmp

		ld		m, z+			; load matrix entry
		mov		tmp, i_0
		and		tmp, m
		eor		res_0, tmp

		mov		tmp, res_0
		andi	res_0, 0x0f
		lsr		tmp
		lsr		tmp
		lsr		tmp
		lsr		tmp

		eor		res_0, tmp
		;-----------------------------------------------------

		or		res_3, res_2
		or		res_1, res_0

		st		y+, res_3
		st		y+, res_1
		;--------------------------------------------------------------------------------------------------------------

		ret

;################################################################## add k1 ###################################################################
add_k1:
	; load address of SRAM where input is stored into y-pointer
	mov		yl, r24
	mov		yh, r25

	; load address of SRAM where key is stored into z-pointer
	mov		zl, r22
	mov		zh, r23
	adiw	z, k1_offset

	; initialize counter
	ldi		counter, 8

	round_func_m_xor_k1:
		ld		key_byte, z+
		ld		input_byte, y

		eor		input_byte, key_byte

		st		y+, input_byte

		dec		counter
		brne	round_func_m_xor_k1

	ret


;############################################################## round constant ###############################################################
add_round_constant:
	; load address of SRAM where input is stored into y-pointer
	mov		yl, r24
	mov		yh, r25

	; load address of SRAM where rc is stored into z-pointer
	mov		zl, r20
	mov		zh, r21
	ldi		tmp, low( rc_offset )
	add		zl, tmp
	ldi		tmp, high( rc_offset )
	adc		zh, tmp

	; initialize counter
	ldi		counter, 8

	mov		tmp, round

	cpi		tmp, 1
	breq	add_round_constant_rndx

	adiw	z, 8
	cpi		tmp, 2
	breq	add_round_constant_rndx

	adiw	z, 8
	cpi		tmp, 3
	breq	add_round_constant_rndx

	adiw	z, 8
	cpi		tmp, 4
	breq	add_round_constant_rndx

	adiw	z, 8
	cpi		tmp, 5
	breq	add_round_constant_rndx

	adiw	z, 8
	cpi		tmp, 6
	breq	add_round_constant_rndx

	adiw	z, 8
	cpi		tmp, 7
	breq	add_round_constant_rndx

	adiw	z, 8
	cpi		tmp, 8
	breq	add_round_constant_rndx

	adiw	z, 8
	cpi		tmp, 9
	breq	add_round_constant_rndx

	adiw	z, 8
	cpi		tmp, 10
	breq	add_round_constant_rndx

	adiw	z, 8

	add_round_constant_rndx:
		ld		rc_byte, z+
		ld		input_byte, y

		eor		input_byte, key_byte

		st		y+, input_byte

		dec		counter
		brne	add_round_constant_rndx
		
	ret


;################################################################ shift rows #################################################################
shift_rows:
	; load address of SRAM where input is stored into y-pointer
	mov		yl, r24
	mov		yh, r25

	; load address of SRAM for input expansion into z-pointer
	mov		zl, r20
	mov		zh, r21
	ldi		tmp, low( sr_offset )
	add		zl, tmp
	ldi		tmp, high( sr_offset )
	adc		zh, tmp

	ldi		counter, 8

	; save expanded input into SRAM
	shift_rows_loop:
		ld		m_h, y+
		mov		m_l, m_h

		andi	m_h, 0xf0
		lsr		m_h
		lsr		m_h
		lsr		m_h
		lsr		m_h

		andi	m_l, 0x0f

		st		z+, m_h
		st		z+, m_l

		dec		counter
		brne	shift_rows_loop
	
	; load address of input in SRAM into y-pointer
	mov		yl, r24
	mov		yh, r25

	; load address of SRAM of input expansion into z-pointer
	mov		zl, r20
	mov		zh, r21
	ldi		tmp, low( sr_offset )
	add		zl, tmp
	ldi		tmp, high( sr_offset )
	adc		zh, tmp

	; 0, 5
	ld		m_h, z
	ldd		m_l, z+5

	lsl		m_h
	lsl		m_h
	lsl		m_h
	lsl		m_h

	or		m_l, m_h

	st		y+, m_l

	; 10, 15
	ldd		m_h, z+10
	ldd		m_l, z+15

	lsl		m_h
	lsl		m_h
	lsl		m_h
	lsl		m_h

	or		m_l, m_h

	st		y+, m_l

	; 4, 9
	ldd		m_h, z+4
	ldd		m_l, z+9

	lsl		m_h
	lsl		m_h
	lsl		m_h
	lsl		m_h

	or		m_l, m_h

	st		y+, m_l	

	; 14, 3
	ldd		m_h, z+14
	ldd		m_l, z+3

	lsl		m_h
	lsl		m_h
	lsl		m_h
	lsl		m_h

	or		m_l, m_h

	st		y+, m_l	

	; 8, 13
	ldd		m_h, z+8
	ldd		m_l, z+13

	lsl		m_h
	lsl		m_h
	lsl		m_h
	lsl		m_h

	or		m_l, m_h

	st		y+, m_l	

	; 2, 7
	ldd		m_h, z+2
	ldd		m_l, z+7

	lsl		m_h
	lsl		m_h
	lsl		m_h
	lsl		m_h

	or		m_l, m_h

	st		y+, m_l	

	; 12, 1
	ldd		m_h, z+12
	ldd		m_l, z+1

	lsl		m_h
	lsl		m_h
	lsl		m_h
	lsl		m_h

	or		m_l, m_h

	st		y+, m_l	

	; 6, 11
	ldd		m_h, z+6
	ldd		m_l, z+11

	lsl		m_h
	lsl		m_h
	lsl		m_h
	lsl		m_h

	or		m_l, m_h

	st		y+, m_l	

	ret


;############################################################### shift rows inv ###############################################################
shift_rows_inv:
	; load address of SRAM where input is stored into y-pointer
	mov		yl, r24
	mov		yh, r25


	mov		zl, r20
	mov		zh, r21
	ldi		tmp, low( sr_offset )
	add		zl, tmp
	ldi		tmp, high( sr_offset )
	adc		zh, tmp

	ldi		counter, 8

	shift_rows_inv_loop:
		ld		m_h, y+
		mov		m_l, m_h

		andi	m_h, 0xf0
		lsr		m_h
		lsr		m_h
		lsr		m_h
		lsr		m_h

		andi	m_l, 0x0f

		st		z+, m_h
		st		z+, m_l

		dec		counter
		brne	shift_rows_inv_loop

	mov		yl, r24
	mov		yh, r25

	mov		zl, r20
	mov		zh, r21
	ldi		tmp, low( sr_offset )
	add		zl, tmp
	ldi		tmp, high( sr_offset )
	adc		zh, tmp

	; 0, 13
	ld		m_h, z
	ldd		m_l, z+13

	lsl		m_h
	lsl		m_h
	lsl		m_h
	lsl		m_h

	or		m_l, m_h

	st		y+, m_l

	; 10, 7
	ldd		m_h, z+10
	ldd		m_l, z+7

	lsl		m_h
	lsl		m_h
	lsl		m_h
	lsl		m_h

	or		m_l, m_h

	st		y+, m_l

	; 4, 1
	ldd		m_h, z+4
	ldd		m_l, z+1

	lsl		m_h
	lsl		m_h
	lsl		m_h
	lsl		m_h

	or		m_l, m_h

	st		y+, m_l	

	; 14, 11
	ldd		m_h, z+14
	ldd		m_l, z+11

	lsl		m_h
	lsl		m_h
	lsl		m_h
	lsl		m_h

	or		m_l, m_h

	st		y+, m_l	

	; 8, 5
	ldd		m_h, z+8
	ldd		m_l, z+5

	lsl		m_h
	lsl		m_h
	lsl		m_h
	lsl		m_h

	or		m_l, m_h

	st		y+, m_l	

	; 2, 15
	ldd		m_h, z+2
	ldd		m_l, z+15

	lsl		m_h
	lsl		m_h
	lsl		m_h
	lsl		m_h

	or		m_l, m_h

	st		y+, m_l	

	; 12, 9
	ldd		m_h, z+12
	ldd		m_l, z+9

	lsl		m_h
	lsl		m_h
	lsl		m_h
	lsl		m_h

	or		m_l, m_h

	st		y+, m_l	

	; 6, 3
	ldd		m_h, z+6
	ldd		m_l, z+3

	lsl		m_h
	lsl		m_h
	lsl		m_h
	lsl		m_h

	or		m_l, m_h

	st		y+, m_l

	ret


;################################################################# m xor k0 ##################################################################
add_k0:
	; load address of SRAM where input is stored into y-pointer
	mov		yl, r24
	mov		yh, r25

	; load address of SRAM where k0 is stored into z-pointer
	mov		zl, r22
	mov		zh, r23

	; initialize counter
	ldi		counter, 8

	add_k0_loop:
		ld		key_byte, z+
		ld		input_byte, y

		eor		input_byte, key_byte

		st		y+, input_byte

		dec		counter
		brne	add_k0_loop

	ret


;################################################################# m xor k0' #################################################################
add_k0_bar:
	; load address of SRAM where input is stored into y-pointer
	mov		yl, r24
	mov		yh, r25

	; load address of SRAM where key is stored into z-pointer
	mov		zl, r22
	mov		zh, r23

	ld		k_7, z+
	ld		k_6, z+
	ld		k_5, z+
	ld		k_4, z+
	ld		k_3, z+
	ld		k_2, z+
	ld		k_1, z+
	ld		k_0, z

	;---------------------------------- compute k0' ----------------------------------
	eor		tmp, tmp

	; save msb of k0
	mov		k_msb, k_7
	andi	k_msb, 0x80
	tst		k_msb
	breq	key2SRAM_rorK0
	ldi		k_msb, 0x01

	; rotate right k0 by 1 byte
	key2SRAM_rorK0:
		ror		k_7
		ror		k_6
		ror		k_5
		ror		k_4
		ror		k_3
		ror		k_2
		ror		k_1
		ror		k_0

	brcc	add_k0_bar_nocarry
	ldi		tmp, 0x80

	add_k0_bar_nocarry:
		or		k_7, tmp
		eor		k_0, k_msb

	;---------------------------------- m xor k0' ----------------------------------
	ld		input_byte, y
	eor		input_byte, k_7
	st		y+, input_byte

	ld		input_byte, y
	eor		input_byte, k_6
	st		y+, input_byte

	ld		input_byte, y
	eor		input_byte, k_5
	st		y+, input_byte

	ld		input_byte, y
	eor		input_byte, k_4
	st		y+, input_byte

	ld		input_byte, y
	eor		input_byte, k_3
	st		y+, input_byte

	ld		input_byte, y
	eor		input_byte, k_2
	st		y+, input_byte

	ld		input_byte, y
	eor		input_byte, k_1
	st		y+, input_byte

	ld		input_byte, y
	eor		input_byte, k_0
	st		y, input_byte

	ret
