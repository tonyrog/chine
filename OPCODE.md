# Chine opcode table

## opcode 00xxxxxx

    dup      0
	rot      1
	over     2
	drop     3
	swap     4
	-        5
	+        6
	*        7
	nop      8
	and      9
	or       10
	xor      11
	0=       12
	0<       13
	not      14
	U/A      15       unassigned
	negate   16
	/        17       integer division
	shift    18
	!        19
	@        20
	>r       21
	r>       22
	r@       23
	exit     24
	sys      25
	yield    26
	[]       27
	execute  28
	fp@      29
	fp!      30
	sp@      31
	sp!      32

## opcode 11xxxyyy

Two of the first 0..7 opcodes can be packed into one byte.

    dup      0
	rot      1
	over     2
	drop     3
	swap     4
	-        5
	+        6
	*        7

## opcode 01eejjjj

This group uses the two middle bits ee as a binary exponent
to code number of bytes that follow the code code.

    ee      0    1 byte signed argument length
    ee      1    2 bytes signed argument (big endian)
    ee      2    4 bytes signed argument (big endian)
    ee      3    unassigned

The jjjj (jump) opcodes are

	jmpz     0
	jmpnz    1
	next     2
	jmplz    3
	jmp      4
	call     5
	literal  6  <integer>
	array    7  <length> <element1> .. <elementn>
	arg      8  <index>
	

## opcode 10aaajjj

This group uses the three middle bits "aaa" as a
signed 3 bit integer

    aaa      0    argument 0
    aaa      1    argument 1
    aaa      2    argument 2
    aaa      3    argument 3	
    aaa      -4   argument 4
    aaa      -3   argument 5	
    aaa      -2   argument 6
    aaa      -1   argument 7	

The jjj (jump) opcodes are the same as in 01eejjjj but for
only the first 8 opcodes.

# Examples

To push constant 0 we use literal and the immediate tiny constant coding

     2  0   6=literal
    10|000|110

To push constant 1000 we use literal with 2 bytes encoding

    01|01|0110 00000011 11101000

To 2dup the stack over over can be used

	( a b -- a b a b )
	
	11|010|010
