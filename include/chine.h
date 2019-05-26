//
// chine
//

#ifndef __CHINE_H__
#define __CHINE_H__

#include <stdint.h>

#ifdef __cplusplus
extern "C" {
#endif

#ifdef __GNUC__
#  define UNUSED(x) UNUSED_ ## x __attribute__((__unused__))
#else
#  define UNUSED(x) UNUSED_ ## x
#endif

#ifdef __GNUC__
#  define UNUSED_FUNCTION(x) __attribute__((__unused__)) UNUSED_ ## x
#else
#  define UNUSED_FUNCTION(x) UNUSED_ ## x
#endif

#if defined(ARDUINO)
  #include "Arduino.h"
  #define CHINE_INLINE
#else
  #define CHINE_INLINE inline
#endif
    
#define FILE_VERSION_MAJOR 1
#define FILE_VERSION_MINOR 1
#define FILE_VERSION_PATCH 0

#define MAX_INPUT  32
#define MAX_STACK  16  // stack + return stack
#define MAX_MEM    16
#define MAX_TIMERS 16
#define NUM_TBYTES  ((MAX_TIMERS+7)>>3)
#define NUM_IBYTES  ((MAX_INPUT+7)>>3)

typedef int32_t  cell_t;
typedef uint32_t ucell_t;
typedef uint32_t timeout_t;

#define SETBIT(v,i) (v)[(i)>>3] |= (1 << ((i) & 7))
#define CLRBIT(v,i) (v)[(i)>>3] &= ~(1 << ((i) & 7))
#define TSTBIT(v,i) ((v)[(i)>>3] & (1 << ((i) & 7)))

#define U32(x) ((uint32_t)(x))
#define U16(x) ((uint16_t)(x))
#define U8(x)  ((uint8_t)(x))

#define CHINE_TRUE       -1
#define CHINE_FALSE      0
#define CHINE_TEST(x)    (-(!!(x)))

// chine test table
//    x  = 123   -1   0
//  !!x  =   1    1   0
// -!!x  =  -1   -1   0    

static inline uint8_t unpack_u8(uint8_t* ptr)
{
    return U8(ptr[0]);
}

static inline uint16_t unpack_u16(uint8_t* ptr)
{
    return (U16(ptr[0])<<8) | U16(ptr[1]);
}

static inline uint32_t unpack_u32(uint8_t* ptr)
{
    return (U32(ptr[0])<<24) | (U32(ptr[1])<<16) |
	(U32(ptr[2])<<8) | U32(ptr[3]);
}

static inline int8_t unpack_i8(uint8_t* ptr)
{
    return (int8_t) unpack_u8(ptr);
}

static inline int16_t unpack_i16(uint8_t* ptr)
{
    return (int16_t) unpack_u16(ptr);
}

static inline int32_t unpack_i32(uint8_t* ptr)
{
    return (int32_t) unpack_u32(ptr);
}

#define UINT8(ptr)  unpack_u8((ptr))
#define UINT16(ptr) unpack_u16((ptr))
#define UINT32(ptr) unpack_u32((ptr))
#define INT8(ptr)   unpack_i8((ptr))
#define INT16(ptr)  unpack_i16((ptr))
#define INT32(ptr)  unpack_i32((ptr))

// OPCODE  yyxxxxxx
#define OP0 0
#define OP1 1
#define OP2 2
#define OP3 3
#define OPMASK 0xC0
#define OPSHFT 6
    
// OPCODE0 00iiiiii 64 instruction opcode
#define OP0MASK 0x3f
#define OPCODE0(op)    (0x00 | ((op)&OP0MASK))

// OPCODE1 01lljjjj 16 "jump" instructions with 2 bit length indicator
// 0=8 bit, 1=16 bit, 2=32 bit (3 = yet unassigned)
#define OP1MASK  0x0f
#define VAL1MASK 0x03
#define VAL1SHFT 4
#define OP1VAL(x) (((x)>>VAL1SHFT)&VAL1MASK)
#define OPCODE1(jop,l) (0x40|((jop)&OP1MASK)|(((l)&VAL1MASK)<<VAL1SHFT))

// OPCODE2 10aaajjj 8 "jump" instructions with 3 bit signed integer argument
#define OP2MASK  0x07
#define VAL2MASK 0x07
#define VAL2SHFT 3
#define OP2VAL(x) (((int8_t)((x)<<(8-OPSHFT)))>>(8-VAL2SHFT))
#define OPCODE2(jop,a) (0x80|((jop)&OP2MASK)|(((a)&VAL2MASK)<<VAL2SHFT))

// OPCODE3 11iiikkk 2x 8 bit packed instructions
#define OP3MASK 0x07
#define OP3SHFT 3
#define OPCODE3(op1,op2) (0xc0|((op1)&OP3MASK)|(((op2)&OP3MASK)<<OP3SHFT))

typedef enum {
    JMPZ    = 0,    // if (TOP == 0) goto L
    JMPNZ   = 1,    // if (TOP != 0) goto L
    JNEXT   = 2,    // if (--RP[0]>=0) goto L; else RP--;
    JMPLZ   = 3,    // if (TOP < 0) goto L
    JMP     = 4,    // goto L
    CALL    = 5,    // call(L)
    LITERAL = 6,    // constant N
    ARRAY   = 7,    // Array+String (of constants)
    // OPCODE1 only
    ARG     = 8,
    JOP_9   = 9,
    JOP_10  = 10,
    JOP_11  = 11,
    JOP_12  = 12,
    JOP_13  = 13,
    JOP_14  = 14,
    JOP_15  = 15,
} opcode1_t;

typedef enum {
    // op3
    DUP     = 0,        // dup: ( a -- a a )
    ROT     = 1,        // rot: ( a b c  -- b c a )  ( down )
    OVER    = 2,        // over: ( a b -- b a )
    DROP    = 3,        // drop: ( a -- )
    SWAP    = 4,        // swap: ( a b -- b a )
    SUB     = 5,        // -: ( a b -- [ a-b ] )
    ADD     = 6,        // +:  ( x1 x2 -- (x1+x2) )
    MUL     = 7,        // *: ( x1 x2 -- (x1*x2) )
    // op6
    NOP     = 8,        // nop: ( -- )
    AND     = 9,        // and: ( a b -- (a&b) )
    OR      = 10,       // or: ( a b -- (a|b) )
    XOR     = 11,       // ^: ( a b -- (a^b) )
    ZEQ     = 12,       // 0=:  ( a -- (a==0) )
    ZLT     = 13,       // 0<:  ( a -- (a<0) )
    NOT     = 14,       // not: ( a -- (~a) )
    OP_15   = 15,       // unassigned
    NEGATE  = 16,       // negate: ( a -- (-a) )
    DIV     = 17,       // / ( a b -- (a/b) )
    SHFT    = 18,       // shift ( a n -- ( a << n, n>=0 ) | ( a >> -n, n<0) )
    STORE   = 19,       // ! ( a i -- )
    FETCH   = 20,       // @ ( i -- a )
    TOR     = 21,       // >r ( n -- ) R: ( -- n )
    FROMR   = 22,       // r> R: ( n -- ) ( -- n )
    RFETCH  = 23,       // r@: R: ( n -- n ) ( -- n )
    EXIT    = 24,       // exit/; ( -- ) R: ( addr -- )
    SYS     = 25,       // sys ( x1 .. xn -- y1 )
    YIELD   = 26,       // ( -- )
    ELEM    = 27,       // [] ( a* i -- n )
    EXEC    = 28,       // execute ( a* i -- )
    FPFETCH = 29,       // fp@ ( -- fp )
    FPSTORE = 30,       // fp! ( fp -- )
    SPFETCH = 31,       // sp! ( -- sp )
    SPSTORE = 32,       // sp@ ( sp -- )
    OP_33   = 33,
    //...
    OP_63   = 63
} opcode_t;

#define ARG8(x)   U8((x))
#define ARG16(x)  U8((x)>>8), U8((x))
#define ARG32(x)  U8((x)>>24), U8((x)>>16), U8((x)>>8), U8((x))

#define JOPi(jop,y)  OPCODE2((jop),(y)) // jump -4 .. 3
#define JOP8(jop,y)  OPCODE1((jop),0),  ARG8((y))
#define JOP16(jop,y) OPCODE1((jop),1),  ARG16((y))

#define PUSHi(x)     OPCODE2(LITERAL,(x)) // push -4 .. 3
#define PUSH8(x)     OPCODE1(LITERAL,0), ARG8((x))
#define PUSH16(x)    OPCODE1(LITERAL,1), ARG16((x))
#define PUSH32(x)    OPCODE1(LITERAL,2), ARG32((x))

#define ARRAY8i(n)   OPCODE2(ARRAY,(n))
#define ARRAY8_8(n)  OPCODE1(ARRAY,0),  ARG8((n))
#define ARRAY8_16(n) OPCODE1(ARRAY,1),  ARG8((n))
#define ARRAY8_32(n) OPCODE1(ARRAY,2),  ARG8((n))
#define ARRAY16_8(n)  OPCODE1(ARRAY,4), ARG16((n))
#define ARRAY16_16(n) OPCODE1(ARRAY,5), ARG16((n))
#define ARRAY16_32(n) OPCODE1(ARRAY,6), ARG16((n))

#define OPOP(x,y)    OPCODE3((x),(y))

// : 1+
#define S_INC PUSHi(1), ADD
// : 1-
#define S_DEC PUSHi(1), SUB
// : <
#define S_LT  SUB, ZLT
// : abs
#define S_ABS DUP, ZLT, JOP8(JMPZ,1), NEGATE
// : max 
#define S_MIN OPOP(OVER,OVER),SUB,ZLT,JOP8(JMPZ,3),DROP,	\
	JOP8(JMP,1),OPOP(SWAP,DROP)
// : max 
#define S_MAX OPOP(OVER,OVER),SUB,ZLT,JOP8(JMPZ,3),	\
	OPOP(SWAP,DROP),JOP8(JMP,1),DROP
// : =
#define S_EQ  SUB, ZEQ
// : mod
#define S_MOD OPOP(OVER,OVER), DIV, OPOP(MUL,SUB)
// : arshift
#define S_ASR DUP,PUSH8(32),OPOP(SWAP,SUB),	\
	PUSHi(-1),SWAP,SHFT,ROT,ROT,NEGATE,SHFT,OR
// : 0<= 1- 0< ;
#define S_ZLE PUSHi(1),SUB,ZLT
// : 2dup
#define S_2DUP OPOP(OVER,OVER)
// : u< ( u u -- t ) 2dup xor 0< if swap drop 0< else - 0< then ;
#define S_ULT S_2DUP, XOR, ZLT, JOP8(JMPZ,4),	\
	OPOP(SWAP,DROP), ZLT, JOP8(JMP,2), SUB, ZLT
// : u<=  2dup xor 0< if swap drop 0< else - 0<= then ;
#define S_ULE S_2DUP, XOR, ZLT, JOP8(JMPZ,4),	\
	OPOP(SWAP,DROP), ZLT, JOP8(JMP,6), SUB, S_ZLE

#define FENTER FPFETCH, TOR, SPFETCH, FPSTORE
#define FLEAVE FPFETCH, FROMR, FPSTORE, SPSTORE

// Failure codes
#define FAIL_INVALID_ARGUMENT       -1
#define FAIL_INVALID_OPCODE         -2
#define FAIL_STACK_OVERFLOW         -3
#define FAIL_STACK_UNDERFLOW        -4
#define FAIL_INVALID_MEMORY_ADDRESS -9
#define FAIL_DIV_ZERO               -10
#define FAIL_TIMER_OVERFLOW         -11
#define FAIL_TERMINATE              -128

// SYSTEM CALLS
typedef enum {
    SYS_INIT = 0,      // ( -- )       called at init
    SYS_TERMINATE,     // ( -- )       terminate the program
    SYS_NOW,           // ( -- u )     milliseconds since start
    SYS_EMIT,          // ( c -- )     transmit on default output
    SYS_RECV,          // ( -- c )     receive from default input
    SYS_AVAIL,         // ( -- f )     check if default input is available
    SYS_PARAM_FETCH,   // ( s i -- n )
    SYS_PARAM_STORE,   // ( s i v -- )
    SYS_TIMER_INIT,    // ( i -- )
    SYS_TIMER_START,    // ( i --  )
    SYS_TIMER_STOP,     // ( i --  )
    SYS_TIMER_TIMEOUT,  // ( i -- f )
    SYS_TIMER_RUNNING,  // ( i -- f )
    SYS_INPUT_FETCH,    // ( i k -- n )
    SYS_OUTPUT_STORE,   // ( i k n -- )
    SYS_SELECT_TIMER,   // ( i -- )
    SYS_DESELECT_TIMER, // ( i -- )
    SYS_SELECT_INPUT,   // ( i -- )
    SYS_DESELECT_INPUT, // ( i -- )
    SYS_DESELECT_ALL,   // ( -- )
    SYS_UART_CONNECT,   // ( mode baud str -- 1 fd | 0 err )
    SYS_UART_SEND,      // ( fd tty -- 1 | 0 )  tx character on uart
    SYS_UART_RECV,      // ( fd -- c )  rx character from uart or -1
    SYS_UART_AVAIL,     // ( fd -- f )  1 if char is read 0 otherwise
    SYS_UART_DISCONNECT,// ( fd -- f )  disconnet uart
    SYS_GPIO_INPUT,     // ( i -- )
    SYS_GPIO_OUTPUT,    // ( i -- )
    SYS_GPIO_SET,       // ( i -- )
    SYS_GPIO_CLR,       // ( i -- )
    SYS_GPIO_GET,       // ( i -- n )
    SYS_ANALOG_SEND,    // ( i u16 -- )
    SYS_ANALOG_RECV,    // ( i -- u16 )
    SYS_CAN_CONNECT,    // ( mode bitrate dev -- 1 fd | 0 err )
    SYS_CAN_SEND,       // ( n fd buf -- 1 n | 0 err )
    SYS_CAN_RECV,       // ( fd buf -- 1 n | 0 err )
    SYS_CAN_AVAIL,      // ( fd -- f )
    SYS_CAN_DISCONNECT, // ( fd -- 1 | 0 err )
    SYS_FILE_OPEN,      // ( mode str -- 1 fd | 0 err )
    SYS_FILE_WRITE,     // ( n fd buf -- 1 n | 0 err )
    SYS_FILE_READ,      // ( n fd buf -- 1 n | 0 err )
    SYS_FILE_CLOSE,     // ( fd -- 1 | 0 err )
} syscall_t;

// LED interface set_led / clr_led
// CAN interface send message

// INPUT kind (k)
#define INPUT_BOOLEAN 0
#define INPUT_ANALOG  1
#define INPUT_ENCODER 2

typedef struct _chine_t
{
    uint8_t* cIP;
    cell_t*  cSP;
    cell_t*  cRP;
    cell_t*  cFP;      // frame pointer
    cell_t   cErr;     // last system error
    int (*sys)(struct _chine_t* mp,
	       cell_t sysop, cell_t* revarg,
	       cell_t* npop, cell_t* reason);
    uint8_t* prog;                // program area
    cell_t   stack[MAX_STACK];    // stack
    cell_t   mem[MAX_MEM];        // local store
    uint8_t  imask[NUM_IBYTES];   // input mask
    uint8_t  tbits[NUM_TBYTES];   // timer running bits
    uint8_t  tmask[NUM_TBYTES];   // selected timers
    timeout_t timer[MAX_TIMERS];  // timers
} chine_t;

// get argument
// opcode1: I = 01llxxxx
// opcode2: I = 10aaaxxx aaa is signed 3-bit argument
// 
static CHINE_INLINE int get_arg(uint8_t I, uint8_t* ptr, cell_t* argp)
{
    if ((I >> OPSHFT) == OP2) { // extract 3 bit signed number
	*argp = OP2VAL(I);
	return 0;
    }
    else {  // opcode1
	switch(OP1VAL(I)) {
	case 0:  *argp=INT8(ptr);   return 1;
	case 1:  *argp=INT16(ptr);  return 2;
	case 2:  *argp=INT32(ptr);  return 4;
	default: *argp=0; return 0;
	}
    }
}

// Get number of bytes of of array length
// opcode2: I = 10|lll|111  => 0
// opcode1: I = 01|0-|0111  => 1
// opcode1: I = 01|1-|0111  => 2
static CHINE_INLINE int get_array_hlen(uint8_t I)
{
    uint8_t H = (I >> 5) & 3;
    return H ? (H-1) : 0;
}

static CHINE_INLINE int get_array_len(uint8_t I, uint8_t* ptr, cell_t* argp)
{
    switch(get_array_hlen(I)) {
    case 0: *argp = (I>>VAL2SHFT) & VAL2MASK; return 0;
    case 1: *argp = UINT8(ptr);  return 1;
    case 2: *argp = UINT16(ptr); return 2;
    default: return 0;
    }
}

// Get number of bytes per array element
// 10|lll|111  => 1
// 01|00|0111  => 1
// 01|01|0111  => 2
// 01|10|0111  => 4
// 01|11|0111  => 8 but is undefined right now!

static CHINE_INLINE int get_element_len(uint8_t I)
{
    if ((I >> OPSHFT) == OP2)
	return 1;
    else
	return (1 << ((I>>VAL1SHFT) & VAL1MASK));
}

extern void chine_init(chine_t* mp, uint8_t* prog, 
		       int  (*sys)(chine_t* mp,
				   cell_t sysop, cell_t* revarg,
				   cell_t* npop, cell_t* reason));
extern int chine_final(chine_t* mp);
extern int chine_run(chine_t* mp);

extern void chine_set_ip(chine_t* mp, int offset);
extern int  chine_is_top_level(chine_t* mp);

extern timeout_t chine_millis(void);
extern timeout_t chine_micros(void);
extern int chine_next(chine_t* mp, timeout_t* tmop, uint8_t* imask);
extern int chine_nextv(chine_t** mpv, size_t n,
		       timeout_t* tmop, uint8_t* imask);

#ifdef __cplusplus
}
#endif

#endif
