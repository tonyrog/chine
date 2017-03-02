//
// chine
//

#ifndef __CHINE_H__
#define __CHINE_H__

#include <stdint.h>

#ifdef __cplusplus
extern "C" {
#endif

#define MAX_INPUT  32
#define MAX_STACK  16
#define MAX_RSTACK 4
#define MAX_MEM    16
#define MAX_TIMERS 8
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

#define OPCODE1(a)    ((a) & 0x7f)
#define OPCODE2(a,b)  (0x80|(((b)&15)<<3)|((a)&7))


typedef enum {
    // op3 : first part of OPCODE2 also an OPCODE1
    ZBRAN_H=0,  // either 4 or 8 bits offset
    LIT_H,      // const: ( -- x ) either 4 or 8 bits
    DUP,        // dup ( a -- a a )
    ROT,        // rot ( a b c  -- b c a )  ( down )
    OVER,       // over ( a b -- b a )
    DROP,       // drop ( a -- )
    SWAP,       // swap ( a b -- b a )
    SUB,        // - ( a b -- [ a-b ] )
    // op4 : second part of OPCODE2 also an OPCODE1
    ADD,        // +  ( x1 x2 -- (x1+x2) )
    MUL,        // *: ( x1 x2 -- (x1*x2) )
    NEG,        // ( a -- (-a) )
    AND,        // and: ( a b -- (a&b) )
    OR,         // or: ( a b -- (a|b) )
    ZEQ,        // 0=:  ( a -- (a==0) )
    ZLT,        // 0<:  ( a -- (a<0) )
    NOT,        // not: ( a -- not a)
    // op7 : reset of opcodes 
    NOP,        // nop: ( -- )
    ULT,        // u<: ( a b -- (a < b) )
    ULTE,       // u<= ( a b -- [a<=b] ) '-' '1-' '0<'
    XOR,        // ( a b -- (a^b) )
    DIV,        // ( a b -- (a/b) )
    INV,        // ( a -- (~a) )
    BSL,        // lshift: ( a u -- (a << u) )
    BSR,        // rshift: ( a u -- (a >> u) )
    STORE,      // ( a i -- )
    FETCH,      // ( i -- a )
    RET,        // ( -- ) R: ( addr -- )
    LIT_W,      // ( -- w )
    LIT_L,      // ( -- l )
    BRAN_B,     // ( -- )
    BRAN_W,     // ( -- )
    ZBRAN_W,    // ( cond -- )
    IBRAN_B,    // ( i -- )
    IBRAN_W,    // ( i -- )
    CALL_B,     // ( -- ) R: ( -- addr )
    CALL_W,     // ( -- ) R: ( -- addr )
    SYS_B,      // sys ( x1 .. xn -- y1 )
    EXIT,       // ( -- )
    YIELD,      // ( -- )
} opcode_t;

#define S_PUSH(x)     OPCODE2(LIT_H,(x))

// when PUSH_H/ZBRAN_H is a op7 then it's the byte versions
#define LIT_B         LIT_H
#define ZBRAN_B       ZBRAN_H

#define S_INC S_PUSH(1), ADD
#define S_DEC S_PUSH(1), SUB
#define S_LT  SUB, ZLT
#define S_ABS DUP, ZLT, ZBRAN_B,1,NEG
#define S_MIN OVER,OVER,S_LT,ZBRAN_B,3,DROP,BRAN_B,2,SWAP,DROP
#define S_MAX OVER,OVER,S_LT,ZBRAN_B,4,SWAP,DROP,BRAN_B,1,DROP
#define S_EQ  SUB, ZEQ
#define S_MOD OVER, OVER, DIV, MUL, SUB
#define S_ASR DUP,LIT_B,32,SWAP,SUB,S_PUSH(-1),SWAP,BSL,ROT,ROT,BSR,OR
// Failure codes
#define FAIL_STACK_OVERFLOW    -1
#define FAIL_STACK_UNDERFLOW   -2
#define FAIL_RSTACK_OVERFLOW   -3
#define FAIL_RSTACK_UNDERFLOW  -4
#define FAIL_DIV_ZERO          -5
#define FAIL_TIMER_OVERFLOW    -6
#define FAIL_MEMORY_OVERFLOW   -7
#define FAIL_BAD_ARG           -8

// SYSTEM CALLS
#define SYS_INIT          0  // ( -- )  called at init
#define SYS_PARAM_FETCH   1  // ( i s -- n )
#define SYS_PARAM_STORE   2  // ( v i s -- )
#define SYS_TIMER_INIT    3  // ( i -- )
#define SYS_TIMER_START   4  // ( i --  )
#define SYS_TIMER_STOP    5  // ( i --  )
#define SYS_TIMER_TIMEOUT 6  // ( i -- f )
#define SYS_TIMER_RUNNING 7  // ( i -- f )
#define SYS_INPUT_FETCH   8  // ( i k -- n )
#define SYS_SELECT_TIMER  9  // ( i -- )
#define SYS_SELECT_INPUT 10  // ( i -- )
#define SYS_DESELECT_ALL 11  // ( -- )
#define SYS_EMIT         12  // ( c -- )     tx character on default uart
#define SYS_KEY          13  // (   -- c )   rx character from uart or -1
#define SYS_QKEY         14  // (   -- f )   1 if char is read 0 otherwise

// LED interface set_led / clr_led
// CAN interface send message

// INPUT kind (k)
#define INPUT_BOOLEAN 0
#define INPUT_ANALOG  1
#define INPUT_ENCODER 2

// Program normally should look something like
// {ibranch, [Main,Init,Final]}
// {branch, Fail}
// {label,Init}
//   <init code>
//   ret
// {label,Final
//   <cleanup code>
//   ret
// {label,Main}
//   <main code>
//
// If Init/Final are missing then:
// zbranch, Main,
// ret
// {label,Main}
//   <main code>
//

typedef struct _chine_t
{
    uint8_t* cIP;
    cell_t*  cSP;
    cell_t*  cRP;
    cell_t   cErr;     // last system error
    cell_t  (*sys)(struct _chine_t* mp,
		   cell_t sysop, cell_t* revarg,
		   cell_t* npop, cell_t* reason);
    uint8_t* prog;                // program area
    cell_t   stack[MAX_STACK];    // stack
    cell_t   rstack[MAX_RSTACK];  // call stack (relative addresses etc)
    cell_t   mem[MAX_MEM];        // local store
    uint8_t  imask[NUM_IBYTES];   // input mask
    uint8_t  tbits[NUM_TBYTES];   // timer running bits
    uint8_t  tmask[NUM_TBYTES];   // selected timers
    uint32_t timer[MAX_TIMERS];   // timers
} chine_t;

extern void chine_init(chine_t* mp, uint8_t* prog, 
		       int32_t  (*sys)(chine_t* mp,
				       cell_t sysop, cell_t* revarg,
				       cell_t* npop, cell_t* reason));
extern int chine_run(chine_t* mp);
extern timeout_t chine_millis(void);
extern timeout_t chine_micros(void);
extern int chine_next(chine_t** mpv, size_t n,
		      timeout_t* tmop, uint8_t* imask);

#ifdef __cplusplus
}
#endif

#endif
