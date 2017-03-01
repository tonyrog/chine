//
// chine
//

#ifndef __CHINE_H__
#define __CHINE_H__

#include <stdint.h>

#define MAX_STACK  16
#define MAX_RSTACK 4
#define MAX_MEM    16
#define MAX_TIMERS 8
#define NUM_TBITS  ((MAX_TIMERS+7)>>3)

#define SETBIT(v,i) (v)[(i)>>3] |= (1 << ((i) & 7))
#define CLRBIT(v,i) (v)[(i)>>3] &= ~(1 << ((i) & 7))
#define TSTBIT(v,i) ((v)[(i)>>3] & (1 << ((i) & 7)))

static inline uint8_t unpack_u8(uint8_t* ptr)
{
    return ptr[0];
}

static inline uint16_t unpack_u16(uint8_t* ptr)
{
    return (ptr[0]<<8) | ptr[1];
}

static inline uint32_t unpack_u32(uint8_t* ptr)
{
    return (ptr[0]<<24) | (ptr[1]<<16) | (ptr[2]<<8) | ptr[3];
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


// op3 : first part of OPCODE2 also an OPCODE1
#define ZBRAN_H     0  // either 4 or 8 bits offset
#define LIT_H       1  // const: ( -- x ) either 4 or 8 bits
#define DUP         2  // dup ( a -- a a )
#define ROT         3  // rot ( a b c  -- b c a )  ( down )
#define OVER        4  // over ( a b -- b a )
#define DROP        5  // drop ( a -- )
#define SWAP        6  // swap ( a b -- b a )
#define SUB         7   // - ( a b -- [ a-b ] )

// op4 : second part of OPCODE2 also an OPCODE1
#define ADD         8   // +  ( x1 x2 -- (x1+x2) )
#define MUL         9   // *: ( x1 x2 -- (x1*x2) )
#define EQ          10  // =: ( a b -- ( a==b) )
#define AND         11  // and: ( a b -- (a&b) )
#define OR          12  // or: ( a b -- (a|b) )
#define ZEQ         13  // 0=:  ( a -- (a==0) )
#define ZLT         14  // 0<:  ( a -- (a<0) )
#define NOT         15  // not: ( a -- not a)

// op7 : reset of opcodes 
#define DIV         16  // ( a b -- (a/b) )
#define MOD         17  // ( a b -- (a%b) )
#define XOR         18  // ( a b -- (a^b) )
#define NEG         19  // ( a -- (-a) )
#define INV         20  // ( a -- (~a) )
#define BSL         21  // lshift: ( a n -- (a << n) )
#define BSR         22  // rshift: ( a n -- (a >> n) )
#define ASR         23  // arshift: ( a n -- (a >> n) )
#define ULT         24  // u<: ( a b -- (a < b) )
#define STORE       25  // ( a i -- )
#define FETCH       26  // ( i -- a )
#define NOP         27  // nop: ( -- )
#define ULTE        28  // u<= ( a b -- [a<=b] ) '-' '1-' '0<'
#define RET         29  // ( -- ) R: ( addr -- )
#define LIT_W       30  // ( -- w )
#define LIT_L       31  // ( -- l )
#define BRAN_B      32  // ( -- )
#define BRAN_W      33  // ( -- )
#define ZBRAN_W     34  // ( cond -- )
#define IBRAN_B     35  // ( i -- )
#define IBRAN_W     36  // ( i -- )
#define CALL_B      37 // ( -- ) R: ( -- addr )
#define CALL_W      38  // ( -- ) R: ( -- addr )
#define SYS_B       39  // sys ( x1 .. xn -- y1 )

#define EXIT        40  // ( -- )  - fixme
#define YIELD       41  // ( -- )

#define S_PUSH(x)     OPCODE2(LIT_H,(x))

// when PUSH_H/ZBRAN_H is a op7 then it's the byte versions
#define LIT_B         LIT_H
#define ZBRAN_B       ZBRAN_H

#define INC S_PUSH(1), ADD
#define DEC S_PUSH(1), SUB
#define LT  SUB, ZLT
#define ABS DUP, ZLT, ZBRAN_B,1,NEG
#define MIN OVER,OVER,LT,ZBRAN_B,3,DROP,BRAN_B,2,SWAP,DROP
#define MAX OVER,OVER,LT,ZBRAN_B,4,SWAP,DROP,BRAN_B,1,DROP



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
#define SYS_SELECT        9  // ( tmask imask -- )
#define SYS_EMIT         10  // ( c -- )     tx character on default uart
#define SYS_KEY          11  // (   -- c )   rx character from uart or -1
// LED interface set_led / clr_led
// CAN interface send message

// INPUT kind (k)
#define INPUT_BOOLEAN 0
#define INPUT_ANALOG  1
#define INPUT_ENCODER 2

typedef struct _chine_t
{
    uint8_t* IP;
    int32_t* SP;
    int32_t* RP;
    uint32_t imask;   // current input mask
    uint32_t tmask;   // active timers mask
    int32_t  err;     // last system error
    int32_t  (*sys)(struct _chine_t* mp,
		    int32_t sysop, int32_t* revarg,
		    int32_t* npop, int32_t* reason);
    uint8_t*  prog;               // program area
    int32_t  stack[MAX_STACK];    // stack 
    int32_t  rstack[MAX_RSTACK];  // call stack
    int32_t  mem[MAX_MEM];        // local store
    uint8_t  tbits[NUM_TBITS];    // timer on/off bits
    uint32_t timer[MAX_TIMERS];   // timers
} chine_t;

extern void chine_init(chine_t* mp, uint8_t* prog, 
		       int32_t  (*sys)(chine_t* mp,
				       int32_t sysop, int32_t* revarg,
				       int32_t* npop, int32_t* reason));
extern int chine_run(chine_t* mp);
extern int chine_next(chine_t** mpv, size_t n, int32_t* tmop, uint32_t* imaskp);

#endif
