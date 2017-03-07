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

#define OPCODE0(op)      (0x00 | ((op) & 63))
#define OPCODE1(jop,l)   (0x40 | ((jop) & 7) | (((l) & 7) << 3)) // L+1 bytes
#define OPCODE2(jop,l)   (0x80 | ((jop) & 7) | (((l) & 7) << 3)) // L:3/signed
#define OPCODE3(op1,op2) (0xc0 | ((op1) & 7) | (((op2)&7) << 3))

// opcode with argument OPCODE1 with L+1 bytes, OPCODE2 with signed L:3
typedef enum {
    JMPZ = 0,    // if (TOP == 0) goto L
    JMPNZ,       // if (TOP != 0) goto L
    JMPGTZ,      // if (TOP > 0) goto L
    JMPGEZ,      // if (TOP >= 0) goto L
    JMP,         // goto L
    JMPI,        // goto *L
    CALL,        // call(L)
    LIT          // constant N
} opcode1_t;

typedef enum {
    // op3
    DUP=0,        // dup ( a -- a a )
    ROT,        // rot ( a b c  -- b c a )  ( down )
    OVER,       // over ( a b -- b a )
    DROP,       // drop ( a -- )
    SWAP,       // swap ( a b -- b a )
    SUB,        // - ( a b -- [ a-b ] )
    ADD,        // +  ( x1 x2 -- (x1+x2) )
    MUL,        // *: ( x1 x2 -- (x1*x2) )
    // op6
    NOP,        // nop: ( -- )
    AND,        // and: ( a b -- (a&b) )
    OR,         // or: ( a b -- (a|b) )
    XOR,        // ( a b -- (a^b) )
    ZEQ,        // 0=:  ( a -- (a==0) )
    ZLT,        // 0<:  ( a -- (a<0) )
    ZLE,        // 0<=: ( a -- (a<=0) )
    ULT,        // u<: ( a b -- (a < b) )
    ULE,        // u<= ( a b -- [a<=b] )
    NOT,        // not: ( a -- not a)
    INV,        // ( a -- (~a) )
    NEG,        // ( a -- (-a) )
    DIV,        // ( a b -- (a/b) )
    BSL,        // lshift: ( a u -- (a << u) )
    BSR,        // rshift: ( a u -- (a >> u) )
    STORE,      // ( a i -- )
    FETCH,      // ( i -- a )
    RET,        // ( -- ) R: ( addr -- )
    SYS,        // sys ( x1 .. xn -- y1 )
    EXIT,       // ( -- )
    YIELD,      // ( -- )
} opcode_t;

#define JOPi(jop,y)  OPCODE2((jop),(y)) // jump -4 .. 3
#define JOP8(jop,y)  OPCODE1((jop),0),  U8((y))
#define JOP16(jop,y) OPCODE1((jop),1),  U8((y)>>8), U8((y))

#define PUSHi(x)     OPCODE2(LIT,(x)) // push -4 .. 3
#define PUSH8(x)     OPCODE1(LIT,0), U8((x))
#define PUSH16(x)    OPCODE1(LIT,1), U8((x)>>8), U8((x))
#define PUSH32(x)    OPCODE1(LIT,3), U8((x)>>24), U8((x)>>16), U8((x)>>8), U8((x))

#define OPOP(x,y)    OPCODE3((x),(y))

#define S_INC PUSHi(1), ADD
#define S_DEC PUSHi(1), SUB
#define S_LT  SUB, ZLT
#define S_ABS DUP, ZLT, JOP8(JMPZ,1), NEG
#define S_MIN OPOP(OVER,OVER),SUB,JOP8(JMPGEZ,3),DROP,\
	JOP8(JMP,1),OPOP(SWAP,DROP)
#define S_MAX OPOP(OVER,OVER),SUB,JOP8(JMPGEZ,3),	\
	OPOP(SWAP,DROP),JOP8(JMP,1),DROP
#define S_EQ  SUB, ZEQ
#define S_MOD OPOP(OVER,OVER), DIV, OPOP(MUL,SUB)
#define S_ASR DUP,PUSH8(32),OPOP(SWAP,SUB),	\
	PUSHi(-1),SWAP,BSL,ROT,ROT,BSR,OR

// Failure codes
#define FAIL_STACK_OVERFLOW    -1
#define FAIL_STACK_UNDERFLOW   -2
#define FAIL_RSTACK_OVERFLOW   -3
#define FAIL_RSTACK_UNDERFLOW  -4
#define FAIL_DIV_ZERO          -5
#define FAIL_TIMER_OVERFLOW    -6
#define FAIL_MEMORY_OVERFLOW   -7
#define FAIL_BAD_ARG           -8
#define FAIL_INVALID_OPCODE    -9

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
#define SYS_NOW          15  // ( -- u )     milliseconds since start

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
