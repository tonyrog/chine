// Virtual machine for state machine processing

#include <stdlib.h>
#include <stdint.h>

#if !defined(ARDUINO)
#include <memory.h>
#endif

#if defined(ARDUINO)
  #include "Arduino.h"
  #define DISPATCH_WITH_SWITCH
#endif

#if !defined(DISPATCH_WITH_SWITCH) && !defined(DISPATCH_WITH_JUMPTABLE)
#define DISPATCH_WITH_SWITCH
#endif

#include "../include/chine.h"

void chine_init(chine_t* mp, uint8_t* prog,
		cell_t  (*sys)(chine_t* mp,
			       cell_t sysop, cell_t* revarg,
			       cell_t* npop, cell_t* reason))
{
    mp->prog = prog;
    mp->sys  = sys;
    mp->cIP = mp->prog;
    mp->cSP = mp->stack+MAX_STACK;
    mp->cRP = mp->rstack+MAX_RSTACK;
    memset(mp->tbits, 0, sizeof(mp->tbits));
    memset(mp->tmask, 0, sizeof(mp->tmask));
    memset(mp->imask, 0, sizeof(mp->imask));
    (*sys)(mp, SYS_INIT, NULL, NULL, NULL);
}

// check vector of machines for input poll and timeout
int chine_next(chine_t** mpv, size_t n, timeout_t* tmop, uint8_t* imask)
{
    timeout_t tmo = 0xffffffff;
    int i;

    for (i = 0; i < n; i++) {
	int j;

	if (imask) {
	    for (j = 0; j < NUM_IBYTES; j++)
		imask[j] |= mpv[i]->imask[j];
	}

	for (j = 0; j < NUM_TBYTES; j++) {
	    uint8_t tm;
	    if ((tm = mpv[i]->tmask[j]) != 0) {
		int t = 0;
		while(tm && (t < 8)) {
		    if (tm & (1 << t)) {
			int32_t remain = mpv[i]->timer[j*8+t] - chine_millis();
			if (remain < 0)   remain = 0;
			if ((timeout_t)remain < tmo) tmo = remain;
		    }
		    tm &= ~(1 << t);
		    t++;
		}
	    }
	}
    }
    if (tmop) *tmop = tmo;
    return 0;
}

#ifdef TRACE
#include <stdio.h>
#include <unistd.h>

void static trace_begin(int op,char* tok,cell_t* sp,int size)
{
    int i;
    printf("%s (", tok);
    for (i = size-1; i >= 0; i--)
	printf(" %d", sp[i]);
    printf(" -- ");
    if (op == EXIT)
	printf(")\n");
}

void static trace_end(int op, cell_t* sp, cell_t* sp0, int size, int size0)
{
    int i;
    for (i = size-1; i >= 0; i--) 
	printf("%d ", sp[i]);
    printf(")\n");
    if (op != SYS) {
	if ((sp0 - sp) != (size - size0)) {
	    printf("operation moved stack pointer\n");
	    exit(1);
	}
    }
}

#define TRACEF(...) printf(__VA_ARGS__)

#define BEGIN(op,tok,b,a) { cell_t* _s_SP=cSP; trace_begin((op),(tok),cSP,(b)); {
#define XEND(op,tok,b,a) } trace_end((op),cSP,_s_SP,(a),(b)); }
#define TEND(op,tok,b,a) trace_end((op),cSP,_s_SP,(a),(b));
#define END(op,tok,b,a) } trace_end((op),cSP,_s_SP,(a),(b)); NEXT; }
#define END0(op,tok,b,a) } trace_end((op),cSP,_s_SP,(a),(b)); NEXT0; }

#else

#define TRACEF(...)

#define BEGIN(op,tok,b,a) 
#define XEND(op,tok,b,a)
#define TEND(op,tok,b,a)
#define END(op,tok,b,a)  NEXT
#define END0(op,tok,b,a) NEXT0

#endif

#define SWAP_IN(mp)				\
    cIP = (mp)->cIP;				\
    cSP = (mp)->cSP;				\
    cRP = (mp)->cRP

#define SWAP_OUT(mp)				\
    (mp)->cIP = cIP;				\
    (mp)->cSP = cSP;				\
    (mp)->cRP = cRP

#if defined(DISPATCH_WITH_JUMPTABLE)

#define SWITCH_DATA() \
    void*     N;	   \
    static void* opA[] = { \
    [JMPZ]   = &&op_JMPZ,   [JMPNZ]  = &&op_JMPNZ,\
    [JMPGTZ] = &&op_JMPGTZ, [JMPGEZ] = &&op_JMPGEZ, \
    [JMP]    = &&op_JMP,    [JMPI]   = &&op_JMPI, \
    [CALL]   = &&op_CALL,   [LIT]    = &&op_LIT }; \
    static void* op[] = { [DUP]   = &&op_DUP, [ROT]   = &&op_ROT,  \
			  [OVER]  = &&op_OVER, [DROP]  = &&op_DROP, \
			  [SWAP]  = &&op_SWAP, [SUB]   = &&op_SUB, \
			  [ADD]   = &&op_ADD,  [MUL]   = &&op_MUL, \
			  [NOP]   = &&op_NOP,  [AND]   = &&op_AND, \
			  [OR]    = &&op_OR,   [XOR]   = &&op_XOR, \
			  [ZEQ]   = &&op_ZEQ,  [ZLT]   = &&op_ZLT, \
			  [ZLE]   = &&op_ZLE,  [ULT]   = &&op_ULT, \
			  [ULE]   = &&op_ULE,  [NOT]   = &&op_NOT, \
			  [INV]   = &&op_INV,  [NEG]   = &&op_NEG, \
			  [DIV]   = &&op_DIV,  [BSL]   = &&op_BSL, \
			  [BSR]   = &&op_BSR,  [STORE] = &&op_STORE, \
			  [FETCH] = &&op_FETCH, [RET]   = &&op_RET, \
			  [SYS]   = &&op_SYS, [EXIT]  = &&op_EXIT, \
			  [YIELD] = &&op_YIELD, \
			  [(YIELD+1) ... 31] = &&op_FAIL }

#define SWITCH() do {				\
	N = &&next;				\
    next:					\
	TRACEF("%04u: ", (int)(cIP - mp->prog));	\
	I = *cIP++;					\
	switch(I >> 6) {				\
	case 0: goto *op[I&31];				\
	case 1:						\
	    switch((I>>3)&7) {				\
	    case 0:  A = INT8(cIP); break;		\
	    case 1:  A = INT16(cIP); break;		\
	    case 3:  A = INT32(cIP); break;		\
	    default: A = 0; fail(FAIL_INVALID_OPCODE);	\
	    }						\
	    TRACEF("A=%d, k=%d ", A, (((I>>3)&7)+1));	\
	    cIP += (((I>>3)&7)+1);			\
	    goto *opA[I & 7];				\
	case 2:						\
	    A = (((int8_t)(I<<2))>>5);			\
	    goto *opA[I & 7];				\
	case 3:						\
	    N = &&next1;				\
	    goto *op[I & 7];				\
	}						\
    next1:						\
	N = &&next;					\
	goto *op[(I>>3) & 7];				\
    } while(0)

#define CASE(mnem) op_##mnem
#define JCASE(mnem) op_##mnem
#define ENDCASE() op_FAIL: { fail(FAIL_INVALID_OPCODE); }
#define NEXT        goto *N
#define NEXT0       goto *N

#elif defined(DISPATCH_WITH_SWITCH)

#define SWITCH_DATA()				\
    int J

#define SWITCH()					\
next:							\
    TRACEF("%04u: ", (int)(cIP - mp->prog));		\
    I = *cIP++;						\
    switch(I >> 6) {					\
    case 0: J=I&31; break;         			\
    case 1:						\
	switch((I>>3)&7) {				\
	case 0:  A = INT8(cIP); break;			\
	case 1:  A = INT16(cIP); break;			\
	case 3:  A = INT32(cIP); break;			\
	default: A = 0; fail(FAIL_INVALID_OPCODE);	\
	}						\
	TRACEF("A=%d, k=%d ", A, (((I>>3)&7)+1));	\
	cIP += (((I>>3)&7)+1);				\
	J = (I&7)+32; break;				\
    case 2:						\
        A = (((int8_t)(I<<2))>>5);			\
	J = (I&7)+32; break;				\
    case 3:						\
        J = I&7; break;					\
    }							\
next1:							\
    switch(J) {						\
    default: fail(FAIL_INVALID_OPCODE)


#define ENDCASE() }
#define CASE(mnem) case mnem
#define JCASE(mnem) case mnem+32
#define NEXT        goto next
#define NEXT0       if ((I >> 6)==3) { J=(I>>3)&7; I=0; goto next1; } goto next
#else 
#error "define DISPATCH_WITH_JUMPTABLE or DISPATCH_WITH_SWITCH"
#endif


int chine_run(chine_t* mp)
{
    uint8_t* cIP;  // instruction pointer
    cell_t*  cSP;  // stack pointer
    cell_t*  cRP;  // return stack
    uint8_t   I;   // instruction
    cell_t    A;   // argument
    SWITCH_DATA();

#define fail(e) do { mp->cErr=(e); goto L_fail; } while(0)

    SWAP_IN(mp);

    SWITCH();

JCASE(JMPZ): {
	BEGIN(JMPZ,"jmpz",1,0);
	if (*cSP++ == 0) cIP += A;
	END(JMPZ,"jmpz",1,0);
    }

JCASE(JMPNZ): {
	BEGIN(JMPNZ,"jmpnz",1,0);
	if (*cSP++ != 0) cIP += A;
	END(JMPNZ,"jmpnz",1,0);
    }

JCASE(JMPGTZ): {
	BEGIN(JMPGTZ,"jmpgtz",1,0);
	if (*cSP++ > 0) cIP += A;
	END(JMPGTZ,"jmpgtz",1,0);
    }

JCASE(JMPGEZ): {
	BEGIN(JMPGEZ,"jmpgez",1,0);
	if (*cSP++ >= 0) cIP += A;
	END(JMPGEZ,"jmpgez",1,0);
    }

JCASE(JMP): {
	BEGIN(JMP,"jmp",0,0);
	cIP += A;
	END(JMP,"jmp",0,0);
    }

JCASE(CALL): {
	BEGIN(CALL,"call",0,0);
	*--cRP = (cIP - mp->prog);
	cIP += A;
	END(CALL,"call",0,0);
    }

JCASE(JMPI): {
	BEGIN(JMPI,"jmpi",1,0);
	// A = n size of jump table
	uint8_t k = ((I>>3)&7)+1;  // number of argument bytes
	cell_t i = *cSP++;

	if ((i < 0) || (i >= A))
	    cIP += k*A;  /* skip when out of range */
	else {
	    int32_t j;
	    switch(k) {
	    case 1: j = INT8(cIP+1*i); break;
	    case 2: j = INT16(cIP+2*i); break;
	    case 4: j = INT32(cIP+4*i); break;
	    }
	    cIP += (k*A+j);
	}
	END(JMPI,"jmpi",1,0);
    }

JCASE(LIT): {
	BEGIN(LIT,"literal",0,1);
	*--cSP = A;
	END(LIT,"literal",0,1);
    }

CASE(DUP): {
	BEGIN(DUP,"dup",1,2);
	cSP--;
	cSP[0] = cSP[1];
	END0(DUP,"dup",1,2);
    }

CASE(ROT): {
	BEGIN(ROT,"rot",3,3);
	cell_t r = cSP[2];
	cSP[2] = cSP[1];
	cSP[1] = cSP[0];
	cSP[0] = r;
	END0(ROT,"rot",3,3);
    }

CASE(SWAP): {
	BEGIN(SWAP,"swap",2,2);
	cell_t r = cSP[1]; 
	cSP[1] = cSP[0]; 
	cSP[0] = r; 
	END0(SWAP,"swap",2,2);	
    }

CASE(OVER): {
	BEGIN(OVER,"over",2,3);
	cSP--;
	cSP[0] = cSP[2];
	END0(OVER,"over",2,3);
    }

CASE(SUB): {
	BEGIN(SUB,"-",2,1);
	cSP[1] -= cSP[0]; 
	cSP++;
	END0(OVER,"-",2,1);
    }
	
CASE(DROP): {
	BEGIN(DROP,"drop",1,0);
	cSP++;
	END0(DROP,"drop",1,0);
    }

CASE(ADD): {
	BEGIN(ADD,"+",2,1);
	cSP[1] += cSP[0];
	cSP++;
	END0(ADD,"+",2,1);
    }

CASE(MUL): {
	BEGIN(MUL,"*",2,1);
	cSP[1] *= cSP[0];
	cSP++;
	END0(MUL,"*",2,1);
    }

CASE(NEG): {
	BEGIN(NEG,"negate",1,1);
	cSP[0] = -cSP[0];
	END(NEG,"negate",1,1);
    }

CASE(AND): {
	BEGIN(AND,"and",2,1);
	cSP[1] &= cSP[0];
	cSP++;
	END(AND,"and",2,1);
    }

CASE(OR): {
	BEGIN(OR,"or",2,1);
	cSP[1] |= cSP[0];
	cSP++;
	END(OR,"or",2,1);
    }

CASE(ZEQ): {
	BEGIN(ZEQ,"0=",1,1);
	cSP[0] = (cSP[0] == 0);
	END(ZEQ,"0=",1,1);
    }

CASE(ZLT): {
	BEGIN(ZLT,"0<",1,1);
	cSP[0] = (cSP[0] < 0);
	END(ZLT,"0<",1,1);
    }

CASE(ZLE): {
	BEGIN(ZLE,"0<=",1,1);
	cSP[0] = (cSP[0] <= 0);
	END(ZLE,"0<=",1,1);
    }

CASE(NOT): {
	BEGIN(NOT,"not",1,1);
	cSP[0] = !cSP[0];
	END(NOT,"not",1,1);
    }

CASE(NOP): {
	BEGIN(NOP,"nop",0,0);
	END(NOP,"nop",0,0);
    }

CASE(ULT): {
	BEGIN(ULT,"u<",2,1);
	cSP[1] = ((ucell_t)cSP[1] < (ucell_t)cSP[0]);
	cSP++;
	END(ULT,"u<",2,1);
    }

CASE(ULE): {
	BEGIN(ULE,"u<=",2,1);
	cSP[1] = ((ucell_t)cSP[1] <= (ucell_t)cSP[0]);
	cSP++;
	END(ULE,"u<=",2,1);
    }

CASE(XOR): {
	BEGIN(XOR,"xor",2,1);
	cSP[1] ^= cSP[0];
	cSP++;
	END(XOR,"xor",2,1);
    }

CASE(DIV): {
	BEGIN(DIV,"/",2,1);
	if (cSP[0] == 0) { cSP += 2; fail(FAIL_DIV_ZERO); }
	cSP[1] /= cSP[0];
	cSP++;
	END(DIV,"/",2,1);
    }

CASE(INV): {
	BEGIN(INV,"invert",1,1);
	cSP[0] = ~cSP[0];
	END(INV,"invert",1,1);
    }

CASE(BSL): {
	BEGIN(BSL,"lshift",2,1);
	cSP[1] = ((ucell_t)cSP[1]) << ((ucell_t)cSP[0]);
	cSP++;
	END(BSL,"lshift",2,1);
    }

CASE(BSR): {
	BEGIN(BSR,"rshift",2,1);
	cSP[1] = ((ucell_t)cSP[1]) >> ((ucell_t)cSP[0]);
	cSP++;
	END(BSR,"rshift",2,1);
    }
    
CASE(STORE): {
	BEGIN(STORE,"!",2,0);
	cell_t i = cSP[0];
	if ((i < 0) || (i >= MAX_MEM)) fail(FAIL_MEMORY_OVERFLOW);
	mp->mem[i] = cSP[1];
	cSP += 2;
	END(STORE,"!",2,0);
    }

CASE(FETCH): {
	BEGIN(FETCH,"@",1,1);
	cell_t i = cSP[0];
	if ((i < 0) || (i >= MAX_MEM)) fail(FAIL_MEMORY_OVERFLOW);
	cSP[0] = mp->mem[i]; 
	END(FETCH,"@",1,1);
    }

CASE(RET): {
	BEGIN(RET,"ret",0,0);
	cIP = mp->prog + *cRP++;
	END(RET,"ret",0,0);
    }

CASE(SYS): {
	BEGIN(SYS,"sys",0,0);
	cell_t sysop = UINT8(cIP);
	cell_t ret;
	cell_t npop;
	cell_t value;

	cIP++;
	if ((ret = (*mp->sys)(mp, sysop, cSP, &npop, &value)) < 0) {
	    TEND(SYS,"sys.b",0,0);
	    fail(ret);
	}
	cSP += npop; // pop arguments
	if (ret > 0) {
	    *--cSP = value;
	}
	END(SYS,"sys.b",0,0);
    }

CASE(EXIT): {
	BEGIN(EXIT,"exit",0,0);
	SWAP_OUT(mp);
	XEND(EXIT,"exit",0,0);
	return 0;
    }

CASE(YIELD): {
	BEGIN(YIELD,"yield",0,0);
	SWAP_OUT(mp);
	XEND(YIELD,"yield",0,0);
	return 1;
    }

ENDCASE();


L_fail:
    SWAP_OUT(mp);
    return -1;
}
