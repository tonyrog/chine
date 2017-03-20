// Virtual machine for state machine processing

#include <stdlib.h>
#include <stdint.h>

#if !defined(ARDUINO)
#include <memory.h>
#endif

#if defined(ARDUINO)
  #include "Arduino.h"
  #define DISPATCH_WITH_SWITCH
  #define CHINE_INLINE
#else
  #define CHINE_INLINE inline
#endif

#if !defined(DISPATCH_WITH_SWITCH) && !defined(DISPATCH_WITH_JUMPTABLE)
#define DISPATCH_WITH_SWITCH
#endif

#include "../include/chine.h"

// Initialize machine
void chine_init(chine_t* mp, uint8_t* prog,
		int (*sys)(chine_t* mp,
			   cell_t sysop, cell_t* revarg,
			   cell_t* npop, cell_t* reason))
{
    mp->prog = prog;
    mp->sys  = sys;
    mp->cIP = NULL;
    mp->cSP = mp->stack+MAX_STACK;  // towards low address
    mp->cRP = mp->stack;            // towards high address
    memset(mp->tbits, 0, sizeof(mp->tbits));
    memset(mp->tmask, 0, sizeof(mp->tmask));
    memset(mp->imask, 0, sizeof(mp->imask));
    (*sys)(mp, SYS_INIT, NULL, NULL, NULL);
}

// Set execution pointer
void chine_set_ip(chine_t* mp, uint8_t* ip)
{
    mp->cIP = ip;
}

// calculcuate next timeout,
// return 1 and the the update timeout value in tmop (if != NULL)
// return 0 otherwise
int chine_timeout(chine_t* mp, timeout_t* tmop)
{
    int i = 0;
    int r = 0;
    timeout_t tmo;

    if (tmop)
	tmo = *tmop;  // refine
    else
	tmo = 0xffffffff;
    
    while(i < NUM_TBYTES) {
	uint8_t tm;
	// only check timers that are selected and running
	if ((tm = (mp->tmask[i] & mp->tbits[i])) != 0) {
	    int t = 0;
	    while(tm && (t < 8)) {
		if (tm & (1 << t)) {
		    int32_t remain = mp->timer[i*8+t] - chine_millis();
		    if (remain < 0) remain = 0;
		    if ((timeout_t)remain < tmo)
			tmo = remain;
		    r = 1;
		}
		tm &= ~(1 << t);
		t++;
	    }
	}
	i++;
    }
    if (tmop) *tmop = tmo;
    return r;
}

// check if any input is selected and update imask (if != NULL)
// return 1 any input is selected
// return 0 otherwise
int chine_input_mask(chine_t* mp, uint8_t* imask)
{
    int r = 0;
    int i = 0;

    while(!r && (i < NUM_IBYTES)) {
	if (mp->imask[i]) r = 1;
	if (imask) imask[i] |= mp->imask[i];
	i++;
    }
    return r;
}

// calculcuate next timeout or input(s)
// return 1 if a chine is waiting on a timer or is waiting for input,
//          an updated timeout time is stored in tmop if not NULL.
// return 0 otherwise 
int chine_next(chine_t* mp, timeout_t* tmop, uint8_t* imask)
{ 
    int r;
    // NOTE! must calculate both! || is not correct.
    r = chine_input_mask(mp, imask);
    r |= chine_timeout(mp, tmop);
    return r;
}


// check vector of machines for input poll and timeout
// return 1 if any of the machines in mpv is waiting for input/timer
int chine_nextv(chine_t** mpv, size_t n, timeout_t* tmop, uint8_t* imask)
{
    int i, r = 0;
    for (i = 0; i < n; i++)
	r |= chine_next(mpv[i], tmop, imask);
    return r;
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
    if (op == YIELD)
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

static CHINE_INLINE int get_array_len(uint8_t I, uint8_t* ptr, cell_t* argp)
{
    if ((I >> 6) == 2) {
	*argp=((I>>3)&7);
	return 0;
    }
    else {
	switch((I>>3) & 7) {
	case 0: *argp=UINT8(ptr); return 1;
	case 1: *argp=UINT8(ptr); return 1;
	case 2: *argp=UINT8(ptr); return 1;
	case 4: *argp=UINT16(ptr); return 2;
	case 5: *argp=UINT16(ptr); return 2;
	case 6: *argp=UINT16(ptr); return 2;
	default: *argp=0; return -1;
	}
    }
}

static CHINE_INLINE int get_element_len(uint8_t I)
{
    if ((I >> 6) == 2)
	return 1;
    else {
	switch((I>>3) & 7) {
	case 0:  return 1;
	case 1:  return 2;
	case 2:  return 4;
	case 4:  return 1;
	case 5:  return 2;
	case 6:  return 4;
	default: return 0;
	}
    }
}

static CHINE_INLINE int get_arg(uint8_t I, uint8_t* ptr, cell_t* argp)
{
    if ((I >> 6) == 2) {
	*argp = (((int8_t)(I<<2))>>5); 
	return 0;
    }
    else {
	switch((I>>3) & 7) {
	case 0: *argp=INT8(ptr); return 1;
	case 1: *argp=INT16(ptr); return 2;
	case 2: *argp=INT32(ptr); return 4;
	default: *argp=0; return -1;
	}
    }
}

#if defined(DISPATCH_WITH_JUMPTABLE)

#define SWITCH_DATA() \
    void*     N;							\
    static void* opA[] = {						\
    [JMPZ]   = &&op_JMPZ,   [JMPNZ]  = &&op_JMPNZ,			\
    [JNEXT] = &&op_JNEXT, [JMPLZ] = &&op_JMPLZ,				\
    [JMP]    = &&op_JMP, [CALL]   = &&op_CALL,				\
    [LITERAL] = &&op_LITERAL, [ARRAY] = &&op_ARRAY };			\
    static void* op[] = { [DUP]   = &&op_DUP, [ROT]   = &&op_ROT,	\
			  [OVER]  = &&op_OVER, [DROP]  = &&op_DROP,	\
			  [SWAP]  = &&op_SWAP, [SUB]   = &&op_SUB,	\
			  [ADD]   = &&op_ADD,  [MUL]   = &&op_MUL,	\
			  [NOP]   = &&op_NOP,  [AND]   = &&op_AND,	\
			  [OR]    = &&op_OR,   [XOR]   = &&op_XOR,	\
			  [ZEQ]   = &&op_ZEQ,  [ZLT]   = &&op_ZLT,	\
			  [NOT]   = &&op_NOT,				\
			  [INV]   = &&op_INV,  [NEG]   = &&op_NEG,	\
			  [DIV]   = &&op_DIV,  [SHFT]  = &&op_SHFT,	\
                          [STORE] = &&op_STORE,[FETCH] = &&op_FETCH,	\
                          [TOR]   = &&op_TOR,  [FROMR] = &&op_FROMR,	\
                          [RFETCH] = &&op_RFETCH,			\
			  [SYS]   = &&op_SYS, [EXIT]  = &&op_EXIT,	\
			  [YIELD] = &&op_YIELD,				\
                          [ELEM] = &&op_ELEM, [EXEC] = &&op_EXEC,	\
			  [(EXEC+1) ... 31] = &&op_FAIL }

#define SWITCH() do {				\
	N = &&next;				\
    next:					\
	if (cRP < mp->stack) fail(FAIL_STACK_UNDERFLOW);		\
	if (cSP > mp->stack+MAX_STACK) fail(FAIL_STACK_UNDERFLOW);	\
	if (cRP >= cSP) fail(FAIL_STACK_OVERFLOW);			\
	TRACEF("%04u: ", (int)(cIP - mp->prog));	\
	I = *cIP++;					\
	switch(I >> 6) {				\
	case 0: goto *op[I&31];				\
        case 1: goto *opA[I&7];				\
	case 2: goto *opA[I&7];				\
	case 3:						\
	    N = &&next1;				\
	    goto *op[I&7];				\
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

#define SWITCH()							\
    next:								\
    TRACEF("%04u: ", (int)(cIP - mp->prog));				\
    if (cRP < mp->stack) fail(FAIL_STACK_UNDERFLOW);			\
    if (cSP > mp->stack+MAX_STACK) fail(FAIL_STACK_UNDERFLOW);		\
    if (cRP >= cSP) fail(FAIL_STACK_OVERFLOW);				\
    I = *cIP++;								\
    switch(I>>6) {							\
    case 0: J=I&31; break;						\
    case 1: J=(I&7)+32; break;						\
    case 2: J=(I&7)+32; break;						\
    case 3: J=I&7; break;						\
    }									\
next1:									\
switch(J) {								\
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
	int j;
	if ((j = get_arg(I,cIP,&A)) < 0) fail(FAIL_INVALID_OPCODE);
	cIP += j;
	if (*cSP++ == 0) cIP += A;
	END(JMPZ,"jmpz",1,0);
    }

JCASE(JMPNZ): {
	BEGIN(JMPNZ,"jmpnz",1,0);
	int j;
	if ((j = get_arg(I,cIP,&A)) < 0) fail(FAIL_INVALID_OPCODE);
	cIP += j;
	if (*cSP++ != 0) cIP += A;
	END(JMPNZ,"jmpnz",1,0);
    }

JCASE(JNEXT): {
	BEGIN(JNEXT,"next",0,0);
	int j;
	if ((j = get_arg(I,cIP,&A)) < 0) fail(FAIL_INVALID_OPCODE);
	cIP += j;
	if (--(cRP[-1])>0) cIP += A; else cRP--;
	END(JNEXT,"next",0,0);
    }

JCASE(JMPLZ): {
	BEGIN(JMPLZ,"jmplz",1,0);
	int j;
	if ((j = get_arg(I,cIP,&A)) < 0) fail(FAIL_INVALID_OPCODE);
	cIP += j;
	if (*cSP++ < 0) cIP += A;
	END(JMPLZ,"jmplz",1,0);
    }

JCASE(JMP): {
	BEGIN(JMP,"jmp",0,0);
	int j;
	if ((j = get_arg(I,cIP,&A)) < 0) fail(FAIL_INVALID_OPCODE);
	cIP += j;
	cIP += A;
	END(JMP,"jmp",0,0);
    }

JCASE(CALL): {
	BEGIN(CALL,"call",0,0);
	int j;
	if ((j = get_arg(I,cIP,&A)) < 0) fail(FAIL_INVALID_OPCODE);
	cIP += j;
	*cRP++ = (cIP - mp->prog);
	cIP += A;
	END(CALL,"call",0,0);
    }

JCASE(LITERAL): {
	BEGIN(LITERAL,"literal",0,1);
	int j;
	if ((j = get_arg(I,cIP,&A)) < 0) fail(FAIL_INVALID_OPCODE);
	cIP += j;
	*--cSP = A;
	END(LITERAL,"literal",0,1);
    }

JCASE(ARRAY): {
	// push array pointer on stack and skip
	BEGIN(ARRAY,"array",0,1);
	int j;
	*--cSP = ((cIP-1) - mp->prog);
	if ((j = get_array_len(I,cIP,&A)) < 0) fail(FAIL_INVALID_OPCODE);
	cIP += j;
	cIP += (get_element_len(I)*A);
	END(ARRAY,"array",0,1);
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

CASE(NOT): {
	BEGIN(NOT,"not",1,1);
	cSP[0] = !cSP[0];
	END(NOT,"not",1,1);
    }

CASE(NOP): {
	BEGIN(NOP,"nop",0,0);
	END(NOP,"nop",0,0);
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

CASE(SHFT): { // shift left (or right)
	BEGIN(SHFT,"shift",2,1);
	if (cSP[0] >= 0)
	    cSP[1] = ((ucell_t)cSP[1]) << cSP[0];
	else
	    cSP[1] = ((ucell_t)cSP[1]) >> -cSP[0];
	cSP++;
	END(SHFT,"shift",2,1);
    }

CASE(STORE): {
	BEGIN(STORE,"!",2,0);
	cell_t i = cSP[0];
	if ((i < 0) || (i >= MAX_MEM)) fail(FAIL_INVALID_MEMORY_ADDRESS);
	mp->mem[i] = cSP[1];
	cSP += 2;
	END(STORE,"!",2,0);
    }

CASE(FETCH): {
	BEGIN(FETCH,"@",1,1);
	cell_t i = cSP[0];
	if ((i < 0) || (i >= MAX_MEM)) fail(FAIL_INVALID_MEMORY_ADDRESS);
	cSP[0] = mp->mem[i]; 
	END(FETCH,"@",1,1);
    }

CASE(TOR): {
	BEGIN(TOR,">r",1,0);
	*cRP++ = *cSP++;
	END(TOR,">r",1,0);
    }

CASE(FROMR): {
	BEGIN(FROMR,"r>",0,1);
	*--cSP = *--cRP;
	END(FROMR,"r>",0,1);
    }

CASE(RFETCH): {
	BEGIN(RFETCH,"r@",0,1);
	*--cSP = cRP[-1];
	END(RFETCH,"r@",0,1);
    }

CASE(EXIT): {
	BEGIN(EXIT,"exit",0,0);
	cIP = mp->prog + *--cRP;
	END(EXIT,"exit",0,0);
    }

CASE(YIELD): {
	BEGIN(YIELD,"yield",0,0);
	SWAP_OUT(mp);
	XEND(YIELD,"yield",0,0);
	return 0;
    }

CASE(SYS): {
	BEGIN(SYS,"sys",0,0);
	cell_t sysop = UINT8(cIP);
	cell_t ret;
	cell_t npop;
	cell_t value;

	cIP++;
	if ((ret = (*mp->sys)(mp, sysop, cSP, &npop, &value)) < 0) {
	    TEND(SYS,"sys",0,0);
	    fail(ret);
	}
	cSP += npop; // pop arguments
	if (ret > 0) {
	    *--cSP = value;
	}
	END(SYS,"sys",0,0);
    }

CASE(ELEM): {
	BEGIN(ELEM,"[]",2,1);
	uint8_t* aptr;
	int i, j, n;
	// check that top of element is a array pointer, 
	// and that index on second element is an index into
	// that  array, push the element onto stack
	i    = cSP[0];             // get index
	aptr = mp->prog + cSP[1];  // get array address

	if (((*aptr & 7) != ARRAY) ||
	    ((j = get_array_len(*aptr, aptr+1, &A)) < 0))
	    fail(FAIL_INVALID_ARGUMENT);
	if ((i < 0) || (i > A))
	    fail(FAIL_INVALID_ARGUMENT);
	n = get_element_len(*aptr);
	aptr += (j+1);

	switch(n) {
	case 1: cSP[1] = INT8(aptr + i*n); break;
	case 2: cSP[1] = INT16(aptr + i*n); break;
	case 4: cSP[1] = INT32(aptr + i*n); break;
	default: fail(FAIL_INVALID_ARGUMENT);
	}
	cSP++;
	END(ELEM,"[]",2,1);
    }

CASE(EXEC): {
	BEGIN(EXEC,"execute",1,0);
	// place a call to the location given by addr on top of stack
	// the address is a location relative to program start
	*cRP++ = (cIP - mp->prog);  // save return address
	cIP = mp->prog + *cSP++;
	END(EXEC,"execute",1,0);
    }

ENDCASE();

L_fail:
    SWAP_OUT(mp);
    return -1;
}
