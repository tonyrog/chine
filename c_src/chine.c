// Virtual machine for state machine processing

#include <stdlib.h>
#include <stdint.h>

#if !defined(ARDUINO)
#include <memory.h>
#endif

#include "chine.h"

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
    mp->cFP = mp->cSP;
    memset(mp->tbits, 0, sizeof(mp->tbits));
    memset(mp->tmask, 0, sizeof(mp->tmask));
    memset(mp->imask, 0, sizeof(mp->imask));
    (*sys)(mp, SYS_INIT, NULL, NULL, NULL);
}

// Set execution pointer
void chine_set_ip(chine_t* mp, int offset)
{
    mp->cIP = mp->prog + offset;
}

// Chine is on toplevel
int chine_is_top_level(chine_t* mp)
{
    return (mp->cRP == mp->stack);
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

typedef struct {
    char* name;
    int before;
    int after;
} instr_info_t;

static const instr_info_t jop_name[] = { // opcode1 and opcode2
    { "jmpz", 1, 0 },
    { "jmpnz", 1, 0 },
    { "next", 0, 0},
    { "jmplz", 1, 0 },
    { "jmp", 0, 0 },
    { "call", 0, 0 },
    { "literal", 0, 1},
    { "array", 0, 1 },
    { "arg", 0, 1 },  // opcode1
};

static const instr_info_t op_name[] = {
    // opcode0, opcode3
    { "dup", 1, 2},
    { "rot", 3, 3},
    { "over", 2, 3},
    { "drop", 1, 0 },
    { "swap", 2, 2 },
    { "-", 2, 1 },
    { "+", 2, 1 },
    { "*", 2, 1 },
    // opcode0
    { "nop", 0, 0 },
    { "and", 2, 1 },
    { "or", 2, 1 },
    { "xor", 2, 1 },
    { "0=", 1, 1 },
    { "0<", 1, 1, },
    { "not", 1, 1 },
    { "???", 0, 0 },
    { "negate", 1, 1 },
    { "/", 2, 1 },
    { "shift", 2, 1},
    { "!", 2, 0 },
    { "@", 1, 1 },
    { ">r", 1, 0 },
    { "r>", 0, 1 },
    { "r@", 0, 1 },
    { "exit", 0, 0 },
    { "sys", 0, 0 },
    {"yield", 0, 0 },
    { "[]", 2, 1 },
    { "execute", 1, 0 },
    { "fp@", 0, 1 },
    { "fp!", 1, 0 },
    { "sp@", 0, 1 },
    { "sp!", 1, 0 },
};

void static trace_begin(int op,cell_t* sp)
{
    int i;
    int size = 0;

    switch(op >> OPSHFT) {
    case 0:
	printf("%s ", op_name[op & OP0MASK].name);
	size = op_name[op & OP0MASK].before;
	break;
    case 1:
	printf("%s ", jop_name[op & OP1MASK].name);
	size = jop_name[op & OP1MASK].before;
	break;
    case 2:
	printf("%s ", jop_name[op & OP2MASK].name);
	size = jop_name[op & OP2MASK].before;
	break;
    case 3:
	printf("%s ", op_name[op & OP3MASK].name);
	size = op_name[op & OP3MASK].before;
	break;
    }
    printf("%d (", size);
    for (i = size-1; i >= 0; i--)
	printf(" %d", sp[i]);
    printf(" -- ");
    if (((op >> OPSHFT) == 0) && ((op & OP0MASK) == YIELD))
	printf(")\n");
}

void static trace_end(int op, cell_t* sp, cell_t* sp0)
{
    int i;
    int size = 0;
    int size0 = 0;

    switch(op >> OPSHFT) {
    case 0:
	printf("%s ", op_name[op & OP0MASK].name);
	size = op_name[op & OP0MASK].after;
	size0 = op_name[op & OP0MASK].before;
	break;
    case 1:
	printf("%s ", jop_name[op & OP1MASK].name);
	size = jop_name[op & OP1MASK].after;
	size0 = jop_name[op & OP1MASK].before;	
	break;
    case 2:
	printf("%s ", jop_name[op & OP2MASK].name);
	size = jop_name[op & OP2MASK].after;
	size0 = jop_name[op & OP2MASK].before;
	break;
    case 3:
	printf("%s ", op_name[op & OP3MASK].name);
	size = op_name[op & OP3MASK].after;
	size0 = op_name[op & OP3MASK].before;
	break;
    }

    for (i = size-1; i >= 0; i--) 
	printf("%d ", sp[i]);
    printf(")\n");
    if (((op >> OPSHFT) != 0) || ((op & OP0MASK) != SYS)) {
	if ((sp0 - sp) != (size - size0)) {
	    printf("operation moved stack pointer\n");
	    exit(1);
	}
    }
}

// print instruction in binary format
static const char* trace_ins(uint8_t ins)
{
    static char insbuf[32];
    
    switch (ins >> 6) {
    case 0: sprintf(insbuf,"|0|-|%d|",(ins & OP0MASK)); break;
    case 1: sprintf(insbuf,"|1|%d|%d|",OP1VAL(ins),(ins&OP1MASK)); break;
    case 2: sprintf(insbuf,"|2|%d|%d|",OP2VAL(ins),(ins&OP2MASK)); break;
    case 3: sprintf(insbuf,"|3|%d|%d|",OP3MASK&(ins>>3),OP3MASK&ins); break;
    default: return "?";
    }
    return insbuf;
}

#define TRACEF(...) printf(__VA_ARGS__)

#define BEGIN { cell_t* _s_SP=cSP; trace_begin(I,cSP); {
#define XEND  } trace_end(I,cSP,_s_SP); }
#define TEND  trace_end(I,cSP,_s_SP);
#define END   } trace_end(I,cSP,_s_SP); NEXT; }
#define END0  } trace_end(I,cSP,_s_SP); NEXT0; }

#else

#define TRACEF(...)

#define BEGIN
#define XEND
#define TEND
#define END  NEXT
#define END0 NEXT0

#endif

#define SWAP_IN(mp)				\
    cIP = (mp)->cIP;				\
    cSP = (mp)->cSP;				\
    cRP = (mp)->cRP;				\
    cFP = (mp)->cFP

#define SWAP_OUT(mp)				\
    (mp)->cIP = cIP;				\
    (mp)->cSP = cSP;				\
    (mp)->cRP = cRP;				\
    (mp)->cFP = cFP


#define CASE(mnem) case mnem
#define JCASE(mnem) case mnem+(OP0MASK+1)
#define NEXT        goto next
#define NEXT0       if ((I >> OPSHFT)==OP3) { J=(I>>OP3SHFT)&OP3MASK; I=0; goto next1; } goto next


int chine_run(chine_t* mp)
{
    uint8_t* cIP;  // instruction pointer
    cell_t*  cSP;  // stack pointer
    cell_t*  cRP;  // return stack
    cell_t*  cFP;  // frame pointer
    uint8_t   I;   // instruction
    cell_t    A;   // argument
    int J   = 0;   // opcode

#define fail(e) do { mp->cErr=(e); goto L_fail; } while(0)

    SWAP_IN(mp);

next:
    TRACEF("%04u: %s ", (int)(cIP - mp->prog), trace_ins(*cIP));
    if (cRP < mp->stack) fail(FAIL_STACK_UNDERFLOW);
    if (cSP > mp->stack+MAX_STACK) fail(FAIL_STACK_UNDERFLOW);
    if (cRP >= cSP) fail(FAIL_STACK_OVERFLOW);
    I = *cIP++;             // load instruction I
    switch(I>>OPSHFT) {     // extract opcode J
    case 0: J = (I & OP0MASK);    break;
    case 1: J = (I & OP1MASK)+(OP0MASK+1); break;
    case 2: J = (I & OP2MASK)+(OP0MASK+1); break;
    case 3: J = (I & OP3MASK);    break;
    }
next1:
    switch(J) {
    default: fail(FAIL_INVALID_OPCODE);
    JCASE(JMPZ): {
	    BEGIN;
	    cIP = cIP + get_arg(I,cIP,&A);
	    if (*cSP++ == 0) cIP += A;
	    END;
	}
	
    JCASE(JMPNZ): {
	    BEGIN;
	    cIP = cIP + get_arg(I,cIP,&A);
	    if (*cSP++ != 0) cIP += A;
	    END;
	}

    JCASE(JNEXT): {
	    BEGIN;
	    cIP = cIP + get_arg(I,cIP,&A);
	    if (--(cRP[-1])>0) cIP += A; else cRP--;
	    END;
	}

    JCASE(JMPLZ): {
	    BEGIN;
	    cIP = cIP + get_arg(I,cIP,&A);
	    if (*cSP++ < 0) cIP += A;
	    END;
	}

    JCASE(JMP): {
	    BEGIN;
	    cIP = cIP + get_arg(I,cIP,&A);
	    cIP += A;
	    END;
	}

    JCASE(CALL): {
	    BEGIN;
	    cIP = cIP + get_arg(I,cIP,&A);
	    *cRP++ = (cIP - mp->prog);
	    cIP += A;
	    END;
	}

    JCASE(LITERAL): {
	    BEGIN;
	    cIP = cIP + get_arg(I,cIP,&A);
	    *--cSP = A;
	    END;
	}

    JCASE(ARRAY): {
	    // push array pointer on stack and skip
	    BEGIN;
	    *--cSP = ((cIP-1) - mp->prog);
	    cIP = cIP + get_array_len(I,cIP,&A);
	    cIP += (get_element_len(I)*A);
	    END;
	}

    JCASE(ARG): {
	    BEGIN;
	    cIP = cIP + get_arg(I,cIP,&A);
	    cSP--;
	    cSP[0] = cFP[A];
	    END;
	}

    CASE(DUP): {
	    BEGIN;
	    cSP--;
	    cSP[0] = cSP[1];
	    END0;
	}

    CASE(ROT): {
	    BEGIN;
	    cell_t r = cSP[2];
	    cSP[2] = cSP[1];
	    cSP[1] = cSP[0];
	    cSP[0] = r;
	    END0;
	}

    CASE(SWAP): {
	    BEGIN;
	    cell_t r = cSP[1]; 
	    cSP[1] = cSP[0]; 
	    cSP[0] = r; 
	    END0;	
	}

    CASE(OVER): {
	    BEGIN;
	    cSP--;
	    cSP[0] = cSP[2];
	    END0;
	}
	
    CASE(SUB): {
	    BEGIN;
	    cSP[1] -= cSP[0]; 
	    cSP++;
	    END0;
	}
	
    CASE(DROP): {
	    BEGIN;
	    cSP++;
	    END0;
	}

    CASE(ADD): {
	    BEGIN;
	    cSP[1] += cSP[0];
	    cSP++;
	    END0;
	}

    CASE(MUL): {
	    BEGIN;
	    cSP[1] *= cSP[0];
	    cSP++;
	    END0;
	}
	
    CASE(NEGATE): {
	    BEGIN;
	    cSP[0] = -cSP[0];
	    END;
	}
	
    CASE(AND): {
	    BEGIN;
	    cSP[1] &= cSP[0];
	    cSP++;
	    END;
	}
	
    CASE(OR): {
	    BEGIN;
	    cSP[1] |= cSP[0];
	    cSP++;
	    END;
	}
	
    CASE(ZEQ): {
	    BEGIN;
	    cSP[0] = CHINE_TEST(cSP[0] == 0);
	    END;
	}
	
    CASE(ZLT): {
	    BEGIN;
	    cSP[0] = CHINE_TEST(cSP[0] < 0);
	    END;
	}
	
    CASE(NOT): {
	    BEGIN;
	    cSP[0] = ~cSP[0];
	    END;
	}	

    CASE(NOP): {
	    BEGIN;
	    END;
	}
	
    CASE(XOR): {
	    BEGIN;
	    cSP[1] ^= cSP[0];
	    cSP++;
	    END;
	}
	
    CASE(DIV): {
	    BEGIN;
	    if (cSP[0] == 0) { cSP += 2; fail(FAIL_DIV_ZERO); }
	    cSP[1] /= cSP[0];
	    cSP++;
	    END;
	}

    CASE(SHFT): { // shift left (or right)
	    BEGIN;
	    if (cSP[0] >= 0)
		cSP[1] = ((ucell_t)cSP[1]) << cSP[0];
	    else
		cSP[1] = ((ucell_t)cSP[1]) >> -cSP[0];
	    cSP++;
	    END;
	}

    CASE(STORE): {
	    BEGIN;
	    cell_t i = cSP[0];
	    if ((i < 0) || (i >= MAX_MEM)) fail(FAIL_INVALID_MEMORY_ADDRESS);
	    mp->mem[i] = cSP[1];
	    cSP += 2;
	    END;
	}

    CASE(FETCH): {
	    BEGIN;
	    cell_t i = cSP[0];
	    if ((i < 0) || (i >= MAX_MEM)) fail(FAIL_INVALID_MEMORY_ADDRESS);
	    cSP[0] = mp->mem[i]; 
	    END;
	}

    CASE(TOR): {
	    BEGIN;
	    *cRP++ = *cSP++;
	    END;
	}
	
    CASE(FROMR): {
	    BEGIN;
	    *--cSP = *--cRP;
	    END;
	}

    CASE(RFETCH): {
	    BEGIN;
	    *--cSP = cRP[-1];
	    END;
	}

    CASE(EXIT): {
	    BEGIN;
	    if (cRP == mp->stack) {
		SWAP_OUT(mp);
		TEND;
		return 0;
	    }
	    cIP = mp->prog + *--cRP;
	    END;
	}

    CASE(YIELD): {
	    BEGIN;
	    SWAP_OUT(mp);
	    XEND;
	    return 0;
	}
	
    CASE(SYS): {
	    BEGIN;
	    cell_t sysop = UINT8(cIP);
	    cell_t ret;
	    cell_t npop;
	    cell_t value;
	    
	    cIP++;
	    if ((ret = (*mp->sys)(mp, sysop, cSP, &npop, &value)) < 0) {
		TEND;
		fail(ret);
	    }
	    cSP += npop; // pop arguments
	    if (ret > 0) {
		*--cSP = value;
	    }
	    END;
	}

    CASE(ELEM): {
	    BEGIN;
	    uint8_t* aptr;
	    int i, j, n;
	    // check that top of element is a array pointer, 
	    // and that index on second element is an index into
	    // that  array, push the element onto stack
	    i    = cSP[0];             // get index
	    aptr = mp->prog + cSP[1];  // get array address
	    if ((*aptr & OP2MASK) != ARRAY) fail(FAIL_INVALID_ARGUMENT);
	    j = get_array_len(*aptr, aptr+1, &A);
	    if ((i < 0) || (i > A)) fail(FAIL_INVALID_ARGUMENT);
	    n = get_element_len(*aptr);
	    aptr += (j+1);
	    switch(n) {
	    case 1: cSP[1] = INT8(aptr + i*n); break;
	    case 2: cSP[1] = INT16(aptr + i*n); break;
	    case 4: cSP[1] = INT32(aptr + i*n); break;
	    default: fail(FAIL_INVALID_ARGUMENT);
	    }
	    cSP++;
	    END;
	}
	
    CASE(EXEC): {
	    BEGIN;
	    // place a call to the location given by addr on top of stack
	    // the address is a location relative to program start
	    *cRP++ = (cIP - mp->prog);  // save return address
	    cIP = mp->prog + *cSP++;
	    END;
	}

    CASE(SPFETCH): {
	    BEGIN;
	    cSP--;
	    cSP[0] = (cSP+1 - mp->stack);
	    END;
	}

    CASE(SPSTORE): {
	    BEGIN;
	    cSP = (mp->stack + cSP[0]);
	    END;
	}		

    CASE(FPFETCH): {  // fp@ ( -- fp )
	    BEGIN;
	    *--cSP = (cFP - mp->stack);
	    END;
	}

    CASE(FPSTORE): {  // fp! ( fp -- )
	    BEGIN;
	    cFP = *cSP++ + mp->stack;
	    END;
	}

    }
    
L_fail:
    SWAP_OUT(mp);
    return -1;
}
