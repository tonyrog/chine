// Virtual machine for state machine processing

#include <stdlib.h>
#include <stdint.h>

#if !defined(ARDUINO)
#include <memory.h>
#endif

#if defined(ARDUINO)
  #include "Arduino.h"
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
void static trace_op_begin(int op,char* tok, cell_t* cSP, int size)
{
    int i;
    printf("%s (", tok);
    for (i = size-1; i >= 0; i--)
	printf(" %d", cSP[i]);
    printf(" -- ");
    if (op == EXIT)
	printf(")\n");
}

void static trace_op_end(int op, cell_t* cSP, cell_t* cSP0, int size, int size0)
{
    int i;
    for (i = size-1; i >= 0; i--) 
	printf("%d ", cSP[i]);
    printf(")\n");
    if (op != SYS_B) {
	if ((cSP0 - cSP) != (size - size0)) {
	    printf("operation moved stack pointer\n");
	    exit(1);
	}
    }
}
#define OP1_BEGIN(op,tok,b,a) case op: { cell_t* _s_SP=cSP; int _s_a=(a); int _s_b=(b); int _s_op=op; trace_op_begin(op,tok,cSP,b);
#define OP1_DEFAULT() default: { cell_t* _s_SP=cSP; int _s_a=0; int _s_b=0; trace_op_begin(NOP,"unknown",cSP,1);
#define OP1_END trace_op_end(_s_op,cSP,_s_SP,_s_a,_s_b); I = (I>>3) & 15; goto dispatch2; }
#define OPx_END trace_op_end(_s_op,cSP,_s_SP,_s_a,_s_b); NEXT; }

#define OP2_BEGIN(op,tok,b,a) case op: { cell_t* _s_SP=cSP; int _s_a=(a); int _s_b=(b); int _s_op=op; trace_op_begin(op,tok,cSP,b);
#define OP2_DEFAULT() default: { cell_t* _s_SP=cSP; int _s_a=0; int _s_b=0; int _s_op=NOP; trace_op_begin(NOP,"unknown",cSP,1);
#define OP2_END trace_op_end(_s_op,cSP,_s_SP,_s_a,_s_b); NEXT; }

#define DISPATCH_SLOW() usleep(1000)

#else

#define OP1_BEGIN(op,tok,b,a) case op:
#define OP1_DEFAULT() default:
#define OP1_END I = (I>>3) & 15; goto dispatch2
#define OPx_END NEXT

#define OP2_BEGIN(op,tok,b,a) case op:
#define OP2_DEFAULT() default:
#define OP2_END NEXT

#define DISPATCH_SLOW()

#endif

#define DISPATCH1_BEGIN()			\
    I = *cIP++;					\
    if (I < 0x80) goto dispatch2;		\
    switch(I & 7)
#define DISPATCH1_END()

#define DISPATCH2_BEGIN() dispatch2: switch(I)
#define DISPATCH2_END()


#define SWAP_IN(mp)				\
    cIP = (mp)->cIP;				\
    cSP = (mp)->cSP;				\
    cRP = (mp)->cRP

#define SWAP_OUT(mp)				\
    (mp)->cIP = cIP;				\
    (mp)->cSP = cSP;				\
    (mp)->cRP = cRP

int chine_run(chine_t* mp)
{
    uint8_t* cIP; 
    cell_t*  cSP;
    cell_t*  cRP;
    uint8_t   I;

    SWAP_IN(mp);

#define NEXT  goto next
#define FAIL(e) do { mp->cErr=(e); goto fail; } while(0)

next:
    DISPATCH_SLOW();
    DISPATCH1_BEGIN() {
	OP1_BEGIN(ZBRAN_H,"[zbranch.h]",1,0) {
	    int8_t j = ((int8_t)(I<<1))>>4;
	    cell_t r = *cSP++;
	    if (r) cIP += 0; else cIP += (j);
	} OPx_END;
	OP1_BEGIN(LIT_H,"[literal.h]",0,1) {
	    int8_t j = ((int8_t)(I<<1))>>4;
	    *--cSP = j;
	} OPx_END;
	OP1_BEGIN(ROT,"[rot]",3,3) {
	    cell_t r = cSP[2];
	    cSP[2] = cSP[1];
	    cSP[1] = cSP[0];
	    cSP[0] = r;
	} OP1_END;
	OP1_BEGIN(SWAP,"[swap]",2,2) {
	    cell_t r = cSP[1]; 
	    cSP[1] = cSP[0]; 
	    cSP[0] = r; 
	} OP1_END;
	OP1_BEGIN(OVER,"[over]",2,3) {
	    cSP--;
	    cSP[0] = cSP[2];
	} OP1_END;
	OP1_BEGIN(DUP,"[dup]",1,2) {
	    cSP--;
	    cSP[0] = cSP[1];
	} OP1_END;
	OP1_BEGIN(SUB,"[-]",2,1) {
	    cSP[1] -= cSP[0]; 
	    cSP++;
	} OP1_END;
	OP1_BEGIN(DROP,"[drop]",1,0) {
	    cSP++;
	} OP1_END;
    } DISPATCH1_END();

    DISPATCH2_BEGIN() {
	OP2_BEGIN(ZBRAN_B,"zbranch.b",1,0) {
	    int8_t j = INT8(cIP);
	    cell_t r = *cSP++;
	    if (r) cIP += 1; else cIP += (j+1);
	} OP2_END;
	OP2_BEGIN(LIT_H,"literal.b",0,1) {
	    *--cSP = INT8(cIP); cIP += 1;
	} OP2_END;
	OP2_BEGIN(DUP,"dup",1,2) {
	    cSP--;
	    cSP[0] = cSP[1];
	} OP2_END;
	OP2_BEGIN(OVER,"over",2,3) {
	    cSP--;
	    cSP[0] = cSP[2];
	} OP2_END;
	OP2_BEGIN(ROT,"rot",3,3) {
	    cell_t r = cSP[2];
	    cSP[2] = cSP[1];
	    cSP[1] = cSP[0];
	    cSP[0] = r;
	} OP2_END;
	OP2_BEGIN(SWAP,"swap",2,2) {
	    cell_t r = cSP[1];
	    cSP[1] = cSP[0];
	    cSP[0] = r;
	} OP2_END;
	OP2_BEGIN(DROP,"drop",1,0) {
	    cSP++;
	} OP2_END;
	OP2_BEGIN(SUB,"-",2,1) {
	    cSP[1] -= cSP[0];
	    cSP++;
	} OP2_END;
	OP2_BEGIN(ADD,"+",2,1) {
	    cSP[1] += cSP[0];
	    cSP++;
	} OP2_END;
	OP2_BEGIN(MUL,"*",2,1)  {
	    cSP[1] *= cSP[0];
	    cSP++;
	} OP2_END;
	OP2_BEGIN(NEG,"negate",1,1) {
	    cSP[0] = -cSP[0];
	} OP2_END;
	OP2_BEGIN(AND,"and",2,1) {
	    cSP[1] &= cSP[0];
	    cSP++;
	} OP2_END;
	OP2_BEGIN(OR,"or",2,1) {
	    cSP[1] |= cSP[0];
	    cSP++;
	} OP2_END;
	OP2_BEGIN(ZEQ,"0=",1,1) {
	    cSP[0] = (cSP[0] == 0);
	} OP2_END;
	OP2_BEGIN(ZLT,"0<",1,1) {
	    cSP[0] = (cSP[0] < 0);
	} OP2_END;
	OP2_BEGIN(NOT,"not",1,1) {
	    cSP[0] = !cSP[0];
	} OP2_END;
	OP2_BEGIN(NOP,"nop",0,0) {
	} OP2_END;
	OP2_BEGIN(ULT,"u<",2,1) { 
	    cSP[1] = ((ucell_t)cSP[1] < (ucell_t)cSP[0]);
	    cSP++;
	} OP2_END;
	OP2_BEGIN(ULTE,"u<=",2,1) {
	    cSP[1] = ((ucell_t)cSP[1] <= (ucell_t)cSP[0]);
	    cSP++;
	} OP2_END;
	OP2_BEGIN(XOR,"xor",2,1)	{
	    cSP[1] ^= cSP[0];
	    cSP++;
	} OP2_END;
	OP2_BEGIN(DIV,"/",2,1) {
	    if (cSP[0] == 0) { cSP += 2; FAIL(FAIL_DIV_ZERO); }
	    cSP[1] /= cSP[0];
	    cSP++;
	} OP2_END;
	OP2_BEGIN(INV,"invert",1,1) {
	    cSP[0] = ~cSP[0];
	} OP2_END;
	OP2_BEGIN(BSL,"lshift",2,1) {
	    cSP[1] = ((ucell_t)cSP[1]) << ((ucell_t)cSP[0]);
	    cSP++;
	} OP2_END;
	OP2_BEGIN(BSR,"rshift",2,1) {
	    cSP[1] = ((ucell_t)cSP[1]) >> ((ucell_t)cSP[0]);
	    cSP++;
	} OP2_END;
	OP2_BEGIN(STORE,"!",2,0) {
	    cell_t i = cSP[0];
	    if ((i < 0) || (i >= MAX_MEM)) FAIL(FAIL_MEMORY_OVERFLOW);
	    mp->mem[i] = cSP[1];
	    cSP += 2;
	} OP2_END;
	OP2_BEGIN(FETCH,"@",1,1) {
	    cell_t i = cSP[0];
	    if ((i < 0) || (i >= MAX_MEM)) FAIL(FAIL_MEMORY_OVERFLOW);
	    cSP[0] = mp->mem[i]; 
	} OP2_END;
	OP2_BEGIN(LIT_W,"literal.w",0,1) {
	    *--cSP = INT16(cIP); cIP += 2; 
	} OP2_END;

	OP2_BEGIN(LIT_L,"literal.l",0,1) {
	    *--cSP = INT32(cIP); cIP += 4;
	} OP2_END;
	OP2_BEGIN(BRAN_B,"branch.b",0,0) {
	    int8_t  j = INT8(cIP); cIP += (j+1); 
	} OP2_END;
	OP2_BEGIN(BRAN_W,"branch.w",0,0) {
	    int16_t j = INT16(cIP); cIP += (j+2); 
	} OP2_END;
	OP2_BEGIN(ZBRAN_W,"zbranch.w",1,0) {
	    int16_t j = INT16(cIP);
	    cell_t r = *cSP++;
	    if (r) cIP += 2; else cIP += (j+2);
	} OP2_END;
	OP2_BEGIN(IBRAN_B,"ibranch.b",1,0) {
	    uint8_t n = UINT8(cIP);
	    cell_t i = *cSP++;
	    if ((i < 0) || (i >= n))
		cIP += (1+n);  /* skip when out of range */
	    else {
		int8_t j = INT8(cIP+1+i);
		cIP += (1+n+j);
	    }
	} OP2_END;
	OP2_BEGIN(IBRAN_W,"ibranch.w",1,0) {
	    uint16_t n = UINT16(cIP);
	    cell_t i = *cSP++;
	    if ((i < 0) || (i >= n))
		cIP += (2+2*n);  /* skip when out of range */
	    else {
		int16_t j = INT16(cIP+2+2*i);
		cIP += (2+2*n+j);
	    }
	} OP2_END;
	OP2_BEGIN(CALL_B,"call.b",0,0) {
	    int8_t j = INT8(cIP);
	    *--cRP = ((cIP+1) - mp->prog);
	    cIP += (j+1);
	} OP2_END;
	OP2_BEGIN(CALL_W,"call.w",0,0) {
	    int8_t j = INT16(cIP);
	    *--cRP = ((cIP+2) - mp->prog);
	    cIP += (j+2);
	} OP2_END;
	OP2_BEGIN(RET,"ret",0,0) {
	    cIP = mp->prog + *cRP++;
	} OP2_END;
	OP2_BEGIN(SYS_B,"sys.b",0,0) {
	    cell_t sysop = UINT8(cIP);
	    cell_t ret;
	    cell_t npop;
	    cell_t value;

	    cIP++;
	    if ((ret = (*mp->sys)(mp, sysop, cSP, &npop, &value)) < 0) {
		mp->cErr = ret;
		goto fail;
	    }
	    cSP += npop; // pop arguments
	    if (ret > 0) {
		*--cSP = value;
	    }
	} OP2_END;
	OP2_BEGIN(EXIT,"exit",0,0) {
	    SWAP_OUT(mp);
	    return 0;
	} OP2_END;
	OP2_BEGIN(YIELD,"yield",0,0) {
	    SWAP_OUT(mp);
	    return 1;
	} OP2_END;
	OP2_DEFAULT() {
	    FAIL(FAIL_BAD_ARG);
	} OP2_END;
	
    } DISPATCH2_END();

fail:
    SWAP_OUT(mp);
    return -1;
#undef NEXT
#undef FAIL
}
