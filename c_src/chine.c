// Virtual machine for state machine processing

#include <stdlib.h>
#include <stdint.h>

#include "../include/chine.h"

void chine_init(chine_t* mp, uint8_t* prog,
		int32_t  (*sys)(chine_t* mp,
				int32_t sysop, int32_t* revarg,
				int32_t* npop, int32_t* reason))
{
    int i;
    mp->prog = prog;
    mp->sys  = sys;
    mp->IP = mp->prog;
    mp->SP = mp->stack+MAX_STACK;
    mp->RP = mp->rstack+MAX_RSTACK;
    for (i = 0; i < NUM_TBITS; i++) mp->tbits[i] = 0;
    mp->imask = 0;
    mp->tmask = 0;
    (*sys)(mp, SYS_INIT, NULL, NULL, NULL);
}

#ifdef TRACE
#include <stdio.h>
void static trace_op_begin(int op,char* tok, int32_t* SP, int size)
{
    int i;
    printf("%s (", tok);
    for (i = size-1; i >= 0; i--)
	printf(" %d", SP[i]);
    printf(" -- ");
    if (op == EXIT)
	printf(")\n");
}

void static trace_op_end(int op,int32_t* SP, int32_t* SP0, int size, int size0)
{
    int i;
    for (i = size-1; i >= 0; i--) 
	printf("%d ", SP[i]);
    printf(")\n");
    if (op != SYS_B) {
	if ((SP0 - SP) != (size - size0)) {
	    printf("operation moved stack pointer\n");
	    exit(1);
	}
    }
}
#define OP1_BEGIN(op,tok,b,a) case op: { int32_t* _s_SP=SP; int _s_a=(a); int _s_b=(b); int _s_op=op; trace_op_begin(op,tok,SP,b);
#define OP1_DEFAULT() default: { int32_t* _s_SP=SP; int _s_a=0; int _s_b=0; trace_op_begin(NOP,"unknown",SP,1);
#define OP1_END trace_op_end(_s_op,SP,_s_SP,_s_a,_s_b); I = (I>>3) & 15; goto dispatch2; }
#define OPx_END trace_op_end(_s_op,SP,_s_SP,_s_a,_s_b); NEXT; }

#define OP2_BEGIN(op,tok,b,a) case op: { int32_t* _s_SP=SP; int _s_a=(a); int _s_b=(b); int _s_op=op; trace_op_begin(op,tok,SP,b);
#define OP2_DEFAULT() default: { int32_t* _s_SP=SP; int _s_a=0; int _s_b=0; int _s_op=NOP; trace_op_begin(NOP,"unknown",SP,1);
#define OP2_END trace_op_end(_s_op,SP,_s_SP,_s_a,_s_b); NEXT; }

#else

#define OP1_BEGIN(op,tok,b,a) case op:
#define OP1_DEFAULT() default:
#define OP1_END I = (I>>3) & 15; goto dispatch2
#define OPx_END NEXT

#define OP2_BEGIN(op,tok,b,a) case op:
#define OP2_DEFAULT() default:
#define OP2_END NEXT

#endif

#define DISPATCH1_BEGIN()			\
    I = *IP++;					\
    if (I < 0x80) goto dispatch2;		\
    switch(I & 7)
#define DISPATCH1_END()

#define DISPATCH2_BEGIN() dispatch2: switch(I)
#define DISPATCH2_END()


#define SWAP_IN(mp)				\
    IP = (mp)->IP;				\
    SP = (mp)->SP;				\
    RP = (mp)->RP

#define SWAP_OUT(mp)				\
    (mp)->IP = IP;				\
    (mp)->SP = SP;				\
    (mp)->RP = RP

int chine_run(chine_t* mp)
{
    uint8_t*  IP; 
    int32_t*  SP;
    int32_t*  RP;
    uint8_t   I;
    uint8_t   J;

    SWAP_IN(mp);

#define NEXT  goto next
#define FAIL  goto fail

next:
    usleep(1000);
    DISPATCH1_BEGIN() {
	OP1_BEGIN(ZBRAN_H,"[zbranch.h]",1,0) {
	    int8_t j = ((int8_t)(I<<1))>>4;
	    int32_t r = *SP++;
	    if (r) IP += 0; else IP += (j);
	} OPx_END;
	OP1_BEGIN(LIT_H,"[literal.h]",0,1) {
	    int8_t j = ((int8_t)(I<<1))>>4;
	    *--SP = j;
	} OPx_END;
	OP1_BEGIN(DUP,"[dup]",1,2) {
	    SP[-1] = SP[0]; SP--;
	} OP1_END;
	OP1_BEGIN(ROT,"[rot]",3,3) {
	    int32_t r = SP[2];
	    SP[2] = SP[1];
	    SP[1] = SP[0];
	    SP[0] = r;
	} OP1_END;
	OP1_BEGIN(OVER,"[over]",2,3) {
	    SP--;
	    SP[0] = SP[2];
	} OP1_END;
	OP1_BEGIN(DROP,"[drop]",1,0) {
	    SP++;
	} OP1_END;
	OP1_BEGIN(SWAP,"[swap]",2,2) {
	    int32_t r = SP[0]; SP[0] = SP[1]; SP[1] = r; 
	} OP1_END;
	OP1_BEGIN(SUB,"[-]",2,1) {
	    SP[1] -= SP[0]; SP++;
	} OP1_END;
    } DISPATCH1_END();

    DISPATCH2_BEGIN() {
	OP2_BEGIN(ZBRAN_B,"zbranch.b",1,0) {
	    int8_t j = INT8(IP);
	    int32_t r = *SP++;
	    if (r) IP += 1; else IP += (j+1); 
	} OP2_END;
	OP2_BEGIN(LIT_H,"literal.b",0,1) {
	    *--SP = INT8(IP);  IP += 1; 
	} OP2_END;
	OP2_BEGIN(DUP,"dup",1,2) {
	    SP[-1] = SP[0]; SP--; 
	} OP2_END;
	OP2_BEGIN(ROT,"rot",3,3) {
	    int32_t r = SP[2];
	    SP[2] = SP[1];
	    SP[1] = SP[0];
	    SP[0] = r;
	} OP2_END;
	OP2_BEGIN(OVER,"over",2,3) {
	    SP--;
	    SP[0] = SP[2];
	} OP2_END;
	OP2_BEGIN(DROP,"drop",1,0) {
	    SP++;
	} OP2_END;
	OP2_BEGIN(SWAP,"swap",2,2) {
	    int32_t r = SP[0]; SP[0] = SP[1]; SP[1] = r; 
	} OP2_END;
	OP2_BEGIN(SUB,"-",2,1) {
	    SP[1] -= SP[0]; SP++;
	} OP2_END;

	OP2_BEGIN(ADD,"+",2,1) {
	    SP[1] += SP[0]; SP++;
	} OP2_END;
	OP2_BEGIN(MUL,"*",2,1)  { 
	    SP[1] *= SP[0]; SP++;
	} OP2_END;
	OP2_BEGIN(EQ,"=",2,1) {
	    SP[1] = (SP[0] == SP[1]);
	    SP++;
	} OP2_END;
	OP2_BEGIN(AND,"and",2,1) {
	    SP[1] &= SP[0]; SP++;
	} OP2_END;
	OP2_BEGIN(OR,"or",2,1) {
	    SP[1] |= SP[0]; SP++;
	} OP2_END;
	OP2_BEGIN(ZEQ,"0=",1,1) {
	    SP[0] = (SP[0] == 0);
	} OP2_END;
	OP2_BEGIN(ZLT,"0<",1,1) {
	    SP[0] = (SP[0] < 0);
	} OP2_END;
	OP2_BEGIN(NOT,"not",1,1) {
	    SP[0] = !SP[0];
	} OP2_END;

	OP2_BEGIN(DIV,"/",2,1) {
	    if (SP[0] == 0) { SP += 2; FAIL; }
	    SP[1] /= SP[0];
	    SP++;
	} OP2_END;
	OP2_BEGIN(MOD,"mod",2,1)  {
	    if (SP[0] == 0) { SP += 2; FAIL; }
	    SP[1] %= SP[0];
	    SP++;
	} OP2_END;

	OP2_BEGIN(XOR,"xor",2,1)	{
	    SP[1] ^= SP[0]; SP++;
	} OP2_END;

	OP2_BEGIN(NEG,"negate",1,1) {
	    SP[0] = -SP[0];
	} OP2_END;
	OP2_BEGIN(INV,"invert",1,1) {
	    SP[0] = ~SP[0];
	} OP2_END;
	OP2_BEGIN(BSL,"<<",2,1) {
	    SP[1] = ((uint32_t)SP[1]) << ((uint32_t)SP[0]); SP++;
	} OP2_END;
	OP2_BEGIN(BSR,">>",2,1) {
	    SP[1] = ((uint32_t)SP[1]) >> ((uint32_t)SP[0]); SP++;
	} OP2_END;
	OP2_BEGIN(ASR,">>a",2,1) {
	    SP[1] = ((int32_t)SP[1]) >> ((uint32_t)SP[0]); SP++;
	} OP2_END;
	OP2_BEGIN(ULT,"u<",2,1) { 
	    int32_t r = *SP++; *SP = ((uint32_t)*SP < (uint32_t)r); 
	} OP2_END;
	OP2_BEGIN(BRAN_B,"branch.b",0,0) {
	    int8_t  j = INT8(IP); IP += (j+1); 
	} OP2_END;
	OP2_BEGIN(BRAN_W,"branch.w",0,0) {
	    int16_t j = INT16(IP); IP += (j+2); 
	} OP2_END;
	OP2_BEGIN(ZBRAN_W,"zbranch.w",1,0) {
	    int16_t j = INT16(IP);
	    int32_t r = *SP++;
	    if (r) IP += 2; else IP += (j+2);
	} OP2_END;
	OP2_BEGIN(IBRAN_B,"ibranch.b",1,0) {
	    uint8_t n = UINT8(IP);
	    int32_t i = *SP++;
	    if ((i < 0) || (i >= n))
		IP += (1+n);  /* skip when out of range */
	    else {
		int8_t j = INT8(IP+1+i);
		IP += (1+n+j);
	    }
	} OP2_END;

	OP2_BEGIN(IBRAN_W,"ibranch.w",1,0) {
	    uint16_t n = UINT16(IP);
	    int32_t i = *SP++;
	    if ((i < 0) || (i >= n))
		IP += (2+2*n);  /* skip when out of range */
	    else {
		int16_t j = INT16(IP+2+2*i);
		IP += (2+2*n+j);
	    }
	} OP2_END;

	OP2_BEGIN(LIT_W,"literal.w",0,1) {
	    *--SP = INT16(IP); IP += 2; 
	} OP2_END;

	OP2_BEGIN(LIT_L,"literal.l",0,1) {
	    *--SP = INT32(IP); IP += 4;
	} OP2_END;

	OP2_BEGIN(NOP,"nop",0,0) {
	} OP2_END;

	OP2_BEGIN(STORE,"!",2,0) {
	    int32_t i = SP[0];
	    if ((i < 0) || (i >= MAX_MEM)) FAIL;
	    mp->mem[i] = SP[1];
	    SP += 2;
	} OP2_END;
	OP2_BEGIN(FETCH,"@",1,1) {
	    int32_t i = SP[0];
	    if ((i < 0) || (i >= MAX_MEM)) FAIL;
	    SP[0] = mp->mem[i]; 
	} OP2_END;
	OP2_BEGIN(ULTE,"u<=",2,1) { 
	    int32_t r = *SP++; *SP = ((uint32_t)*SP <= (uint32_t)r); 
	} OP2_END;
	OP2_BEGIN(CALL_B,"call.b",0,0) {
	    int8_t j = INT8(IP);
	    *--RP = ((IP+1) - mp->prog);
	    IP += (j+1);
	} OP2_END;
	OP2_BEGIN(CALL_W,"call.w",0,0) {
	    int8_t j = INT16(IP);
	    *--RP = ((IP+2) - mp->prog);
	    IP += (j+2);
	} OP2_END;

	OP2_BEGIN(RET,";",0,0) {
	    IP = mp->prog + *RP++;
	} OP2_END;

	OP2_BEGIN(SYS_B,"sys.b",0,0) {
	    uint8_t sysop = UINT8(IP);
	    int32_t ret;
	    int32_t npop;
	    int32_t value;

	    IP++;
	    if ((ret = (*mp->sys)(mp, sysop, SP, &npop, &value)) < 0) {
		mp->err = ret;
		goto fail;
	    }
	    SP += npop; // pop arguments
	    if (ret > 0) {
		*--SP = value;
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
	    FAIL;
	} OP2_END;
	
    } DISPATCH2_END();

fail:
    SWAP_OUT(mp);
    return -1;
#undef NEXT
#undef FAIL
}
