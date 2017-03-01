#include <stdio.h>
#include <stdlib.h>
#include <assert.h>
#include <unistd.h>
#include <sys/time.h>

#include "../include/chine.h"

extern int32_t chine_unix_sys(chine_t* mp,
			      int32_t sysop, int32_t* revarg,
			      int32_t* npop, int32_t* value);

int32_t test_code_pop(uint8_t* prog, size_t len)
{
    chine_t m;
    int i;

    chine_init(&m, prog, chine_unix_sys);
    for (i = 0; i < MAX_STACK; i++)
	m.stack[i] = -1;
again:
    switch(chine_run(&m)) {
    case 0:
	return m.SP[0];
    case 1: {
	uint32_t imask = 0;
	int32_t  tmo = 0;
	chine_t* mv[1];
	mv[0] = &m;
	chine_next(mv, 1, &tmo, &imask);
	// printf("tmo = %d, imask = %08x\n", tmo, imask);
	if (tmo > 0) usleep(tmo*1000);
	goto again;
    }
    default:
	printf("fail: err = %d\n", m.err);
	break;
    }
    return -1;
}

void test_integer_macros()
{
    uint8_t data1[] = {189};
    uint8_t data2[] = {229,135};
    uint8_t data3[] = {255,67,158,178};

    assert(INT8(data1) == -67);
    assert(UINT8(data1) == 189);
    assert(INT16(data2) == -6777);
    assert(UINT16(data2) == 58759);
    assert(INT32(data3) == -12345678);
    assert(UINT32(data3) == 4282621618);
}

void test_arithmetic()
{
    uint8_t prog1[] = { LIT_B, 105, LIT_B, 34, ADD, EXIT };
    uint8_t prog2[] = { LIT_B, 105, LIT_B, 34, SUB, EXIT };
    uint8_t prog3[] = { LIT_B, 105, LIT_B, 34, MUL, EXIT };
    uint8_t prog4[] = { LIT_B, 105, LIT_B, 34, DIV, EXIT };
    uint8_t prog5[] = { LIT_B, 105, LIT_B, 34, MOD, EXIT };
    uint8_t prog6[] = { LIT_B, 105, LIT_B, 34, AND, EXIT };
    uint8_t prog7[] = { LIT_B, 105, LIT_B, 34, OR, EXIT };
    uint8_t prog8[] = { LIT_B, 105, LIT_B, 34, XOR, EXIT };
    uint8_t prog9[] = { LIT_B, 105, NEG, EXIT };
    uint8_t prog10[] = { LIT_B, 105, INV, EXIT };
    uint8_t prog11[] = { LIT_B, 105,  S_PUSH(2), BSL, EXIT };
    uint8_t prog12[] = { LIT_B, -105, S_PUSH(2), BSR, EXIT };
    uint8_t prog13[] = { LIT_B, -105, S_PUSH(2), ASR, EXIT };

    assert(test_code_pop(prog1, sizeof(prog1)) == 139);
    assert(test_code_pop(prog2, sizeof(prog2)) == 71);
    assert(test_code_pop(prog3, sizeof(prog3)) == 3570);
    assert(test_code_pop(prog4, sizeof(prog4)) == 3);
    assert(test_code_pop(prog5, sizeof(prog5)) == 3);
    assert(test_code_pop(prog6, sizeof(prog6)) == 32);
    assert(test_code_pop(prog7, sizeof(prog7)) == 107);
    assert(test_code_pop(prog8, sizeof(prog8)) == 75);
    assert(test_code_pop(prog9, sizeof(prog9)) == -105);
    assert(test_code_pop(prog10, sizeof(prog10)) == -106);

    assert(test_code_pop(prog11, sizeof(prog11)) == 420);
    assert(test_code_pop(prog12, sizeof(prog12)) == 1073741797);
    assert(test_code_pop(prog13, sizeof(prog13)) == -27);
}

void test_comp()
{
    uint8_t prog1[] = { LIT_B, 105, ZEQ, EXIT };
    uint8_t prog2[] = { S_PUSH(0), ZEQ, EXIT };
    uint8_t prog3[] = { LIT_B, -105, ZLT, EXIT };
    uint8_t prog4[] = { LIT_B, 105, ZLT, EXIT };
    uint8_t prog5[] = { LIT_B, 105, LIT_B, 34, LT, EXIT };
    uint8_t prog6[] = { LIT_B, 105, LIT_B, -34, ULT, EXIT };
    
    assert(test_code_pop(prog1, sizeof(prog1)) == 0);
    assert(test_code_pop(prog2, sizeof(prog2)) == 1);
    assert(test_code_pop(prog3, sizeof(prog3)) == 1);
    assert(test_code_pop(prog4, sizeof(prog4)) == 0);
    assert(test_code_pop(prog5, sizeof(prog5)) == 0);
    assert(test_code_pop(prog6, sizeof(prog6)) == 1);
}

void test_misc()
{
    uint8_t prog1[] = { LIT_B, 105, INC, EXIT };
    uint8_t prog2[] = { LIT_B, 105, DEC, EXIT };
    uint8_t prog3[] = { LIT_B, -105, ABS, EXIT };
    uint8_t prog4[] = { LIT_B, 105, LIT_B, 34, MIN, EXIT };
    uint8_t prog5[] = { LIT_B, 105, LIT_B, 34, MAX, EXIT };
    uint8_t prog6[] = { LIT_B, 34, DUP, MUL, EXIT };
    uint8_t prog7[] = { LIT_B, 34, LIT_B, 105, SWAP, EXIT };

    assert(test_code_pop(prog1, sizeof(prog1)) == 106);
    assert(test_code_pop(prog2, sizeof(prog2)) == 104);
    assert(test_code_pop(prog3, sizeof(prog3)) == 105);
    assert(test_code_pop(prog4, sizeof(prog4)) == 34);
    assert(test_code_pop(prog5, sizeof(prog5)) == 105);
    assert(test_code_pop(prog6, sizeof(prog6)) == 1156);
    assert(test_code_pop(prog7, sizeof(prog7)) == 34);
}

void test_mem()
{
    uint8_t prog1[] = { LIT_B, 105, S_PUSH(1), STORE,
			S_PUSH(1), FETCH, EXIT };
    uint8_t prog2[] = { LIT_B, -34, S_PUSH(0), STORE,
			S_PUSH(0), FETCH, EXIT };

    assert(test_code_pop(prog1, sizeof(prog1)) == 105);
    assert(test_code_pop(prog2, sizeof(prog2)) == -34);
}

void test_branch()
{
    uint8_t prog1[] = { BRAN_B, 2, LIT_B, 10, LIT_B, 20, EXIT };
    uint8_t prog2[] = { BRAN_W, 0, 2, LIT_B, 10, LIT_B, 20, EXIT };
    uint8_t prog3[] = { BRAN_B, 3, LIT_B, 20, EXIT, BRAN_B, -5, EXIT };
    uint8_t prog4[] = { BRAN_W, 0, 3, LIT_B, 20, EXIT, BRAN_B, -5, EXIT };
    uint8_t prog5[] = { S_PUSH(0), ZBRAN_B, 2, LIT_B, 10, LIT_B, 20, EXIT };
    uint8_t prog6[] = { S_PUSH(0), ZBRAN_W, 0, 2, LIT_B, 10, LIT_B, 20, EXIT };
    uint8_t prog7[] = { S_PUSH(1), IBRAN_B, 4, 4, 8, 12, 16,
			LIT_B, -1, BRAN_B, 16,
			LIT_B, 10, BRAN_B, 12,
			LIT_B, 20, BRAN_B, 8,
			LIT_B, 30, BRAN_B, 4,
			LIT_B, 40, BRAN_B, 0,
			EXIT };
    uint8_t prog8[] = { S_PUSH(2), IBRAN_W, 0, 4, 0, 4, 0, 8, 0, 12, 0, 16,
			LIT_B, -1, BRAN_B, 16,
			LIT_B, 10, BRAN_B, 12,
			LIT_B, 20, BRAN_B, 8,
			LIT_B, 30, BRAN_B, 4,
			LIT_B, 40, BRAN_B, 0,
			EXIT };

    assert(test_code_pop(prog1, sizeof(prog1)) == 20);
    assert(test_code_pop(prog2, sizeof(prog2)) == 20);
    assert(test_code_pop(prog3, sizeof(prog3)) == 20);
    assert(test_code_pop(prog4, sizeof(prog4)) == 20);
    assert(test_code_pop(prog5, sizeof(prog5)) == 20);
    assert(test_code_pop(prog6, sizeof(prog6)) == 20);
    assert(test_code_pop(prog7, sizeof(prog7)) == 20);
    assert(test_code_pop(prog8, sizeof(prog8)) == 30);
}

void test_call()
{
    uint8_t prog1[] = { CALL_B, 1, EXIT, LIT_B, 10, RET };
    uint8_t prog2[] = { LIT_B,15, LIT_B,5, CALL_B, 1, EXIT, ADD, RET };
    uint8_t prog3[] = { LIT_B, 5, CALL_B, 1, EXIT,
			// FACT
			DUP,  // ( n -- r n )
			// LOOP
			S_PUSH(1), SUB, // ( r n -- r n-1 )  = DEC
			DUP, S_PUSH(1), EQ, OPCODE2(ZBRAN_H,2),
			DROP, RET,
			// DUP,   // ( r n -- r n n )
			// ROT,  // ( r n n -- n r n )
			OPCODE2(DUP,ROT),
			MUL,   // ( n r n -- n r*n )
			SWAP,  // ( n r - r n )
			BRAN_B, -13 };

    assert(test_code_pop(prog1, sizeof(prog1)) == 10);
    assert(test_code_pop(prog2, sizeof(prog2)) == 20);
    assert(test_code_pop(prog3, sizeof(prog3)) == 120);
}

void test_syscall()
{
    uint8_t prog1[] = {  LIT_B, 1, SYS_B, SYS_TIMER_INIT,
			 LIT_B, 1, SYS_B, SYS_TIMER_START,
			 LIT_B, 2, LIT_B, 0, SYS_B, SYS_SELECT,
			 LIT_B, 1, SYS_B, SYS_TIMER_TIMEOUT,
			 ZBRAN_B, 7,
			 LIT_B, 1, SYS_B, SYS_TIMER_STOP,
			 LIT_B, 100, EXIT,
			 YIELD,
			 BRAN_B, -16 };

    uint8_t prog10[] = { LIT_B, 'o', SYS_B, SYS_EMIT,
			 LIT_B, 'k', SYS_B, SYS_EMIT,
			 LIT_B, '\n', SYS_B, SYS_EMIT,
			 LIT_B, 3,
			 EXIT};
    assert(test_code_pop(prog1, sizeof(prog1)) == 100);
    assert(test_code_pop(prog10, sizeof(prog10)) == 3);
}



int main()
{
#if 0
    test_integer_macros();
    test_arithmetic();
    test_comp();
    test_misc();
    test_mem();
    test_branch();
    test_call();
#endif
    test_syscall();
    exit(0);
}
