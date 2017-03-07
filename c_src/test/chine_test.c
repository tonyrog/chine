#include <stdio.h>
#include <stdlib.h>
#include <assert.h>
#include <unistd.h>
#include <memory.h>
#include <sys/time.h>

#include "../../include/chine.h"

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
	return m.cSP[0];
    case 1: {
	uint8_t  imask[NUM_IBYTES];   // input mask
	timeout_t tmo = 0;
	chine_t* mv[1];
	mv[0] = &m;
	memset(&imask, 0, sizeof(imask));
	chine_next(mv, 1, &tmo, imask);
	// printf("tmo = %d, imask = %08x\n", tmo, imask);
	if (tmo > 0) usleep(tmo*1000);
	goto again;
    }
    default:
	printf("fail: err = %d\n", m.cErr);
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
    uint8_t prog1[] = { PUSH8(105), PUSH8(34), ADD, EXIT };
    uint8_t prog2[] = { PUSH8(105), PUSH8(34), SUB, EXIT };
    uint8_t prog3[] = { PUSH8(105), PUSH8(34), MUL, EXIT };
    uint8_t prog4[] = { PUSH8(105), PUSH8(34), DIV, EXIT };
    uint8_t prog5[] = { PUSH8(105), PUSH8(34), S_MOD, EXIT };
    uint8_t prog6[] = { PUSH8(105), PUSH8(34), AND, EXIT };
    uint8_t prog7[] = { PUSH8(105), PUSH8(34), OR, EXIT };
    uint8_t prog8[] = { PUSH8(105), PUSH8(34), XOR, EXIT };
    uint8_t prog9[] = { PUSH8(105), NEG, EXIT };
    uint8_t prog10[] = { PUSH8(105), INV, EXIT };
    uint8_t prog11[] = { PUSH8(105),  PUSHi(2), BSL, EXIT };
    uint8_t prog12[] = { PUSH8(-105), PUSHi(2), BSR, EXIT };
    uint8_t prog13[] = { PUSH8(-105), PUSHi(2), S_ASR, EXIT };

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
    uint8_t prog1[] = { PUSH8(105), ZEQ, EXIT };
    uint8_t prog2[] = { PUSHi(0), ZEQ, EXIT };
    uint8_t prog3[] = { PUSH8(-105), ZLT, EXIT };
    uint8_t prog4[] = { PUSH8(105), ZLT, EXIT };
    uint8_t prog5[] = { PUSH8(105), PUSH8(34), S_LT, EXIT };
    uint8_t prog6[] = { PUSH8(105), PUSH8(-34), ULT, EXIT };
    
    assert(test_code_pop(prog1, sizeof(prog1)) == 0);
    assert(test_code_pop(prog2, sizeof(prog2)) == 1);
    assert(test_code_pop(prog3, sizeof(prog3)) == 1);
    assert(test_code_pop(prog4, sizeof(prog4)) == 0);
    assert(test_code_pop(prog5, sizeof(prog5)) == 0);
    assert(test_code_pop(prog6, sizeof(prog6)) == 1);
}

void test_misc()
{
    uint8_t prog1[] = { PUSH8(105), S_INC, EXIT };
    uint8_t prog2[] = { PUSH8(105), S_DEC, EXIT };
    uint8_t prog3[] = { PUSH8(-105), S_ABS, EXIT };
    uint8_t prog4[] = { PUSH8(105), PUSH8(34), S_MIN, EXIT };
    uint8_t prog5[] = { PUSH8(105), PUSH8(34), S_MAX, EXIT };
    uint8_t prog6[] = { PUSH8(34), DUP, MUL, EXIT };
    uint8_t prog7[] = { PUSH8(34), PUSH8(105), SWAP, EXIT };

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
    uint8_t prog1[] = { PUSH8(105), PUSHi(1), STORE,
			PUSHi(1), FETCH, EXIT };
    uint8_t prog2[] = { PUSH8(-34), PUSHi(0), STORE,
			PUSHi(0), FETCH, EXIT };

    assert(test_code_pop(prog1, sizeof(prog1)) == 105);
    assert(test_code_pop(prog2, sizeof(prog2)) == -34);
}

void test_branch()
{
    uint8_t prog1[] = { JOP8(JMP,2), PUSH8(10), PUSH8(20), EXIT };
    uint8_t prog2[] = { JOP16(JMP,2), PUSH8(10), PUSH8(20), EXIT };
    uint8_t prog3[] = { JOP8(JMP,3), PUSH8(20), EXIT, JOP8(JMP,-5), EXIT };
    uint8_t prog4[] = { JOP16(JMP,3), PUSH8(20), EXIT, JOP8(JMP,-5), EXIT };
    uint8_t prog5[] = { PUSHi(0), JOP8(JMPZ,2), PUSH8(10), PUSH8(20), EXIT };
    uint8_t prog6[] = { PUSHi(0), JOP16(JMPZ,2), PUSH8(10), PUSH8(20), EXIT };
    uint8_t prog7[] = { PUSHi(1), JOP8(JMPI,4), 4, 8, 12, 16,
			PUSH8(-1), JOP8(JMP,16),
			PUSH8(10), JOP8(JMP,12),
			PUSH8(20), JOP8(JMP,8),
			PUSH8(30), JOP8(JMP,4),
			PUSH8(40), JOP8(JMP,0),
			EXIT };
    uint8_t prog8[] = { PUSHi(2),  JOP16(JMPI,4), 0, 4, 0, 8, 0, 12, 0, 16,
			PUSH8(-1), JOP8(JMP,16),
			PUSH8(10), JOP8(JMP,12),
			PUSH8(20), JOP8(JMP,8),
			PUSH8(30), JOP8(JMP,4),
			PUSH8(40), JOP8(JMP,0),
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
    uint8_t prog1[] = { JOP8(CALL,1), EXIT, PUSH8(10), RET };
    uint8_t prog2[] = { PUSH8(5), PUSH8(15), JOP8(CALL,1), EXIT, ADD, RET };
    uint8_t prog3[] = { PUSH8(5), JOP8(CALL,1), EXIT,
			// FACT
			DUP,  // ( n -- r n )
			// LOOP
			PUSHi(1), SUB, // ( r n -- r n-1 )  = DEC
			DUP, PUSHi(1), S_EQ, JOPi(JMPZ,2),
			DROP, RET,
			// DUP,   // ( r n -- r n n )
			// ROT,  // ( r n n -- n r n )
			OPOP(DUP,ROT),
			MUL,   // ( n r n -- n r*n )
			SWAP,  // ( n r - r n )
			JOP8(JMP,-14) };

    assert(test_code_pop(prog1, sizeof(prog1)) == 10);
    assert(test_code_pop(prog2, sizeof(prog2)) == 20);
    assert(test_code_pop(prog3, sizeof(prog3)) == 120);
}

void test_syscall()
{
    uint8_t prog0[] = { PUSH8('w'), SYS, SYS_EMIT,
			PUSH8('a'), SYS, SYS_EMIT,
			PUSH8('i'), SYS, SYS_EMIT,
			PUSH8('t'), SYS, SYS_EMIT,
			PUSH8('\n'), SYS, SYS_EMIT,
			PUSH8(5),
			EXIT};

    uint8_t prog1[] = {  PUSH8(1), SYS, SYS_TIMER_INIT,
			 PUSH8(1), SYS, SYS_TIMER_START,
			 PUSH8(1), SYS, SYS_SELECT_TIMER,
			 PUSH8(1), SYS, SYS_TIMER_TIMEOUT,
			 JOP8(JMPZ,7),
			 PUSH8(1), SYS, SYS_TIMER_STOP,
			 PUSH8(100), EXIT,
			 YIELD,
			 JOP8(JMP,-16) };
    uint8_t prog9[] = { PUSH8('>'), SYS, SYS_EMIT,
			PUSH8(' '), SYS, SYS_EMIT,
			// LABEL L1
			SYS, SYS_KEY,
			DUP, SYS, SYS_EMIT,
			PUSH8('\n'), S_EQ,
			JOP8(JMPZ,-10),
			PUSHi(1),
			EXIT};

    uint8_t prog10[] = { PUSH8('o'), SYS, SYS_EMIT,
			 PUSH8('k'), SYS, SYS_EMIT,
			 PUSH8('\n'), SYS, SYS_EMIT,
			 PUSH8(3),
			 EXIT};
    assert(test_code_pop(prog0, sizeof(prog0)) == 5);
    assert(test_code_pop(prog1, sizeof(prog1)) == 100);
    assert(test_code_pop(prog9, sizeof(prog9)) == 1);
    assert(test_code_pop(prog10, sizeof(prog10)) == 3);
}



int main()
{
    printf("test_integer_macros\n");
    test_integer_macros();
    printf("test_arithmetic\n");
    test_arithmetic();
    printf("test_comp\n");
    test_comp();
    printf("test_misc\n");
    test_misc();
    printf("test_mem\n");
    test_mem();
    printf("test_branch\n");
    test_branch();
    printf("test_call\n");
    test_call();
    printf("test_syscall\n");
    test_syscall();
    exit(0);
}
