#include <stdio.h>
#include <memory.h>
#include <unistd.h>
#include "../../include/chine.h"

chine_t m1;
chine_t m2;
chine_t m3;

uint8_t prog1[] = { LIT_B, 1, SYS_B, SYS_TIMER_INIT,
		    // L1:
		    LIT_B, 1, SYS_B, SYS_TIMER_START,
		    LIT_B, 1, SYS_B, SYS_SELECT_TIMER,
		    // L2:
		    LIT_B, 1, SYS_B, SYS_TIMER_TIMEOUT,
		    ZBRAN_B, 6,
		    LIT_B, '1', SYS_B, SYS_EMIT,
		    BRAN_B, U8(-20),
		    YIELD,
		    BRAN_B, U8(-15) };

uint8_t prog2[] = { LIT_B, 2, SYS_B, SYS_TIMER_INIT,
		    LIT_B, 2, SYS_B, SYS_TIMER_START,
		    LIT_B, 2, SYS_B, SYS_SELECT_TIMER,
		    LIT_B, 2, SYS_B, SYS_TIMER_TIMEOUT,
		    ZBRAN_B, 6,
		    LIT_B, '2', SYS_B, SYS_EMIT,
		    BRAN_B, U8(-20),
		    YIELD,
		    BRAN_B, U8(-15) };

uint8_t prog3[] = { LIT_B, 3, SYS_B, SYS_TIMER_INIT,
		    LIT_B, 3, SYS_B, SYS_TIMER_START,
		    LIT_B, 3, SYS_B, SYS_SELECT_TIMER,
		    LIT_B, 3, SYS_B, SYS_TIMER_TIMEOUT,
		    ZBRAN_B, 6,
		    LIT_B, '3', SYS_B, SYS_EMIT,
		    BRAN_B, U8(-20),
		    YIELD,
		    BRAN_B, U8(-15) };

extern int32_t chine_unix_sys(chine_t* mp,
			      int32_t sysop, int32_t* revarg,
			      int32_t* npop, int32_t* value);

void init()
{
    chine_init(&m1, prog1, chine_unix_sys);
    chine_init(&m2, prog2, chine_unix_sys);
    chine_init(&m3, prog3, chine_unix_sys);
}

int main()
{
    int r1 = 0;
    int r2 = 0;
    int r3 = 0;
    int n;
    chine_t* mv[3];
    uint8_t imask[NUM_IBYTES];   // input mask
    timeout_t tmo = 0;

    init();

again:
    tmo = 0;
    n = 0;
    memset(&imask, 0, sizeof(imask));

    if (r1 == 0)
	r1 = chine_run(&m1);
    if (r1 == 1)
	mv[n++] = &m1;
    
    if (r2 == 0)
	r2 = chine_run(&m2);
    if (r2 == 1)
	mv[n++] = &m2;

    if (r3 == 0)
	r3 = chine_run(&m3);
    if (r3 == 1)
	mv[n++] = &m3;

    chine_next(mv, n, &tmo, imask);
    // printf("n = %d, tmo = %d, imask[0]=%u\n", n, tmo, imask[0]);
    if ((n == 3) && (tmo > 0))
	usleep(tmo*1000);
    r1 = r2 = r3 = 0;
    goto again;
}
