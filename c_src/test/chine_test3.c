#include <stdio.h>
#include <memory.h>
#include <unistd.h>
#include "../../include/chine.h"

chine_t m1;
chine_t m2;
chine_t m3;

// 1 use timer number 1, (default 1s)
uint8_t prog1[] = { PUSH8(1), SYS, SYS_TIMER_INIT,
		    // L1:
		    PUSH8(1), SYS, SYS_TIMER_START,
		    PUSH8(1), SYS, SYS_SELECT_TIMER,
		    // L2:
		    PUSH8(1), SYS, SYS_TIMER_TIMEOUT,
		    JOP8(JMPZ,6),
		    PUSH8('1'), SYS, SYS_EMIT,
		    JOP8(JMP,-20),
		    YIELD,
		    JOP8(JMP,-15) };

// 2 use second timer (default 2s)
uint8_t prog2[] = { PUSH8(2), SYS, SYS_TIMER_INIT,
		    PUSH8(2), SYS, SYS_TIMER_START,
		    PUSH8(2), SYS, SYS_SELECT_TIMER,
		    PUSH8(2), SYS, SYS_TIMER_TIMEOUT,
		    JOP8(JMPZ,6),
		    PUSH8('2'), SYS, SYS_EMIT,
		    JOP8(JMP,-20),
		    YIELD,
		    JOP8(JMP,-15) };

// 3 use third timer (default 3s)
uint8_t prog3[] = { PUSH8(3), SYS, SYS_TIMER_INIT,
		    PUSH8(3), SYS, SYS_TIMER_START,
		    PUSH8(3), SYS, SYS_SELECT_TIMER,
		    PUSH8(3), SYS, SYS_TIMER_TIMEOUT,
		    JOP8(JMPZ,6),
		    PUSH8('3'), SYS, SYS_EMIT,
		    JOP8(JMP,-20),
		    YIELD,
		    JOP8(JMP,-15) };

extern int chine_unix_sys(chine_t* mp,
			  cell_t sysop, cell_t* revarg,
			  cell_t* npop, cell_t* value);

void setup()
{
    chine_init(&m1, prog1, chine_unix_sys);
    chine_set_ip(&m1, 0);
    chine_init(&m2, prog2, chine_unix_sys);
    chine_set_ip(&m2, 0);
    chine_init(&m3, prog3, chine_unix_sys);
    chine_set_ip(&m3, 0);
}

void loop()
{
    int i, r=0;
    chine_t* mv[3] = { &m1, &m2, &m3 };
    uint8_t imask[NUM_IBYTES];   // input mask
    timeout_t tmo = 0xffffffff;

    memset(&imask, 0, sizeof(imask));
    for (i = 0; i < 3; i++) {
	chine_run(mv[i]);
	r |= chine_next(mv[i], &tmo, imask);
    }
    if (r) {
	if (tmo < 0xffffffff) {
	    usleep(tmo*1000);
	}
    }
}

int main()
{
    setup();

    while(1) 
	loop();
}
