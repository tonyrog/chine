
#include "../include/chine.h"

// #if defined(ARDUINO)
// #include <avr/pgmspace.h>
// #else
#define PROGMEM
// #define pgm_read_byte(ptr) *((byte*)ptr)
// #endif

chine_t m1;
chine_t m2;
chine_t m3;

uint8_t prog1[] PROGMEM =
{ PUSH8(1), SYS, SYS_TIMER_INIT,
  PUSH8(1), SYS, SYS_TIMER_START,
  PUSH8(1), SYS, SYS_SELECT_TIMER,
  PUSH8(1), SYS, SYS_TIMER_TIMEOUT,
  JOP8(JMPZ,6),
  PUSH8('1'), SYS, SYS_EMIT,
  JOP8(JMP,-20),
  YIELD,
  JOP8(JMP,-15) };

uint8_t prog2[] PROGMEM =
{ PUSH8(2), SYS, SYS_TIMER_INIT,
  PUSH8(2), SYS, SYS_TIMER_START,
  PUSH8(2), SYS, SYS_SELECT_TIMER,
  PUSH8(2), SYS, SYS_TIMER_TIMEOUT,
  JOP8(JMPZ,6),
  PUSH8('2'), SYS, SYS_EMIT,
  JOP8(JMP,-20),
  YIELD,
  JOP8(JMP,-15) };

uint8_t prog3[] PROGMEM =
{ PUSH8(3), SYS, SYS_TIMER_INIT,
  PUSH8(3), SYS, SYS_TIMER_START,
  PUSH8(3), SYS, SYS_SELECT_TIMER,
  PUSH8(3), SYS, SYS_TIMER_TIMEOUT,
  JOP8(JMPZ,6),
  PUSH8('3'), SYS, SYS_EMIT,
  JOP8(JMP,-20),
  YIELD,
  JOP8(JMP,-15) };

extern int32_t chine_arduino_sys(chine_t* mp,
				 int32_t sysop, int32_t* revarg,
				 int32_t* npop, int32_t* value);

void setup()
{
    chine_init(&m1, prog1, chine_arduino_sys);
    chine_init(&m2, prog2, chine_arduino_sys);
    chine_init(&m3, prog3, chine_arduino_sys);
}

void loop()
{
    int r1 = 0;
    int r2 = 0;
    int r3 = 0;
    int n;
    chine_t* mv[3];
    uint8_t imask[NUM_IBYTES];   // input mask
    timeout_t tmo = 0;

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
    if ((n == 3) && (tmo > 0))
	delay(tmo);
    r1 = r2 = r3 = 0;
    goto again;
}
