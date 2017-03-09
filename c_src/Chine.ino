
#include "../include/chine.h"

// #if defined(ARDUINO)
// #include <avr/pgmspace.h>
// #else
// #define pgm_read_byte(ptr) *((byte*)ptr)
// #endif

chine_t m;

#include "blink.prog"

extern int chine_arduino_sys(chine_t* mp,
			     cell_t sysop, cell_t* revarg,
			     cell_t* npop, cell_t* value);

void setup()
{
    chine_init(&m, prog, chine_arduino_sys);
}

void loop()
{
    uint8_t imask[NUM_IBYTES];   // input mask
    timeout_t tmo;

    chine_run(&m);

    tmo = 0xffffffff;
    memset(&imask, 0, sizeof(imask));
    if (chine_next(&m, &tmo, imask)) { // wait for input or timer
	if (tmo < 0xffffffff) {
	    delay(tmo);
	}
    }
}
