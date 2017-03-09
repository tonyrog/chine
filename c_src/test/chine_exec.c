#include <stdio.h>
#include <stdlib.h>
#include <memory.h>
#include <unistd.h>
#include <errno.h>

#include "../../include/chine.h"

chine_t m;

extern int32_t chine_unix_sys(chine_t* mp,
			      int32_t sysop, int32_t* revarg,
			      int32_t* npop, int32_t* value);

char prog[65537];

int main(int argc, char** argv)
{
    uint8_t imask[NUM_IBYTES];   // input mask
    timeout_t tmo;
    FILE* f;

    if ((f = fopen(argv[1], "rb")) == NULL) {
	fprintf(stderr, "chine_exec: unable to open file [%s] %s\n",
		argv[1], strerror(errno));
	exit(1);
    }

    if (fread(prog, sizeof(char), sizeof(prog), f) >= sizeof(prog)) {
	fprintf(stderr, "chine_exec: file [%s] too large\n", argv[1]);
	exit(1);
    }
    fclose(f);

    chine_init(&m, (uint8_t*)prog, chine_unix_sys);

again:
    chine_run(&m);
    tmo = 0xffffffff;
    memset(&imask, 0, sizeof(imask));

    if (chine_next(&m, &tmo, imask)) { // wait for input or timer
	if (tmo < 0xffffffff) {
	    usleep(tmo*1000);
	    goto again;
	}
    }
    printf("TOS = %d\n", m.cSP[0]);
    exit(0);
}
