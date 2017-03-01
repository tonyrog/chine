//
//  Test implmentation of sys callback for unix
// 

#include <stdio.h>
#include <stdint.h>
#include <assert.h>
#include <unistd.h>
#include <sys/time.h>

#include "../include/chine.h"

int32_t param[9];

struct {
    uint8_t  digital;
    uint16_t analog;
    int16_t  encoder;
} input[32];

struct timeval boot_time;

// time since program started in ms
static uint32_t time_tick(void)
{
    struct timeval now;
    struct timeval t;
    gettimeofday(&now, 0);
    timersub(&now, &boot_time, &t);
    return t.tv_sec*1000 + t.tv_usec/1000;
}

static void time_init()
{
    gettimeofday(&boot_time, 0);
}

static int32_t fetch(uint16_t index, uint8_t si, int32_t* value)
{
    if (si >= 3) return FAIL_BAD_ARG;
    switch(index) {
    case 0x2000: *value = param[0*3+si]; break;
    case 0x3000: *value = param[1*3+si]; break;
    case 0x4000: *value = param[2*3+si]; break;
    default: return FAIL_BAD_ARG;
    }
    return 0;
}

static int32_t store(uint16_t index, uint8_t si,int32_t value)
{
    if (si >= 3) return FAIL_BAD_ARG;
    switch(index) {
    case 0x2000: param[0*3+si]=value; break;
    case 0x3000: param[1*3+si]=value; break;
    case 0x4000: param[2*3+si]=value; break;
    default: return FAIL_BAD_ARG;
    }
    return 0;
}

//
// return: <  0   FAIL_xyz
//         == 0   return no value
//         >  0   return value in *value
// number of arguments to pop is return in npop 
//
int32_t chine_unix_sys(chine_t* mp,
		       int32_t sysop, int32_t* revarg,
		       int32_t* npop, int32_t* value)
{
    switch(sysop) {
    case SYS_INIT:
	time_init();
	return 0;
    case SYS_PARAM_FETCH:
	*npop = 2;
	return fetch(revarg[1], revarg[0], value);
    case SYS_PARAM_STORE:
	*npop = 3;
	return store(revarg[2], revarg[1], revarg[0]);
    case SYS_TIMER_STOP: // same as init
    case SYS_TIMER_INIT: {
	int32_t i = revarg[0];
	*npop = 1;
	if ((i < 0) || (i>=MAX_TIMERS)) return FAIL_TIMER_OVERFLOW;
	CLRBIT(mp->tbits, i);
	mp->timer[i] = 0;
	return 0;
    }

    case SYS_TIMER_START: {
	int32_t i = revarg[0];
	*npop = 1;
	if ((i < 0) || (i>=MAX_TIMERS)) return FAIL_TIMER_OVERFLOW;
	SETBIT(mp->tbits, i);
	mp->timer[i] = time_tick()+2000;  // FIXME: parameters!!!
	return 0;
    }

    case SYS_TIMER_TIMEOUT: {
	int32_t i = revarg[0];
	*npop = 1;
	if ((i < 0) || (i>=MAX_TIMERS)) return FAIL_TIMER_OVERFLOW;
	if (!TSTBIT(mp->tbits,i))
	    *value = 0;
	else
	    *value = (time_tick() >= mp->timer[i]);
	return 1;
    }

    case SYS_TIMER_RUNNING:  {
	int32_t i = revarg[0];
	*npop = 1;
	if ((i < 0) || (i>=MAX_TIMERS)) return FAIL_TIMER_OVERFLOW;
	*value = TSTBIT(mp->tbits, i) != 0;
	return 1;
    }

    case SYS_INPUT_FETCH: {
	int32_t k = revarg[0];
	int32_t i = revarg[1];
	*npop = 2;
	if ((i < 0) || (i >= 32)) return FAIL_BAD_ARG;
	switch(k) {
	case INPUT_BOOLEAN: *value = input[i].digital;
	case INPUT_ANALOG:  *value = input[i].analog;
	case INPUT_ENCODER: *value = input[i].encoder;
	default: return FAIL_BAD_ARG;
	}
	return 1;
    }

    case SYS_SELECT: {
	uint32_t imask = (uint32_t) revarg[0];
	uint32_t tmask = (uint32_t) revarg[1];
	mp->imask = imask;
	mp->tmask = tmask;
	printf("imask = 0x%08x, tmask = 0x%08x\n", imask, tmask);
	*npop = 2;
	return 0;
    }

    case SYS_EMIT: {
	int32_t c = revarg[0];
	*npop = 1;
	fputc(c, stdout);
	return 0;
    }
    case SYS_KEY: {
	*npop = 0;
	*value = -1;
	return 1;
    }
    default:
	*npop = 0;
	return FAIL_BAD_ARG;
    }
}

// check vector of machines for input poll and timeout
int chine_next(chine_t** mpv, size_t n, int32_t* tmop, uint32_t* imaskp)
{
    int32_t tmo = 0x7fffffff;
    uint32_t imask = 0;
    int i;

    for (i = 0; i < n; i++) {
	uint32_t tmask = mpv[i]->tmask;
	int t = 0;
	imask |= mpv[i]->imask;
	while(tmask && (tmo > 0)) {
	    if (tmask & (1 << t)) {
		uint32_t a = time_tick();
		uint32_t b = mpv[i]->timer[t];
		uint32_t r = b - a;
		int32_t remain = (int32_t) r;
		printf("remain = %d, tick=%u, timer=%u\n", remain, a, b);
		if (remain < 0)
		    remain = 0;
		if (remain < tmo)
		    tmo = remain;
	    }
	    tmask &= ~(1 << t);
	    t++;
	}
    }
    if (imaskp) *imaskp = imask;
    if (tmop) *tmop   = tmo;
    return 0;
}
