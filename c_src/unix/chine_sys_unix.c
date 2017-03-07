//
//  Test implmentation of sys callback for unix
// 

#include <stdio.h>
#include <stdint.h>
#include <assert.h>
#include <unistd.h>
#include <memory.h>
#include <sys/time.h>

#include "../../include/chine.h"

#define INDEX_TYPE     0x2701  // UNSIGNED8  - type code
#define INDEX_DELAY    0x2710  // UNSIGNED32 - time
#define INDEX_WAIT     0x2712  // UNSIGNED32 - time
#define INDEX_RAMPUP   0x2715  // UNSIGNED32 - time
#define INDEX_RAMPDOWN 0x2716  // UNSIGNED32 - time

uint8_t  param_type[MAX_TIMERS];
uint32_t param_delay[MAX_TIMERS] = 
{ 0, 1000, 2000, 3000, 4000, 5000, 6000, 7000 };
uint32_t param_wait[MAX_TIMERS];
uint32_t param_rampup[MAX_TIMERS];
uint32_t param_rampdown[MAX_TIMERS];

struct {
    uint8_t  digital;
    uint16_t analog;
    int16_t  encoder;
} input[32];

struct timeval boot_time;

// time since program started in ms
uint32_t chine_micros(void)
{
    struct timeval now;
    struct timeval t;
    gettimeofday(&now, 0);
    timersub(&now, &boot_time, &t);
    return t.tv_sec*1000000 + t.tv_usec;
}

uint32_t chine_millis(void)
{
    struct timeval now;
    struct timeval t;
    gettimeofday(&now, 0);
    timersub(&now, &boot_time, &t);
    return t.tv_sec*1000 + t.tv_usec/1000;
}

static int32_t fetch(uint16_t index, uint8_t si, int32_t* value)
{
    if (si >= MAX_TIMERS) return FAIL_BAD_ARG;
    switch(index) {
    case INDEX_TYPE: *value = param_type[si]; break;
    case INDEX_DELAY: *value = param_delay[si]; break;
    case INDEX_WAIT:  *value = param_wait[si]; break;
    case INDEX_RAMPUP: *value = param_rampup[si]; break;
    case INDEX_RAMPDOWN: *value = param_rampdown[si]; break;
    default: return FAIL_BAD_ARG;
    }
    return 0;
}

static int32_t store(uint16_t index, uint8_t si,int32_t value)
{
    if (si >= MAX_TIMERS) return FAIL_BAD_ARG;
    switch(index) {
    case INDEX_TYPE:  param_type[si]=value; break;
    case INDEX_DELAY: param_delay[si]=value; break;
    case INDEX_WAIT:  param_wait[si]=value; break;
    case INDEX_RAMPUP: param_rampup[si]=value; break;
    case INDEX_RAMPDOWN: param_rampdown[si]=value; break;
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
	gettimeofday(&boot_time, 0);
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
	mp->timer[i] = chine_millis()+param_delay[i];
	return 0;
    }

    case SYS_TIMER_TIMEOUT: {
	int32_t i = revarg[0];
	*npop = 1;
	if ((i < 0) || (i>=MAX_TIMERS)) return FAIL_TIMER_OVERFLOW;
	if (!TSTBIT(mp->tbits,i))
	    *value = 0;
	else
	    *value = (chine_millis() >= mp->timer[i]);
	return 1;
    }

    case SYS_TIMER_RUNNING:  {
	int32_t i = revarg[0];
	*npop = 1;
	if ((i < 0) || (i>=MAX_TIMERS)) return FAIL_TIMER_OVERFLOW;
	*value = TSTBIT(mp->tbits, i) != 0;
	return 1;
    }

    case SYS_NOW: {
	*npop = 0;
	*value = chine_millis();
	return 1;
    }

    case SYS_INPUT_FETCH: {  // ( i k -- v )
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

    case SYS_DESELECT_ALL: {  // ( -- )
	memset(mp->imask, 0, sizeof(mp->imask));
	memset(mp->tmask, 0, sizeof(mp->tmask));
	return 0;
    }

    case SYS_SELECT_INPUT: { // ( i -- )
	int32_t i = revarg[0];
	*npop = 1;
	if ((i < 0) || (i >= MAX_INPUT)) return FAIL_BAD_ARG;
	SETBIT(mp->imask, i);
	return 0;
    }

    case SYS_SELECT_TIMER: { // ( i -- )
	int32_t i = revarg[0];
	*npop = 1;
	if ((i < 0) || (i>=MAX_TIMERS)) return FAIL_TIMER_OVERFLOW;
	SETBIT(mp->tmask, i);
	return 0;
    }

    case SYS_EMIT: {
	uint8_t c = revarg[0];
	*npop = 1;
	write(1, &c, 1);
	return 0;
    }

    case SYS_QKEY: {
	struct timeval poll = { 0, 0 };
	fd_set iset;
	FD_ZERO(&iset);
	FD_SET(0, &iset);
	*npop = 0;
	if (select(1, &iset, NULL, NULL, &poll) == 1)
	    *value = 1;
	else
	    *value = 0;
	return 1;
    }

    case SYS_KEY: {
	uint8_t c;
	if (read(0, &c, 1) == 1)
	    *value = c;
	else
	    *value = -1;
	return 1;
    }
    default:
	*npop = 0;
	return FAIL_BAD_ARG;
    }
}
