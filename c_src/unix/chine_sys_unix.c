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

#ifdef TRACE
#define TRACEF(...) printf(__VA_ARGS__)
#else
#define TRACEF(...)
#endif

#if defined(WIRINGPI)
#include <wiringPI.h>
#endif

#define INDEX_TYPE     0x2701  // UNSIGNED8  - type code
#define INDEX_DELAY    0x2710  // UNSIGNED32 - time
#define INDEX_WAIT     0x2712  // UNSIGNED32 - time
#define INDEX_RAMPUP   0x2715  // UNSIGNED32 - time
#define INDEX_RAMPDOWN 0x2716  // UNSIGNED32 - time

uint8_t  param_type[MAX_TIMERS];
uint32_t param_delay[MAX_TIMERS] = 
{ 500, 1000, 2000, 3000, 4000, 5000, 6000, 7000 };
uint32_t param_wait[MAX_TIMERS];
uint32_t param_rampup[MAX_TIMERS];
uint32_t param_rampdown[MAX_TIMERS];

struct _input_t {
    uint8_t  digital;
    uint16_t analog;
    int16_t  encoder;
} input[32];

struct _output_t {
    uint8_t  digital;
    uint16_t analog;
    int16_t  encoder;
} output[32];

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

static int fetch(uint16_t index, uint8_t si, int32_t* value)
{
    if (si >= MAX_TIMERS) return FAIL_INVALID_ARGUMENT;
    switch(index) {
    case INDEX_TYPE: *value = param_type[si]; break;
    case INDEX_DELAY: *value = param_delay[si]; break;
    case INDEX_WAIT:  *value = param_wait[si]; break;
    case INDEX_RAMPUP: *value = param_rampup[si]; break;
    case INDEX_RAMPDOWN: *value = param_rampdown[si]; break;
    default: return FAIL_INVALID_ARGUMENT;
    }
    return 0;
}

static int store(uint16_t index, uint8_t si,int32_t value)
{
    if (si >= MAX_TIMERS) return FAIL_INVALID_ARGUMENT;
    switch(index) {
    case INDEX_TYPE:  param_type[si]=value; break;
    case INDEX_DELAY: param_delay[si]=value; break;
    case INDEX_WAIT:  param_wait[si]=value; break;
    case INDEX_RAMPUP: param_rampup[si]=value; break;
    case INDEX_RAMPDOWN: param_rampdown[si]=value; break;
    default: return FAIL_INVALID_ARGUMENT;
    }
    return 0;
}

//
// return: <  0   FAIL_xyz
//         == 0   return no value
//         >  0   return value in *value
// number of arguments to pop is return in npop 
//
int chine_unix_sys(chine_t* mp,
		   cell_t sysop, cell_t* revarg,
		   cell_t* npop, cell_t* value)
{
    switch(sysop) {
    case SYS_INIT: {
	TRACEF("init\n");
	gettimeofday(&boot_time, 0);
#if defined(WIRINGPI)
	// maybe the simple solution (bit slow)
	// it uses native pin numbers!
	wiringPiSetupSys();
#endif
	return 0;
    }
    case SYS_PARAM_FETCH: {
	TRACEF("param@(%04x,%d)",revarg[1], revarg[0]);
	*npop = 2;
	return fetch(revarg[1], revarg[0], value);
    }
    case SYS_PARAM_STORE: {
	TRACEF("param!(%04x,%d,%d)",revarg[2], revarg[1], revarg[0]);
	*npop = 3;
	return store(revarg[2], revarg[1], revarg[0]);
    }
    case SYS_TIMER_INIT: {
	cell_t i = revarg[0];
	TRACEF("timer-init(%d)", i);
	*npop = 1;
	if ((i < 0) || (i>=MAX_TIMERS)) return FAIL_TIMER_OVERFLOW;
	CLRBIT(mp->tbits, i);
	mp->timer[i] = 0;
	return 0;
    }
    case SYS_TIMER_STOP: {
	cell_t i = revarg[0];
	TRACEF("timer-stop(%d)", i);
	*npop = 1;
	if ((i < 0) || (i>=MAX_TIMERS)) return FAIL_TIMER_OVERFLOW;
	CLRBIT(mp->tbits, i);
	return 0;
    }
    case SYS_TIMER_START: {
	cell_t i = revarg[0];
	TRACEF("timer-start(%d)", i);
	*npop = 1;
	if ((i < 0) || (i>=MAX_TIMERS)) return FAIL_TIMER_OVERFLOW;
	SETBIT(mp->tbits, i);
	mp->timer[i] = chine_millis()+param_delay[i];
	return 0;
    }
    case SYS_TIMER_TIMEOUT: {
	cell_t i = revarg[0];
	TRACEF("timer-timeout(%d)", i);
	*npop = 1;
	if ((i < 0) || (i>=MAX_TIMERS)) return FAIL_TIMER_OVERFLOW;
	if (!TSTBIT(mp->tbits,i))
	    *value = 0;
	else
	    *value = (chine_millis() >= mp->timer[i]);
	return 1;
    }
    case SYS_TIMER_RUNNING:  {
	cell_t i = revarg[0];
	TRACEF("timer-running(%d)", i);
	*npop = 1;
	if ((i < 0) || (i>=MAX_TIMERS)) return FAIL_TIMER_OVERFLOW;
	*value = TSTBIT(mp->tbits, i) != 0;
	return 1;
    }
    case SYS_NOW: {
	TRACEF("now");
	*npop = 0;
	*value = chine_millis();
	return 1;
    }
    case SYS_INPUT_FETCH: {  // ( i k -- v )
	cell_t k = revarg[0];
	cell_t i = revarg[1];
	TRACEF("input@(%d.%d)", k, i);
	*npop = 2;
	if ((i < 0) || (i >= 32)) return FAIL_INVALID_ARGUMENT;
	switch(k) {
	case INPUT_BOOLEAN: *value = input[i].digital;
	case INPUT_ANALOG:  *value = input[i].analog;
	case INPUT_ENCODER: *value = input[i].encoder;
	default: return FAIL_INVALID_ARGUMENT;
	}
	return 1;
    }
    case SYS_OUTPUT_STORE: {  // ( i k n -- )
	cell_t n = revarg[0];
	cell_t k = revarg[1];
	cell_t i = revarg[2];
	TRACEF("output!([%d].%d,%d)", k, i, n);
	*npop = 3;
	if ((i < 0) || (i >= 32)) return FAIL_INVALID_ARGUMENT;
	switch(k) {
	case INPUT_BOOLEAN: output[i].digital = n;
	case INPUT_ANALOG:  output[i].analog = n;
	case INPUT_ENCODER: output[i].encoder = n;
	default: return FAIL_INVALID_ARGUMENT;
	}
	return 1;
    }
    case SYS_DESELECT_ALL: {  // ( -- )
	TRACEF("deselect-all");
	memset(mp->imask, 0, sizeof(mp->imask));
	memset(mp->tmask, 0, sizeof(mp->tmask));
	return 0;
    }
    case SYS_SELECT_INPUT: { // ( i -- )
	cell_t i = revarg[0];
	TRACEF("select-input(%d)", i);
	*npop = 1;
	if ((i < 0) || (i >= MAX_INPUT)) return FAIL_INVALID_ARGUMENT;
	SETBIT(mp->imask, i);
	return 0;
    }
    case SYS_DESELECT_INPUT: { // ( i -- )
	cell_t i = revarg[0];
	TRACEF("deselect-input(%d)", i);
	*npop = 1;
	if ((i < 0) || (i >= MAX_INPUT)) return FAIL_INVALID_ARGUMENT;
	CLRBIT(mp->imask, i);
	return 0;
    }
    case SYS_SELECT_TIMER: { // ( i -- )
	cell_t i = revarg[0];
	TRACEF("select-timer(%d)", i);
	*npop = 1;
	if ((i < 0) || (i>=MAX_TIMERS)) return FAIL_TIMER_OVERFLOW;
	SETBIT(mp->tmask, i);
	return 0;
    }
    case SYS_DESELECT_TIMER: { // ( i -- )
	cell_t i = revarg[0];
	TRACEF("deselect-timer(%d)", i);
	*npop = 1;
	if ((i < 0) || (i>=MAX_TIMERS)) return FAIL_TIMER_OVERFLOW;
	CLRBIT(mp->tmask, i);
	return 0;
    }
    case SYS_UART_SEND: {
	uint8_t c = revarg[0];
	TRACEF("uart_send(%d)", c);
	*npop = 1;
	write(1, &c, 1);
	return 0;
    }
    case SYS_UART_AVAIL: {
	struct timeval poll = { 0, 0 };
	fd_set iset;
	FD_ZERO(&iset);
	FD_SET(0, &iset);
	*npop = 0;
	TRACEF("uart_avail()");
	if (select(1, &iset, NULL, NULL, &poll) == 1)
	    *value = 1;
	else
	    *value = 0;
	return 1;
    }
    case SYS_UART_RECV: {
	uint8_t c;
	TRACEF("uart_recv()");
	if (read(0, &c, 1) == 1)
	    *value = c;
	else
	    *value = -1;
	return 1;
    }
    case SYS_GPIO_INPUT: {
	int i = revarg[0];
	TRACEF("gpio_input(%d)",i);
	*npop = 1;
#if defined(WIRINGPI)
	pinMode(i, INPUT);
#endif
	return 0;
    }
    case SYS_GPIO_OUTPUT: {
	int i = revarg[0];
	TRACEF("gpio_output(%d)",i);
	*npop = 1;
#if defined(WIRINGPI)
	pinMode(i, OUTPUT);
#endif
	return 0;
    }
    case SYS_GPIO_SET: {
	int i = revarg[0];
	TRACEF("gpio_set(%d)",i);
	*npop = 1;
#if defined(WIRINGPI)
	digitalWrite(i, HIGH);
#endif
	return 0;
    }
    case SYS_GPIO_CLR: {
	int i = revarg[0];
	TRACEF("gpio_clr(%d)",i);
	*npop = 1;
#if defined(WIRINGPI)
	digitalWrite(i, LOW);
#endif
	return 0;
    }
    case SYS_GPIO_GET: {
	int i = revarg[0];
	TRACEF("gpio_get(%d)",i);
	*npop = 1;
#if defined(WIRINGPI)
	*value = digitalRead(i);
#else
	*value = 0;
#endif
	return 1;
    }
    case SYS_ANALOG_SEND: {
	int i   = revarg[1];
	int val = revarg[0];
	*npop = 2;
	TRACEF("analog_send(%d,%u)",i,val);
#if defined(WIRINGPI)
	analogWrite(i,val>>8);
#endif
	return 0;
    }
    case SYS_ANALOG_RECV: {
	int i = revarg[0];
	*npop = 1;
	TRACEF("analog_recv(%d)",i);
#if defined(WIRINGPI)
	*value = analogRead(i)<<6;
#else
	*value = 0;
#endif
	return 1;
    }
    case SYS_CAN_SEND: {
	cell_t n     = revarg[0];	
	int    index = revarg[1];	
	int    si    = revarg[2];
	TRACEF("can_send(%04x,%d,%d)",index,si,n);
	*npop = 3;
	return 0;
    }
    default:
	TRACEF("????");
	*npop = 0;
	return FAIL_INVALID_ARGUMENT;
    }
}
