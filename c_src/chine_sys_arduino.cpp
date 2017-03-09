//
//  Test implmentation of sys callback for unix
// 

#include "Arduino.h"
#include "HardwareSerial.h"

#include "../include/chine.h"

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

// time since program started in ms
uint32_t chine_micros(void)
{
    return micros();
}

uint32_t chine_millis(void)
{
    return millis();
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

static int store(uint16_t index, uint8_t si, int32_t value)
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
int chine_arduino_sys(chine_t* mp,
		      cell_t sysop, cell_t* revarg,
		      cell_t* npop, cell_t* value)
{
    switch(sysop) {
    case SYS_INIT: {
	Serial.begin(9600);
	return 0;
    }
    case SYS_PARAM_FETCH: {
	*npop = 2;
	return fetch(revarg[1], revarg[0], value);
    }
    case SYS_PARAM_STORE: {
	*npop = 3;
	return store(revarg[2], revarg[1], revarg[0]);
    }
    case SYS_TIMER_STOP: // same as init
    case SYS_TIMER_INIT: {
	cell_t i = revarg[0];
	*npop = 1;
	if ((i < 0) || (i>=MAX_TIMERS)) return FAIL_TIMER_OVERFLOW;
	CLRBIT(mp->tbits, i);
	mp->timer[i] = 0;
	return 0;
    }
    case SYS_TIMER_START: {
	cell_t i = revarg[0];
	*npop = 1;
	if ((i < 0) || (i>=MAX_TIMERS)) return FAIL_TIMER_OVERFLOW;
	SETBIT(mp->tbits, i);
	mp->timer[i] = chine_millis()+param_delay[i];
	return 0;
    }
    case SYS_TIMER_TIMEOUT: {
	cell_t i = revarg[0];
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
	cell_t k = revarg[0];
	cell_t i = revarg[1];
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
	memset(mp->imask, 0, sizeof(mp->imask));
	memset(mp->tmask, 0, sizeof(mp->tmask));
	return 0;
    }
    case SYS_SELECT_INPUT: { // ( i -- )
	cell_t i = revarg[0];
	*npop = 1;
	if ((i < 0) || (i >= MAX_INPUT)) return FAIL_INVALID_ARGUMENT;
	SETBIT(mp->imask, i);
	return 0;
    }
    case SYS_DESELECT_INPUT: { // ( i -- )
	cell_t i = revarg[0];
	*npop = 1;
	if ((i < 0) || (i >= MAX_INPUT)) return FAIL_INVALID_ARGUMENT;
	CLRBIT(mp->imask, i);
	return 0;
    }
    case SYS_SELECT_TIMER: { // ( i -- )
	cell_t i = revarg[0];
	*npop = 1;
	if ((i < 0) || (i>=MAX_TIMERS)) return FAIL_TIMER_OVERFLOW;
	SETBIT(mp->tmask, i);
	return 0;
    }
    case SYS_DESELECT_TIMER: { // ( i -- )
	cell_t i = revarg[0];
	*npop = 1;
	if ((i < 0) || (i>=MAX_TIMERS)) return FAIL_TIMER_OVERFLOW;
	CLRBIT(mp->tmask, i);
	return 0;
    }
    case SYS_UART_SEND: {
	cell_t c = revarg[0];
	*npop = 1;
	Serial.write(c);
	return 0;
    }
    case SYS_UART_AVAIL: {
	*npop = 0;
	*value = Serial.available();
	return 1;
    }
    case SYS_UART_RECV: {
	*npop = 0;
	*value = Serial.read();
	return 1;
    }
    case SYS_GPIO_INPUT: {
	int i = revarg[0];
	*npop = 1;
	pinMode(i, INPUT);
	return 0;
    }
    case SYS_GPIO_OUTPUT: {
	int i = revarg[0];
	*npop = 1;
	pinMode(i, OUTPUT);
	return 0;
    }
    case SYS_GPIO_SET: {
	int i = revarg[0];
	*npop = 1;
	digitalWrite(i, HIGH);
	return 0;
    }
    case SYS_GPIO_CLR: {
	int i = revarg[0];
	*npop = 1;
	digitalWrite(i, LOW);
	return 0;
    }
    case SYS_GPIO_GET: {
	int i = revarg[0];
	*npop = 1;
	*value = digitalRead(i);
	return 1;
    }
    case SYS_ANALOG_SEND: {
	int i   = revarg[1];
	int val = revarg[0];
	*npop = 2;
	analogWrite(i,val>>8);
	return 0;
    }
    case SYS_ANALOG_RECV: {
	int i = revarg[0];
	*npop = 1;
	*value = analogRead(i)<<6;
	return 1;
    }
    default:
	*npop = 0;
	return FAIL_INVALID_ARGUMENT;
    }
}
