//
//  Test implmentation of sys callback for unix
// 

#include <stdio.h>
#include <stdint.h>
#include <assert.h>
#include <unistd.h>
#include <memory.h>
#include <errno.h>
#include <unistd.h>
#include <fcntl.h>
#include <sys/time.h>

#include "../../include/chine.h"

#ifdef TRACE
#define TRACEF(...) printf(__VA_ARGS__)
#else
#define TRACEF(...)
#endif

#if defined(WIRINGPI)
#include <wiringPI.h>
#include <wiringSerial.h>
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

#define PTR(x) ((void*)((intptr_t)(x)))

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


void* get_const_array(chine_t* mp, cell_t offs)
{
    uint8_t* aptr = mp->prog + offs;  // get array address
    if ((*aptr & 7) != ARRAY) return NULL;
    return (void*)(aptr+get_array_hlen(*aptr)+1);
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

    case SYS_TERMINATE: {
	*npop = 0;
	return FAIL_TERMINATE;
    }
	
    case SYS_NOW: {
	TRACEF("now");
	*npop = 0;
	*value = chine_millis();
	return 1;
    }
	
    case SYS_EMIT: {
	uint8_t c = revarg[0];
	TRACEF("emit(%d)", c);
	*npop = 1;
	if (write(1, &c, 1) < 0)
	    return FAIL_INVALID_ARGUMENT;
	return 0;
    }
	
    case SYS_AVAIL: {
	struct timeval poll = { 0, 0 };
	fd_set iset;
	FD_ZERO(&iset);
	FD_SET(0, &iset);
	*npop = 0;
	TRACEF("avail()");
	if (select(1, &iset, NULL, NULL, &poll) == 1)
	    *value = CHINE_TRUE;
	else
	    *value = CHINE_FALSE;
	return 1;
    }
	
    case SYS_RECV: {
	uint8_t c;
	TRACEF("recv()");
	if (read(0, &c, 1) == 1)
	    *value = c;
	else
	    *value = -1;
	return 1;
    }
	
    case SYS_PARAM_FETCH: {
	TRACEF("param@(param=%04x,index=%d)", revarg[1], revarg[0]);
	*npop = 2;
	return fetch(revarg[1], revarg[0], value);
    }
	
    case SYS_PARAM_STORE: {
	TRACEF("param!(param=%04x,index=%d,value=%d)",
	       revarg[2], revarg[1], revarg[0]);
	*npop = 3;
	return store(revarg[2], revarg[1], revarg[0]);
    }
	
    case SYS_TIMER_INIT: { // ( i -- )
	cell_t i = revarg[0];
	TRACEF("timer-init(%d)", i);
	*npop = 1;
	if ((i < 0) || (i>=MAX_TIMERS)) return FAIL_TIMER_OVERFLOW;
	CLRBIT(mp->tbits, i);
	mp->timer[i] = 0;
	return 0;
    }
	
    case SYS_TIMER_STOP: {  // ( i -- )
	cell_t i = revarg[0];
	TRACEF("timer-stop(%d)", i);
	*npop = 1;
	if ((i < 0) || (i>=MAX_TIMERS)) return FAIL_TIMER_OVERFLOW;
	CLRBIT(mp->tbits, i);
	return 0;
    }
	
    case SYS_TIMER_START: {  // ( i -- )
	cell_t i = revarg[0];
	TRACEF("timer-start(%d)", i);
	*npop = 1;
	if ((i < 0) || (i>=MAX_TIMERS)) return FAIL_TIMER_OVERFLOW;
	SETBIT(mp->tbits, i);
	mp->timer[i] = chine_millis()+param_delay[i];
	return 0;
    }

    case SYS_TIMER_TIMEOUT: {  // ( i -- f ) 
	cell_t i = revarg[0];
	TRACEF("timer-timeout(%d)", i);
	*npop = 1;
	if ((i < 0) || (i>=MAX_TIMERS)) return FAIL_TIMER_OVERFLOW;
	if (!TSTBIT(mp->tbits,i))
	    *value = CHINE_FALSE;
	else
	    *value = CHINE_TEST(chine_millis() >= mp->timer[i]);
	return 1;
    }

    case SYS_TIMER_RUNNING:  {  // ( i -- f )
	cell_t i = revarg[0];
	TRACEF("timer-running(%d)", i);
	*npop = 1;
	if ((i < 0) || (i>=MAX_TIMERS)) return FAIL_TIMER_OVERFLOW;
	*value = CHINE_TEST(TSTBIT(mp->tbits, i) != 0);
	return 1;
    }

    case SYS_INPUT_FETCH: {  // ( i k -- v )
	cell_t k = revarg[0];
	cell_t i = revarg[1];
	TRACEF("input@(%d.%d)", k, i);
	*npop = 2;
	if ((i < 0) || (i >= 32)) return FAIL_INVALID_ARGUMENT;
	switch(k) {
	case INPUT_BOOLEAN: *value = input[i].digital; break;
	case INPUT_ANALOG:  *value = input[i].analog; break;
	case INPUT_ENCODER: *value = input[i].encoder; break;
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
	case INPUT_BOOLEAN: output[i].digital = n; break;
	case INPUT_ANALOG:  output[i].analog = n;  break;
	case INPUT_ENCODER: output[i].encoder = n; break;
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

    case SYS_GPIO_INPUT: {
	TRACEF("gpio_input(%d)", revarg[0]);
	*npop = 1;
#if defined(WIRINGPI)
	pinMode(revarg[0], INPUT);
#endif
	return 0;
    }
    case SYS_GPIO_OUTPUT: {
	TRACEF("gpio_output(%d)",revarg[0]);
	*npop = 1;
#if defined(WIRINGPI)
	pinMode(revarg[0], OUTPUT);
#endif
	return 0;
    }
    case SYS_GPIO_SET: {
	TRACEF("gpio_set(%d)",revarg[0]);
	*npop = 1;
#if defined(WIRINGPI)
	digitalWrite(revarg[0], HIGH);
#endif
	return 0;
    }
    case SYS_GPIO_CLR: {
	TRACEF("gpio_clr(%d)",revarg[0]);
	*npop = 1;
#if defined(WIRINGPI)
	digitalWrite(revarg[0], LOW);
#endif
	return 0;
    }
    case SYS_GPIO_GET: {
	TRACEF("gpio_get(%d)",revarg[0]);
	*npop = 1;
#if defined(WIRINGPI)
	*value = digitalRead(revarg[0]);
#else
	*value = 0;
#endif
	return 1;
    }
	
    case SYS_ANALOG_SEND: {
	*npop = 2;
	TRACEF("analog_send(%d,%u)",revarg[1],revarg[0]);
#if defined(WIRINGPI)
	analogWrite(revarg[1],revarg[0]>>8);
#endif
	return 0;
    }

    case SYS_ANALOG_RECV: {
	*npop = 1;
	TRACEF("analog_recv(%d)",revarg[0]);
#if defined(WIRINGPI)
	*value = analogRead(revarg[0])<<6;
#else
	*value = 0;
#endif
	return 1;
    }

    case SYS_UART_CONNECT: {
	char* tty = (char*)get_const_array(mp, revarg[0]);
	int baud = revarg[1];
	int mode = revarg[2];
	int fd;
	TRACEF("uart_connect(tty=%s,baud=%d,mode=%d)",tty,baud,mode);
	*npop = 2;	
#if defined(WIRINGPI)
	if ((fd = serialOpen(tty, baud)) < 0) {
	    revarg[2] = errno;
	    *value = CHINE_FALSE;
	}
	else {
	    revarg[2] = fd;
	    *value = CHINE_TRUE;
	}
#else
	(void) mode;
	(void) baud;
	if ((fd = open(tty, O_RDWR)) < 0) {
	    revarg[2] = errno;
	    *value = CHINE_FALSE;
	}
	else {
	    revarg[2] = fd;
	    *value = CHINE_TRUE;
	}
#endif
	return 1;
    }

    case SYS_UART_SEND: {
	int fd      = revarg[1];
	uint8_t val = revarg[0];
	(void)fd;
	(void)val;
	TRACEF("uart_send(fd=%d,val=%c)", fd, (char)val);
#if defined(WIRINGPI)
	serialPutchar(fd, val);
	*npop = 2;
	*value = CHINE_TRUE;
#else
	switch(write(fd, &val, 1)) {
	case 1:
	    *npop = 2;
	    *value = CHINE_TRUE;
	default:
	    *npop = 1;
	    revarg[1] = errno;
	    *value = CHINE_FALSE;
	}
#endif
	return 1;
    }

    case SYS_UART_RECV: { // ( fd -- char t | err f )
	int fd = revarg[0];
	uint8_t c;
	TRACEF("uart_recv(fd=%d)", fd);
	*npop = 0;
#if defined(WIRINGPI)
	revarg[0] = serialGetchar(fd);
	*value = CHINE_TRUE;
#else
	switch(read(fd, &c, 1)) {
	case 1:
	    revarg[0] = c;
	    *value = CHINE_TRUE;
	    break;
	default:
	    revarg[0] = errno;
	    *value = CHINE_FALSE;
	}
	return 1;
    }
#endif

    case SYS_UART_AVAIL: { // ( fd -- flag )
	int fd    = revarg[0];
	TRACEF("uart_avail(fd=%d)", fd);
	*npop = 1;
#if defined(WIRINGPI)
	*value = CHINE_TEST(serialDataAvil(fd));
#else
	(void)fd;
	*value = CHINE_TRUE;
#endif
	return 1;
    }

    case SYS_UART_DISCONNECT: {
	int fd    = revarg[0];
	*npop = 1;	
#if defined(WIRINGPI)
	serialClosee(fd);
	*value = CHINE_TRUE;
	return 1;
#else
	if (close(fd) < 0) {
	    *npop = 0;
	    revarg[0] = errno;
	    *value = CHINE_FALSE;
	}
	else {
	    *value = CHINE_TRUE;
	}
	return 1;
#endif
    }

    case SYS_CAN_CONNECT: {
	char* dev = (char*)PTR(revarg[0]);
	int bitrate = revarg[1];
	int mode = revarg[2];
	(void) dev;
	(void) bitrate;
	(void) mode;
	TRACEF("can_connect(%s,%d,%d)",dev,bitrate,mode);
	revarg[2] = 12;
	*value = CHINE_TRUE;
	*npop = 1;
	return 0;
    }

    case SYS_CAN_SEND: {
	uint32_t B   = revarg[0];
	uint32_t A   = revarg[1];
	uint32_t len = revarg[2];	
	uint32_t fid = revarg[3];
	int fd       = revarg[4];
	(void) B;
	(void) A;
	(void) len;
	(void) fid;
	(void) fd;
	TRACEF("can_send(fd=%d,fid=%x,len=%d,A=%x,B=%x)", fd, fid, len, A, B);
	*npop = 5;
	*value = CHINE_TRUE;
	return 1;
    }

    case SYS_CAN_RECV: {
	int fd     = revarg[0];
	uint32_t A = 0xFEEDBABE;
	uint32_t B = 0xC0FFE000;
	uint32_t fid = 0x123;
	(void)fd;
	TRACEF("can_recv(fd=%d)", fd);
	revarg[0] = fid;
	revarg[-1] = 7;
	revarg[-2] = B;
	revarg[-3] = A;
	*npop = -3;
	*value = CHINE_TRUE;
	return 1;	
    }

    case SYS_CAN_DISCONNECT: {
	int fd    = revarg[0];
	(void)fd;
	*npop = 1;
	*value = CHINE_TRUE;
	return 1;
    }

    case SYS_FILE_OPEN: {
	char* name = (char*)get_const_array(mp,revarg[1]);
	int flags  = revarg[0];
	int fd;
	TRACEF("file_open(%s,%d)", name, flags);
	if ((fd=open(name, flags, 0666)) < 0) {
	    *npop = 1;
	    revarg[1] = errno;
	    *value = CHINE_FALSE;
	}
	else {
	    *npop = 1;
	    revarg[1] = fd;
	    *value = CHINE_TRUE;
	}
	return 1;
    }

    case SYS_FILE_WRITE: {
	int fd       = revarg[2];
	void* buf    = get_const_array(mp, revarg[1]);
	size_t count = revarg[0];
	ssize_t n;
	TRACEF("file_write(%d,%p,%lu)", fd, buf, count);
	if ((n=write(fd,buf,count)) < 0) {
	    *npop = 2;
	    revarg[2] = errno;
	    *value = CHINE_FALSE;
	}
	else {
	    *npop = 2;
	    revarg[2] = n;
	    *value = CHINE_TRUE;
	}
	return 1;
    }
	
    case SYS_FILE_READ: {
	int fd       = revarg[2];
	void* buf    = get_const_array(mp, revarg[1]);
	size_t count = revarg[0];
	ssize_t n;
	TRACEF("file_read(%d,%p,%lu)", fd, buf, count);
	if ((n=read(fd,buf,count)) < 0) {
	    *npop = 2;
	    revarg[2] = errno;
	    *value = CHINE_FALSE;
	}
	else {
	    *npop = 2;
	    revarg[2] = n;
	    *value = CHINE_TRUE;
	}
	return 1;
    }	

    case SYS_FILE_CLOSE: {
	int fd = revarg[0];
	TRACEF("file_close(%d)", fd);
	if (close(fd) < 0) {
	    *npop = 0;
	    revarg[0] = errno;
	    *value = CHINE_FALSE;
	}
	else {
	    *npop = 1;
	    *value = CHINE_TRUE;
	}
	return 1;
    }
	
    default:
	TRACEF("????");
	*npop = 0;
	return FAIL_INVALID_ARGUMENT;
    }
}
