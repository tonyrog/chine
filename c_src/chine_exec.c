//
// chine executive 
//
// usage:
//      chine_exec packed.script (or $0 from script)
//      chine_exec prog.x        run chine code from file
//      chine_exec               run chine code from stdin
//
#include <stdlib.h>
#include <unistd.h>
#include <fcntl.h>
#include <errno.h>

#include "chine.h"

#ifdef DEBUG
#define DEBUGF(...) if (debug) printf(__VA_ARGS__)
#else
#define DEBUGF(...)
#endif

#ifdef TRACE
extern int printf(const char* fomat, ...);
#define TRACEF(...) if (trace) printf(__VA_ARGS__)
#else
#define TRACEF(...)
#endif

#define HERE "50F645CD7C7209972B48C3220959677A"

chine_t m;

extern int32_t chine_unix_sys(chine_t* mp,
			      int32_t sysop, int32_t* revarg,
			      int32_t* npop, int32_t* value);

int verbose = 0;
int debug = 0;
int trace = 0;
int mem_size = 0;           // default
int stack_size = 0;         // default
int return_stack_size = 0;  // default

// FIXME dynamic!? optional?
char data[65537];

// format signed integer into buf
char* format_int(int val, char* buf, int size)
{
    char* ptr = buf+size;
    int sign = 0;
    *--ptr = '\0';
    if (val == 0)
	*--ptr = '0';
    else if (val < 0) {
	val = -val;
	sign = 1;
    }
    while(val != 0) {
	*--ptr = ((val % 10) + '0');
	val /= 10;
    }
    if (sign)
	*--ptr = '-';
    return ptr;
}

int str_len(const char* str)
{
    int len = 0;
    while(*str++) len++;
    return len;
}

void mem_zero(void* ptr, size_t n)
{
    unsigned char* uptr = (unsigned char*) ptr;
    while(n--) *uptr++ = 0;
}

int str_cmp(const char *s1, const char *s2, register size_t n)
{
    register unsigned char u1, u2;
    while (n-- > 0) {
	u1 = (unsigned char) *s1++;
	u2 = (unsigned char) *s2++;
	if (u1 != u2)
	    return u1 - u2;
	if (u1 == '\0')
	    return 0;
    }
    return 0;
}

void error(const char* str1, const char* str2)
{
    const char* str0 = "chine_exec: ";
    write(2, str0, str_len(str0));
    write(2, str1, str_len(str1));
    write(2, str2, str_len(str2));
    write(2, "\n", 1);
    exit(1);
}

int to_hex(char x)
{
    int c = x-'0';
    if (c < 0) return -1;
    if (c > 9) c -= 7;
    if (c > 15) return -1;
    return c;
}

// Standard CRC-32
#define CRC32_POLY 0xEDB88320

typedef struct {
    uint32_t crc;
} crc_32_ctx_t;

void crc32_init(crc_32_ctx_t* p)
{
    p->crc = (uint32_t) -1;
}

uint32_t crc32_final(crc_32_ctx_t* p)
{
    return ~p->crc;
}

void crc32_update(crc_32_ctx_t* p, uint8_t* data, size_t len)
{
    uint32_t crc = p->crc;

   while (len--) {
       uint32_t byte = *data++;
       int j;
       crc = crc ^ byte;
       for (j = 7; j >= 0; j--) {    // Do eight times.
	   uint32_t mask = -(crc & 1);
	   crc = (crc >> 1) ^ (CRC32_POLY & mask);
       }
   }
   p->crc = crc;
}

uint32_t crc32(uint8_t* data, size_t len)
{
    crc_32_ctx_t param;

    crc32_init(&param);
    crc32_update(&param, data, len);
    return crc32_final(&param);
}

// return pointer to symbol table
uint8_t* file_header(uint8_t* ptr, uint8_t** symb_end)
{
    uint32_t length;
    uint32_t symblen;
    uint32_t crc, crc2;
    uint32_t zero = 0;
    crc_32_ctx_t ctx;
    uint8_t* ptr0 = ptr;
    
    if (str_cmp((const char*)ptr, "CHIN", 4) != 0)
	return NULL;
    ptr += 4;
    if ((ptr[0] != FILE_VERSION_MAJOR) || (ptr[1] != FILE_VERSION_MINOR) )
	return NULL;
    ptr += 4;
    crc = chine_read_mem_u32(ptr);
    ptr += 4;
    length = chine_read_mem_u32(ptr);
    ptr += 4;

    // printf("crc=%u, length=%d\n", crc, length);

    crc32_init(&ctx);
    crc32_update(&ctx, (uint8_t*)ptr0, 8);              // magic + version
    crc32_update(&ctx, (uint8_t*)&zero, sizeof(zero));  // crc=0
    crc32_update(&ctx, ptr-4, length + 4);
    crc2 = crc32_final(&ctx);
    if (crc2 != crc) {
	char buf[10];
	error("bad_crc, computed = %u\n", format_int(crc2, buf, sizeof(buf)));
	return NULL;
    }

    if (str_cmp((const char*) ptr, "SYMB", 4) != 0)
	return NULL;
    ptr += 4;
    symblen = chine_read_mem_u32(ptr);
    ptr += 4;
    *symb_end = (ptr + symblen);
    return ptr;
}

// return pointer code section
uint8_t* code_section(uint8_t* symb_end, uint8_t** code_end)
{
    uint8_t* ptr = symb_end;
    uint32_t code_len;

    if (str_cmp((const char *)ptr, "CODE", 4) != 0)
	return NULL;
    ptr += 4;
    code_len = chine_read_mem_u32(ptr);
    ptr += 4;
    *code_end = ptr + code_len;
    return ptr;
}

int lookup(uint8_t* symb_start, uint8_t* symb_end, char* symbol)
{
    uint8_t* ptr = symb_start;
    int symlen = str_len(symbol);

    DEBUGF("lookup: [%d] %s\n", symlen, symbol);

    while(ptr < symb_end) {
	uint8_t sn = ptr[0];
	uint8_t vn;
	uint8_t* sptr = ptr+1;
	uint8_t* vptr;
	ptr = sptr + sn;
	vn = ptr[0];
	vptr = ptr+1;
	DEBUGF("search: vn=%d [%d] %.*s\n", vn, sn, sn, sptr);
	if ((symlen == sn) && (str_cmp((const char*)symbol,
				       (const char*) sptr, sn) == 0)) {
	    int offset;
	    switch(vn) {
	    case 1: offset = chine_read_mem_i8(vptr); break;
	    case 2: offset = chine_read_mem_i16(vptr); break;
	    case 4: offset = chine_read_mem_i32(vptr); break;
	    default: return -1;
	    }
	    TRACEF("symbol %.*s offset %d\n", sn, sptr, offset);
	    DEBUGF("found offst = %d\n", offset);
	    return offset;
	}
	ptr = vptr+vn;
    }
    return -1;
}

//
// execute pack files
// size of the binary program is located as the last line in ascii
// the size in hex is twice that size not counting new lines
// FIXME:
// restore the patched script?
//

// scan 8-digit hex number
// last line in file looks like
// ": xxxxxxxx\n"
// this is an offset to start of hex encoded program area
// from end of file (including the hex offset and final newline)

off_t scan_program_offset(int fd)
{
    char buf[11];
    off_t offset = 0;
    int i;
    
    if (lseek(fd, -11, SEEK_END) < 0)
	return -1;
    if (read(fd, buf, 11) != 11)
	return -1;
    if ((buf[0] != ':') || (buf[1] != ' '))
	return -1;
    for (i = 0; i < 8; i++) {
	int c;
	if ((c = to_hex(buf[i+2])) < 0) return -1;
	offset = (offset << 4) + c;
    }
    return offset;
}

int load_program(int fd, char* data, size_t max_data)
{
    char* ptr = data;
    char* data_end = data + max_data;
    char buf[78];
    int len;
    int i;
next:
    if (data >= data_end) return -1;
    if ((len = read(fd, buf, 77)) < 0) return -1;
    if ((str_cmp(buf, HERE, 32)) == 0) return ptr - data;
    buf[len] = 0;
    DEBUGF("loaded line %s\n", buf);
    i = 0;
    while(i < len) {
	int c1, c0;
	if ((c1 = to_hex(buf[i])) < 0) {
	    if (buf[i] == '\n') goto next;
	}
	if ((c0 = to_hex(buf[i+1])) < 0) return -1;
	*ptr++ = (c1 << 4)+c0;
	i += 2;
    }
    return -1;
}

int main(int argc, char** argv)
{
    uint8_t imask[NUM_IBYTES];   // input mask
    timeout_t tmo;
    int fd = 0;
    const char* input_file = NULL;
    uint8_t* symb_start;
    uint8_t* symb_end;
    uint8_t* code_start;
    uint8_t* code_end;
    size_t code_len;
    off_t offset;
    int i = 1;

    while((i < argc) && (argv[i][0] == '-')) {
	switch(argv[i][1]) {
	case 'v': verbose = 1; i++; break;
	case 't': trace = 1; i++; break;
	case 'd': debug = 1; i++; break;
	case 'm': mem_size = atoi(argv[i+1]); i += 2; break;
	case 's': stack_size = atoi(argv[i+1]); i += 2; break;
	case 'r': return_stack_size = atoi(argv[i+1]); i += 2; break;
	default:
	    error("chinex: unknown option ", argv[i]);
	    exit(1);
	}
    }

    if (i == argc) {
	if (isatty(0))  // otherwise (for now) it is a piped chine program
	    input_file = argv[0];  // ???
    }
    else if (i < argc) {
	if (str_cmp((const char*)argv[i], "--", 3) == 0)
	    input_file = NULL;
	else
	    input_file = argv[i];
    }
    
    if (input_file != NULL) {
	if ((fd = open(input_file, O_RDONLY)) < 0)
	    error("unable to open file ", input_file);
	if (read(fd, data, 4) != 4)
	    error("unable to read file ", input_file);
	if (str_cmp((const char*)data, "CHIN", 4) != 0) {
	    if (lseek(fd, 0, SEEK_SET) < 0)
		error("unable to open seek file ", input_file);
	    if ((offset = scan_program_offset(fd)) < 0)
		error("unable to read offset ", input_file);
	    
	    DEBUGF("offset = %ld\n", offset);

	    if (lseek(fd, -offset, SEEK_END) < 0)
		error("unable to open seek file ", input_file);
	    if (load_program(fd, data, sizeof(data)) < 0)
		error("unable to open load program ", input_file);
	    close(fd);
	}
	else {
	    if (read(fd, data+4, sizeof(data)-4) < 0)
		error("unable to open load program ", input_file);
	    close(fd);
	}
    }
    else {
	if (read(0, data, sizeof(data)) < 0)
	    error("unable to open load program ", "*stdin*");
    }

    if ((symb_start = file_header((uint8_t*)data, &symb_end)) == NULL)
	error("file format error", "");
    if ((code_start = code_section(symb_end, &code_end)) == NULL)
	error("code section not found", "");

    code_len = code_end - code_start;
    chine_init(&m, (uint8_t*)code_start, code_len, chine_unix_sys);

    if ((offset = lookup(symb_start, symb_end, "init")) >= 0) {
	chine_set_ip(&m, offset);
	if (chine_run(&m) < 0) goto fail;
    }
    
    if ((offset = lookup(symb_start, symb_end, "run")) < 0)
	error("no code found in ", argv[1]);

    chine_set_ip(&m, offset);

again:
    if (chine_is_top_level(&m))
	chine_set_ip(&m, offset);

    if (chine_run(&m) < 0) goto final;

    tmo = 0xffffffff;
    mem_zero(&imask, sizeof(imask));

    if (chine_next(&m, &tmo, imask)) { // wait for input or timer
	if (tmo < 0xffffffff) {
	    usleep(tmo*1000);
	    goto again;
	}
    }
    goto again;

final:
    if ((offset = lookup(symb_start, symb_end, "final")) >= 0) {
	int prev_fail = m.cErr;
	chine_set_ip(&m, offset);
	if (chine_run(&m) < 0) {
	    m.cErr = prev_fail;
	    goto fail;
	}
    }
fail:
    if (m.cErr != FAIL_TERMINATE) {
	char* errstr = "unknown";
	switch(m.cErr) {
	case FAIL_INVALID_ARGUMENT:
	    errstr = "invalid argument"; break;
	case FAIL_INVALID_OPCODE:
	    errstr = "invalid opcode"; break;
	case FAIL_STACK_OVERFLOW:
	    errstr = "stack overflow"; break;
	case FAIL_STACK_UNDERFLOW:
	    errstr = "stack underflow"; break;
	case FAIL_INVALID_MEMORY_ADDRESS:
	    errstr = "invalid address"; break;
	case FAIL_DIV_ZERO:
	    errstr = "division by zero"; break;
	case FAIL_TIMER_OVERFLOW:
	    errstr = "timer overflow"; break;
	default: {
	    char buf[8];
	    char* ptr = format_int((int)m.cErr, buf, sizeof(buf));
	    error("execution error: code=", ptr);
	}
	}
	error("execution error: ", errstr);
    }
    exit(0);    
}
