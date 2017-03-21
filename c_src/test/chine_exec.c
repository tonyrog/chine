#include <stdio.h>
#include <stdlib.h>
#include <memory.h>
#include <unistd.h>
#include <errno.h>

#include "../../include/chine.h"

// #define DEBUG(...) printf(__VA_ARGS__)
#define DEBUG(...)

#ifdef TRACE
#define TRACEF(...) printf(__VA_ARGS__)
#else
#define TRACEF(...)
#endif

chine_t m;

extern int32_t chine_unix_sys(chine_t* mp,
			      int32_t sysop, int32_t* revarg,
			      int32_t* npop, int32_t* value);

char data[65537];

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
    
    if (memcmp(ptr, "CHIN", 4) != 0)
	return NULL;
    ptr += 4;
    if ((ptr[0] != FILE_VERSION_MAJOR) || (ptr[1] != FILE_VERSION_MINOR) )
	return NULL;
    ptr += 4;
    crc = UINT32(ptr);
    ptr += 4;
    length = UINT32(ptr);
    ptr += 4;

    // printf("crc=%u, length=%d\n", crc, length);

    crc32_init(&ctx);
    crc32_update(&ctx, (uint8_t*)ptr0, 8);              // magic + version
    crc32_update(&ctx, (uint8_t*)&zero, sizeof(zero));  // crc=0
    crc32_update(&ctx, ptr-4, length + 4);
    crc2 = crc32_final(&ctx);
    if (crc2 != crc) {
	fprintf(stderr, "file_header: bad_crc, computed = %u\n", crc2);
	return NULL;
    }

    if (memcmp(ptr, "SYMB", 4) != 0)
	return NULL;
    ptr += 4;
    symblen = UINT32(ptr);
    ptr += 4;
    *symb_end = (ptr + symblen);
    return ptr;
}

// return pointer code section
uint8_t* code_section(uint8_t* symb_end, uint8_t** code_end)
{
    uint8_t* ptr = symb_end;
    uint32_t code_len;

    if (memcmp(ptr, "CODE", 4) != 0)
	return NULL;
    ptr += 4;
    code_len = UINT32(ptr);
    ptr += 4;
    *code_end = ptr + code_len;
    return ptr;
}

int lookup(uint8_t* symb_start, uint8_t* symb_end, char* symbol)
{
    uint8_t* ptr = symb_start;
    int symlen = strlen(symbol);

    DEBUG("lookup: [%d] %s\n", symlen, symbol);

    while(ptr < symb_end) {
	uint8_t sn = ptr[0];
	uint8_t vn;
	uint8_t* sptr = ptr+1;
	uint8_t* vptr;
	ptr = sptr + sn;
	vn = ptr[0];
	vptr = ptr+1;
	DEBUG("search: vn=%d [%d] %.*s\n", vn, sn, sn, sptr);
	if ((symlen == sn) && (memcmp(symbol, sptr, sn) == 0)) {
	    int offset;
	    switch(vn) {
	    case 1: offset = INT8(vptr); break;
	    case 2: offset = INT16(vptr); break;
	    case 4: offset = INT32(vptr); break;
	    default: return -1;
	    }
	    TRACEF("symbol %.*s offset %d\n", sn, sptr, offset);
	    DEBUG("found offst = %d\n", offset);
	    return offset;
	}
	ptr = vptr+vn;
    }
    return -1;
}

int main(int argc, char** argv)
{
    uint8_t imask[NUM_IBYTES];   // input mask
    timeout_t tmo;
    FILE* f;
    int offset;
    uint8_t* symb_start;
    uint8_t* symb_end;
    uint8_t* code_start;
    uint8_t* code_end;

    if ((f = fopen(argv[1], "rb")) == NULL) {
	fprintf(stderr, "chine_exec: unable to open file [%s] %s\n",
		argv[1], strerror(errno));
	exit(1);
    }

    if (fread(data, sizeof(char), sizeof(data), f) >= sizeof(data)) {
	fprintf(stderr, "chine_exec: file [%s] too large\n", argv[1]);
	exit(1);
    }
    fclose(f);

    if ((symb_start = file_header((uint8_t*)data, &symb_end)) == NULL) {
	fprintf(stderr, "chine_exec: file format error\n");
	exit(1);
    }
    if ((code_start = code_section(symb_end, &code_end)) == NULL) {
	fprintf(stderr, "chine_exec: code section not found\n");
	exit(1);
    }

    chine_init(&m, (uint8_t*)code_start, chine_unix_sys);

    if ((offset = lookup(symb_start, symb_end, "init")) >= 0) {
	chine_set_ip(&m, offset);
	if (chine_run(&m) < 0) {
	    fprintf(stderr, "chine_exec: execution error %d\n", m.cErr);
	    exit(1);
	}
    }
    
    if ((offset = lookup(symb_start, symb_end, "run")) < 0) {
	fprintf(stderr, "chine_exec: [%s] nothing to run\n", argv[1]);
	exit(1);
    }

    chine_set_ip(&m, offset);

again:
    if (chine_is_top_level(&m))
	chine_set_ip(&m, offset);

    if (chine_run(&m) < 0) {
	fprintf(stderr, "chine_exec: execution error %d\n", m.cErr);
	goto final;
    }
    tmo = 0xffffffff;
    memset(&imask, 0, sizeof(imask));

    if (chine_next(&m, &tmo, imask)) { // wait for input or timer
	if (tmo < 0xffffffff) {
	    usleep(tmo*1000);
	    goto again;
	}
    }
    goto again;

final:
    if ((offset = lookup(symb_start, symb_end, "final")) >= 0) {
	chine_set_ip(&m, offset);
	if (chine_run(&m) < 0) {
	    fprintf(stderr, "chine_exec: execution error %d\n", m.cErr);
	    exit(1);
	}
    }
    // printf("TOS = %d\n", m.cSP[0]);
    exit(0);
}
