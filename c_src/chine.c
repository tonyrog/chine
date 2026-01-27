// Virtual machine for state machine processing

#include <stdlib.h>
#include <stdint.h>

#include <stdio.h>

#if !defined(ARDUINO)
#include <memory.h>
#endif

#include "chine.h"

void chine_fault(cell_t vaddr)
{
    printf("chine address fault addr = %x\n", vaddr);
    exit(1);
}

// VM -> host (t.ex. för syscall-parametrar)
void vm_copy_out(chine_t* mp, void* dst, uint32_t vaddr, size_t len)
{
    uint8_t* host_dst = (uint8_t*)dst;
    
    while (len--) {
        *host_dst++ = vm_read_u8(mp, vaddr++);
    }
}

// host -> VM (t.ex. resultat från syscall)
void vm_copy_in(chine_t* mp, uint32_t vaddr, const void* src, size_t len)
{
    const uint8_t* host_src = (const uint8_t*)src;
    
    while (len--) {
        vm_write_u8(mp, vaddr++, *host_src++);
    }
}    
    

cell_t vm_read_signed(chine_t* mp, uint8_t L, vaddr_t addr)
{
    switch(L) {
    case 1: return vm_read_i8(mp, addr);
    case 2: return vm_read_i16(mp, addr);
    case 4: return vm_read_i32(mp, addr);
    default: chine_fault(addr); return 0;
    }
}

cell_t vm_read_unsigned(chine_t* mp, uint8_t L, vaddr_t addr)
{
    switch(L) {
    case 1: return vm_read_u8(mp, addr);
    case 2: return vm_read_u16(mp, addr);
    case 4: return vm_read_u32(mp, addr);
    default: chine_fault(addr); return 0;
    }
}

cell_t vm_read_element(chine_t* mp, uint8_t n, vaddr_t addr, int i)
{
    switch(n) {
    case 1: return vm_read_u8(mp, addr + i);
    case 2: return vm_read_u16(mp, addr + 2*i);
    case 4: return vm_read_u32(mp, addr + 4*i);
    default: return 0; // error! fault
    }
}

// Initialize machine
void chine_init(chine_t* mp, uint8_t* prog, size_t prog_size,
		int (*sys)(chine_t* mp,
			   cell_t sysop, cell_t* revarg,
			   cell_t* npop, cell_t* reason))
{
    int i;

    // PAGE0 = 0x00000000 (000_)
    memset(mp->page, 0x00, sizeof(mp->page));

    // page 0 is blank - not used right now

    // PAGE1 = 0x20000000 (001_)
#ifdef __AVR__
    // fixme: set phy = 0, and require prog to be in PAGE1
    mp->page[1].phy = prog;       // program area
    mp->page[1].size = prog_size; // fixme: pass to init as prog_size?
    mp->page[1].flags = PAGE_READ|PAGE_EXEC|PAGE_PROGMEM;
#else
    mp->page[1].phy = prog;       // program area
    mp->page[1].size = prog_size; // fixme: pass to init as prog_size?
    mp->page[1].flags = PAGE_READ|PAGE_EXEC;
#endif
    printf("page 1 = phy=%p, size=%ld\n", prog, prog_size);

    // PAGE2 = 0x40000000 (010_)
    // PAGE3 = 0x60000000 (011_)
    // PAGE4 = 0x80000000 (100_)
    mp->page[4].phy = (uint8_t*) mp->mem;
    mp->page[4].size = sizeof(mp->mem);
    mp->page[4].flags = PAGE_READ|PAGE_WRITE|PAGE_EXEC;

    printf("page 4 = phy=%p, size=%ld\n", mp->mem, sizeof(mp->mem));
    // PAGE5 = 0xA0000000 (101_)
    // PAGE6 = 0xC0000000 (110_)
    // PAGE7 = 0xE0000000 (111_)    
    mp->sys  = sys;
    mp->cIP = CODE_PAGE;
    mp->cSP = mp->stack+MAX_STACK-1;  // towards low address (include a dummy)
    mp->cRP = mp->stack;              // towards high address
    mp->cFP = mp->cSP;
    memset(mp->tbits, 0, sizeof(mp->tbits));
    memset(mp->tmask, 0, sizeof(mp->tmask));
    memset(mp->imask, 0, sizeof(mp->imask));
    *mp->cSP = 0xFEEDBABE;            // initial TOS value
    for (i = 0; i < U_MAX_VARS; i++)
	mp->mem[i] = 0;
    // dictionary pointer after variable area
    mp->mem[U_DP] = RAM_PAGE + U_MAX_VARS*sizeof(cell_t); 
    (*sys)(mp, SYS_INIT, NULL, NULL, NULL);
}

// Set execution pointer
void chine_set_ip(chine_t* mp, int offset)
{
    // CODE_START is a bit brutal, make better
    mp->cIP = CODE_PAGE + offset;
}

// Chine is on toplevel
int chine_is_top_level(chine_t* mp)
{
    return (mp->cRP == mp->stack);
}

// calculcuate next timeout,
// return 1 and the the update timeout value in tmop (if != NULL)
// return 0 otherwise
int chine_timeout(chine_t* mp, timeout_t* tmop)
{
    int i = 0;
    int r = 0;
    timeout_t tmo;

    if (tmop)
	tmo = *tmop;  // refine
    else
	tmo = 0xffffffff;
    
    while(i < NUM_TBYTES) {
	uint8_t tm;
	// only check timers that are selected and running
	if ((tm = (mp->tmask[i] & mp->tbits[i])) != 0) {
	    int t = 0;
	    while(tm && (t < 8)) {
		if (tm & (1 << t)) {
		    int32_t remain = mp->timer[i*8+t] - chine_millis();
		    if (remain < 0) remain = 0;
		    if ((timeout_t)remain < tmo)
			tmo = remain;
		    r = 1;
		}
		tm &= ~(1 << t);
		t++;
	    }
	}
	i++;
    }
    if (tmop) *tmop = tmo;
    return r;
}

// check if any input is selected and update imask (if != NULL)
// return 1 any input is selected
// return 0 otherwise
int chine_input_mask(chine_t* mp, uint8_t* imask)
{
    int r = 0;
    int i = 0;

    while(!r && (i < NUM_IBYTES)) {
	if (mp->imask[i]) r = 1;
	if (imask) imask[i] |= mp->imask[i];
	i++;
    }
    return r;
}

// calculcuate next timeout or input(s)
// return 1 if a chine is waiting on a timer or is waiting for input,
//          an updated timeout time is stored in tmop if not NULL.
// return 0 otherwise 
int chine_next(chine_t* mp, timeout_t* tmop, uint8_t* imask)
{ 
    int r;
    // NOTE! must calculate both! || is not correct.
    r = chine_input_mask(mp, imask);
    r |= chine_timeout(mp, tmop);
    return r;
}


// check vector of machines for input poll and timeout
// return 1 if any of the machines in mpv is waiting for input/timer
int chine_nextv(chine_t** mpv, size_t n, timeout_t* tmop, uint8_t* imask)
{
    int i, r = 0;
    for (i = 0; i < n; i++)
	r |= chine_next(mpv[i], tmop, imask);
    return r;
}

#ifdef TRACE
#include <stdio.h>
#include <unistd.h>
extern int trace;

typedef struct {
    char* name;
    int before;
    int after;
} instr_info_t;

static const instr_info_t op_info[] = {
    [DUP]  = { "dup",  1, 2 },
    [ROT]  = { "rot",  3, 3 },
    [OVER] = { "over", 2, 3 },
    [DROP] = { "drop", 1, 0 },
    [SWAP] = { "swap", 2, 2 },
    [SUB]  = { "-",    2, 1 },
    [ADD]  = { "+",    2, 1 },
    [MUL]  = { "*",    2, 1 },
    [NOP] = { "nop",     0, 0 },
    [AND] = { "and",     2, 1 },
    [OR]  = { "or",      2, 1 },
    [XOR] = { "xor",     2, 1 },
    [ZEQ] = { "0=",      1, 1 },
    [ZLT] = { "0<",      1, 1, },
    [NOT] = { "not",     1, 1 },
    [NEGATE] = { "negate",  1, 1 },
    [DIV] = { "/",       2, 1 },
    [SHIFT] = { "shift",   2, 1 },
    [STORE] = { "!",       2, 0 },
    [FETCH] = { "@",       1, 1 },
    [TOR] = { ">r",      1, 0 },
    [FROMR] = { "r>",      0, 1 },
    [RFETCH] = { "r@",      0, 1 },
    [EXIT] = { "exit",    0, 0 },
    [SYS] = { "sys",     0, 0 },
    [YIELD] = { "yield",   0, 0 },
    [ELEM] = { "[]",      2, 1 },
    [EXEC] = { "execute", 1, 0 },
    [FPFETCH] = { "fp@",     0, 1 },
    [FPSTORE] = { "fp!",     1, 0 },
    [SPFETCH] = { "sp@",     0, 1 },
    [SPSTORE] = { "sp!",     1, 0 },
    [CSTORE] = { "c!",       2, 0 },
    [CFETCH] = { "c@",       1, 1 },
    [SIZE]   = { "size",     1, 1 },
};

static const instr_info_t jop_info[] = {
    [JMPZ]    = { "jmpz",    1, 0 },
    [JMPNZ]   = { "jmpnz",   1, 0 },
    [JNEXT]   = { "next",    0, 0},
    [JMPLZ]   = { "jmplz",   1, 0 },
    [JMP]     =  { "jmp",     0, 0 },
    [CALL]    = { "call",    0, 0 },
    [LITERAL] = { "literal", 0, 1 },
    [JOP_7]   = { "jop_7",   0, 0 },
    [GET]     = { "get",     0, 1 },  // op1 = 8
    [ARRAY]   = { "array",   0, 1 },  // op1 = 9
    [ENTER]   = { "enter",  0, 0 },   // op1 = 10
    [LEAVE]   = { "leave",  0, 0 },   // op1 = 11
    [SET]     = { "set",    1, 0 },   // op1 = 12
    [JOP_13]  = { "jop_13",  0, 0 },
    [JOP_14]  = { "jop_14",  0, 0 },
    [JOP_15]  = { "jop_15",  0, 0 },    
};

static int min(int a, int b)
{
    return (a < b) ? a : b;
}

static int max(int a, int b)
{
    return (a > b) ? a : b;
}

static int effect_upd(const instr_info_t* ip, int* bp, int* ap, int depth)
{
    int depth1 = depth + (ip->after - ip->before);
    *bp = min(*bp, depth - ip->before);
    *ap = max(*ap, depth - ip->after);
    return depth1;
}

static const instr_info_t* info_ptr(uint8_t I)
{
    switch(I & OPMASK) {
    case OP0INS: return &op_info[I & OP0MASK];
    case OP1INS: return &jop_info[I & OP1MASK];
    case OP2INS: return &jop_info[I & OP2MASK];
    case OP3INS: return &op_info[I & OP3MASK];
    }
    return NULL;
}

static int effect_update(uint8_t I, int* bp, int* ap, int depth)
{
    const instr_info_t* ip = info_ptr(I);
    int d = effect_upd(ip, bp, ap, depth);
    if ((I & OPMASK) == OP3INS)
	return effect_upd(&op_info[(I>>OP3SHFT) & OP3MASK], bp, ap, d);
    return d;
}

// display part of stack that is used by the op
// BEFORE operation starts
void static trace_begin(chine_t* mp,uint8_t TI,cell_t A,cell_t* sp)
{
    int i;
    // int d;
    // int a;
    int b;
    int mi = 9, ma = 0;
    // int spos = sp - mp->stack;

    if (!trace)
	return;
    switch(TI >> OPSHFT) {
    case OP1:
    case OP2: printf("A=%d,", A);
    default: break;
    }
    if ((TI & (OPMASK|OP1MASK)) == (OP1INS|ENTER))
	b = A >> 16;
    else {
	(void) effect_update(TI, &mi, &ma, 0); // d =
	b = -mi;
    }
    // a = -mi + d;    
    
    // printf("b=%d,a=%d (", b, a);
    // printf("sp=%d (", spos);
    printf(" (");
    if (b) {
	for (i = b-1; i > 0; i--)
	    printf("%d ", sp[i]);
	printf("%d", sp[0]);
    }
    printf(" -- ");
    if (TI == YIELD)
	printf(")\n");
}

// display part of stack that is used by the op
// AFTER operation ends
void static trace_end(chine_t* mp,uint8_t TI, cell_t A, cell_t* sp)
{
    int a;
    // int b;
    int i, d;
    int mi = 9, ma = 0;
    // int spos = sp - mp->stack;    

    if (!trace)
	return;

    if ((TI & (OPMASK|OP1MASK)) == (OP1INS|LEAVE))
	a = A & 0xf; // number of return values
    else {
	d = effect_update(TI, &mi, &ma, 0);
	// b = -mi;
	a = -mi + d;
    }

    if (a) {
	for (i = a-1; i > 0; i--) 
	    printf("%d ", sp[i]);
	printf("%d", sp[0]);
    }
    printf(")\n");
}

// print instruction in binary format
static const char* trace_ins(uint8_t ins)
{
    static char insbuf[32];
    
    switch (ins >> 6) {
    case 0:
	sprintf(insbuf,"|0|-|%d| %s",(ins & OP0MASK),
		op_info[(ins & OP0MASK)].name);
	break;
    case 1:
	sprintf(insbuf,"|1|%d|%d| %s",OP1VAL(ins),(ins&OP1MASK),
		jop_info[(ins&OP1MASK)].name);
	break;
    case 2:
	sprintf(insbuf,"|2|%d|%d| %s",OP2VAL(ins),(ins&OP2MASK),
		jop_info[(ins&OP2MASK)].name);
	break;
    case 3:
	sprintf(insbuf,"|3|%d|%d| (%s,%s)",OP3MASK&(ins>>3),OP3MASK&ins,
		op_info[OP3MASK&(ins>>3)].name,
		op_info[OP3MASK&ins].name);
	break;
    default: return "?";
    }
    return insbuf;
}

#define TRACEF(...) if (trace) printf(__VA_ARGS__)

#define BEGIN { if (I!=SKIP) { SAVE(); trace_begin(mp,TI,A,cSP); RESTORE();} {
#define XEND  } SAVE(); trace_end(mp,TI,A,cSP); RESTORE(); }
#define TEND  SAVE(); trace_end(mp,TI,A,cSP); RESTORE()
#define END   } SAVE(); trace_end(mp,TI,A,cSP); RESTORE(); NEXT; }
#define END3  } if ((I&OPMASK)!=OP3INS) { SAVE(); trace_end(mp,TI,A,cSP); RESTORE(); } NEXT3; }

#else

#define TRACEF(...)

#define BEGIN
#define XEND
#define TEND
#define END   NEXT
#define END3  NEXT3

#endif

#ifdef USE_TOS_CACHE
#define TOS_DECL cell_t TOS
#define FST TOS
#define SND cSP[0]
#define THR cSP[1]
#define PUSH(val) do { *--cSP = TOS; TOS = (val); } while(0)
#define POP()     (TOS = *cSP++)
#define SAVE()    PUSH(0)
#define RESTORE() POP()
#else
#define TOS_DECL
#define FST cSP[0]
#define SND cSP[1]
#define THR cSP[2]
#define PUSH(val) do { *--cSP = (val); } while(0)
#define POP()     cSP++
#define SAVE() 
#define RESTORE()
#endif

#define SWAP_IN(mp)				\
    cIP = (mp)->cIP;				\
    cSP = (mp)->cSP;				\
    cRP = (mp)->cRP;				\
    cFP = (mp)->cFP;				\
    RESTORE()

#define SWAP_OUT(mp)				\
    SAVE();					\
    (mp)->cIP = cIP;				\
    (mp)->cSP = cSP;				\
    (mp)->cRP = cRP;				\
    (mp)->cFP = cFP

#define CASE(mnem) case mnem
#define NEXT        goto next
#define NEXT3 do {						\
	if ((I & OPMASK)==OP3INS) {				\
	    J=(I>>OP3SHFT)&OP3MASK;				\
	    I=SKIP;						\
	    goto next3;						\
	}							\
	goto next;						\
    } while(0)

static vaddr_t get_array(chine_t* mp, vaddr_t aptr)
{
    uint8_t I = vm_read_u8(mp, aptr);
    if ((I & OP1MASK) != ARRAY)
	return 0;
    return aptr;
}

static cell_t sizeof_array(chine_t* mp, vaddr_t aptr)
{
    uint8_t I = vm_read_u8(mp, aptr);
    cell_t L = (1 << OP1VAL(I));              // arg len
    cell_t A = vm_read_signed(mp, L, aptr+1); // number of bytes
    int n = vm_read_u8(mp, aptr+L+1) & 0x3;
    // printf("L=%d,A=%d,n=%d\n", L, A, n);
    return (A-L) >> n;         // L = number of elements    
}

static cell_t get_element(chine_t* mp, vaddr_t aptr, cell_t L, int i)
{
    uint8_t n;
    uint8_t T;
    
    aptr += (1+L);              // skip past op en A to element type
    T = vm_read_u8(mp, aptr++); // read element type byte
    n = T & 0x03;               // element byte size = 2^n
    i = i << n;                 // scale to multiple of element size
    if (T & 0x80) {
	return vm_read_signed(mp, 1<<n, aptr+i);
    }
    else {
	return vm_read_unsigned(mp, 1<<n, aptr+i);
    }    
}


int chine_run(chine_t* mp)
{
    TOS_DECL;      // top of stack cache (USE_TOS_CACHE)
    vaddr_t cIP;   // instruction pointer
    cell_t*  cSP;  // stack pointer
    cell_t*  cRP;  // return stack
    cell_t*  cFP;  // frame pointer
    cell_t    A;   // argument
    cell_t    L;   // argument len
    uint8_t   I;   // instruction
    int J   = 0;   // opcode
#ifdef TRACE
    uint8_t  TI;
#endif

#define fail(e) do { mp->cErr=(e); goto L_fail; } while(0)
    // check that at least N elements exist on stack

#define check_stack_size(N) do {					\
	if (cSP + ((N) - MAX_STACK) > mp->stack)			\
	    goto L_FAIL_STACK_UNDERFLOW;				\
    } while(0)
    
#define check_return_size(N) do {					\
	if (cRP-(N) < mp->stack)					\
	    goto L_FAIL_STACK_UNDERFLOW;				\
    } while(0)
    
#define stack_need(N) do {						\
	if (cRP+(N) >= cSP)						\
	    goto L_FAIL_STACK_OVERFLOW;					\
    } while(0)    

    SWAP_IN(mp);
    
next:
    TRACEF("%08x: %s ", cIP, trace_ins(vm_read_u8(mp,cIP)));
    I = vm_read_u8(mp,cIP++);
#ifdef TRACE
    TI = I;  // save I for use in BEGIN / END
#endif
    switch((I>>OPSHFT)&3) {     // extract opcode J
    case 0:
	J = (I & OP0MASK);
	break;
    case 1:
	J = (I & OP1MASK);
	L = (1 << OP1VAL(I));
	A = vm_read_signed(mp, L, cIP);
	cIP = cIP + L; // advance beyond argument
	goto jump;
    case 2:
	J = (I & OP2MASK);
	L = 0;
	A = OP2VAL(I);
	goto jump;
    case 3:
	J = (I & OP3MASK);
	break;
    }
next3:
    // op0 & op3
    switch(J) {
    default: goto L_FAIL_INVALID_OPCODE;
    CASE(DUP): {
	    BEGIN;
	    A = FST;
	    PUSH(A);
	    END3;
	}

    CASE(ROT): {
	    BEGIN;
	    A = THR;
	    THR = SND;
	    SND = FST;
	    FST = A;
	    END3;
	}

    CASE(SWAP): {
	    BEGIN;
	    A = SND;
	    SND = FST;
	    FST = A;
	    END3;
	}

    CASE(OVER): {
	    BEGIN;
	    A = SND;
	    PUSH(A);
	    END3;
	}
	
    CASE(SUB): {  // ( n1 n2 -- diff )  diff = n1 - n2
	    BEGIN;
	    A = FST;
	    POP();
	    FST -= A;
	    END3;
	}
	
    CASE(DROP): {
	    BEGIN;
	    POP();
	    END3;
	}

    CASE(ADD): {  // ( n1 n2 -- sum )  sum = n1 + n2
	    BEGIN;
	    A = FST;
	    POP();
	    FST += A;
	    END3;
	}

    CASE(MUL): {  // ( n1 n2 -- prod )  prod = n1 * n2
	    BEGIN;
	    A = FST;
	    POP();
	    FST *= A;
	    END3;
	}
	
    CASE(NEGATE): {
	    BEGIN;
	    FST = -FST;
	    END;
	}
	
    CASE(AND): {
	    BEGIN;
	    A = FST;
	    POP();
	    FST &= A;
	    END;
	}
	
    CASE(OR): {
	    BEGIN;
	    A = FST;
	    POP();
	    FST |= A;
	    END;
	}
	
    CASE(ZEQ): {
	    BEGIN;
	    FST = CHINE_TEST(FST == 0);
	    END;
	}
	
    CASE(ZLT): {
	    BEGIN;
	    FST = CHINE_TEST(FST < 0);
	    END;
	}
	
    CASE(NOT): {
	    BEGIN;
	    FST = ~FST;
	    END;
	}	

    CASE(NOP): {
	    BEGIN;
	    END;
	}
	
    CASE(XOR): {
	    BEGIN;
	    A = FST;
	    POP();
	    FST ^= A;
	    END;
	}
	
    CASE(DIV): {  // ( n1 n2 -- quot )  tos = n1/n2
	    BEGIN;
	    A = FST;
	    POP();
	    if (A == 0) { POP(); goto L_FAIL_DIV_ZERO; }
	    FST /= A;
	    END;
	}

    CASE(SHIFT): { // shift left (or right)
	    BEGIN;
	    A = FST;
	    POP();
	    if (A >= 0)
		FST = ((ucell_t)FST) << A;
	    else
		FST = ((ucell_t)FST) >> -A;
	    END;
	}

    CASE(STORE): {
	    BEGIN;
	    cell_t addr = FST;
	    POP();
	    // printf("STORE: addr=0x%x value=0x%x\n", addr, FST);
	    vm_write_aligned_u32(mp, addr, FST);
	    POP();
	    END;
	}

    CASE(FETCH): {
	    BEGIN;
	    cell_t addr = FST;	    
	    FST = vm_read_aligned_u32(mp, addr);
	    // printf("FETCH: addr=0x%x value=0x%x\n", addr, FST);
	    END;
	}

    CASE(TOR): {
	    BEGIN;
	    A = FST;
	    POP();
	    *cRP++ = A;
	    END;
	}
	
    CASE(FROMR): {
	    BEGIN;
	    A = *--cRP;
	    PUSH(A);
	    END;
	}

    CASE(RFETCH): {
	    BEGIN;
	    PUSH(cRP[-1]);
	    END;
	}

    CASE(EXIT): {
	    BEGIN;
	    if (cRP == mp->stack) {
		SWAP_OUT(mp);
		TEND;
		goto L_FAIL_TERMINATE;
	    }
	    cIP = *--cRP;
	    END;
	}

    CASE(YIELD): {
	    BEGIN;
	    SWAP_OUT(mp);
	    XEND;
	    return 0;
	}
	
    CASE(SYS): {
	    BEGIN;
	    cell_t sysop = vm_read_u8(mp,cIP++);
	    cell_t ret;
	    cell_t npop;
	    cell_t value;
	    
	    SAVE();
	    if ((ret = (*mp->sys)(mp, sysop, cSP, &npop, &value)) < 0) {
		TEND;
		fail(ret);
	    }
	    cSP += npop; // pop arguments
	    RESTORE();
	    if (ret > 0) {
		PUSH(value);
	    }
	    END;
	}

    CASE(SIZE): {
	    BEGIN;
	    vaddr_t aptr;
	    if ((aptr = get_array(mp, FST)) == 0)
		goto L_FAIL_INVALID_ARGUMENT;
	    FST = sizeof_array(mp, aptr);
	    END;
	}

    CASE(ELEM): {
	    BEGIN;
	    vaddr_t aptr;
	    uint8_t B;
	    int i;
	    i = FST;             // array index
 	    POP();
	    if ((aptr = get_array(mp, FST)) == 0)
		goto L_FAIL_INVALID_ARGUMENT;
	    B = vm_read_u8(mp, aptr);
	    L = (1 << OP1VAL(B));            // number of bytes for A
	    A = sizeof_array(mp, aptr);      // number of elements
	    if ((i < 0) || (i > A)) goto L_FAIL_INVALID_ARGUMENT;
	    FST = get_element(mp, aptr, L, i);
	    END;
	}
	
    CASE(EXEC): {
	    BEGIN;
	    // place a call to the location given by addr on top of stack
	    // the address is a location relative to program start
	    A = cIP;      // save return address
	    cIP = FST;
	    *cRP++ = A;
	    POP();
	    END;
	}

    CASE(SPFETCH): {
	    BEGIN;
	    SAVE();
	    FST = (cSP - mp->stack);
	    END;
	}

    CASE(SPSTORE): {
	    BEGIN;
	    cSP = (mp->stack + FST);
	    RESTORE();
	    END;
	}		

    CASE(FPFETCH): {  // fp@ ( -- fp )
	    BEGIN;
	    PUSH(cFP - mp->stack);
	    END;
	}

    CASE(FPSTORE): {  // fp! ( fp -- )
	    BEGIN;
	    cFP = mp->stack + FST;
	    POP();
	    END;
	}

    CASE(CSTORE): {  // main RAM from RAM_PAGE
	    BEGIN;
	    uint32_t addr = FST;
	    POP();
	    vm_write_u8(mp, addr, FST);
	    POP();
	    END;
	}

    CASE(CFETCH): {
	    BEGIN;
	    FST = vm_read_u8(mp, FST);
	    END;
	}
    }
    
jump:  // op1 and op2
    switch(J) {
    default: goto L_FAIL_INVALID_OPCODE;
    CASE(JMPZ): {
	    BEGIN;
	    if (FST == 0)
		cIP = cIP + A;
	    POP();
	    END;
	}
	
    CASE(JMPNZ): {
	    BEGIN;
	    if (FST != 0)
		cIP = cIP + A;
	    POP();
	    END;
	}

    CASE(JNEXT): {
	    BEGIN;
	    if (--(cRP[-1])>0)
		cIP = cIP + A;
	    else
		cRP--;
	    END;
	}

    CASE(JMPLZ): {
	    BEGIN;
	    if (FST < 0)
		cIP = cIP + A;
	    POP();
	    END;
	}
	
    CASE(JMP): {
	    BEGIN;
	    cIP = cIP + A;
	    END;
	}

    CASE(CALL): {
	    BEGIN;
	    *cRP++ = cIP;
	    cIP += A;
	    END;
	}
	
    CASE(ARRAY): {
	    // push array pointer on stack and skip
	    BEGIN;
	    cell_t offs = (cIP-L-1);
	    // printf("array: offs = %d, [ip]=%d\r\n", offs, *(cIP-L-1));
	    PUSH(offs);
	    cIP = cIP + A;
	    END;
	}

    CASE(LITERAL): {
	    BEGIN;
	    PUSH(A);
	    END;
	}

    CASE(ENTER): {  // A = <<nargs:8,nlocals:16>>
	    // 'fenter' => ['fp@','>r','sp@','fp!'],
	    BEGIN;
	    cell_t L = A & 0xffff;     // locals
	    // check arg?
	    *cRP++ = (cFP - mp->stack);  // save old fp on return stack
	    SAVE();
	    cFP = cSP;                 // set new fp
	    // clear locals
	    memset((void*)(cFP-L), 0x00, L*sizeof(cell_t));
	    // setup new stack top
#ifdef USE_TOS_CACHE
	    cSP -= (L+1);
	    TOS = 0;
#else
	    cSP -= L;   
#endif
	    END;
	}
	
    CASE(LEAVE): { // A = <<nargs:8,nret:4>>
	    // 'leave' => ['fp@','r>','fp!','sp!'],
	    BEGIN;
	    cell_t R = A & 0xf;     // number of return values
	    cell_t N = A >> 4;      // N is number of arguments
	    cell_t FP0 = cFP - mp->stack;  // save furrent = fp@
	    cell_t* SP0;
	    SAVE();
	    SP0 = cSP + R;
	    cFP = mp->stack + *--cRP;      // restore fp
	    cSP = (mp->stack + FP0 + N); // restore sp + remove args
	    while(R--)
		*--cSP = *--SP0;
	    RESTORE();
	    END;
	}

    CASE(SET): {  // A = index in Fp
	    BEGIN;
	    cFP[A] = FST;
	    POP();
	    END;
	}

    CASE(GET): {
	    BEGIN;
	    PUSH(cFP[A]);
	    END;
	}
    }
    
// L_FAIL_STACK_UNDERFLOW:
//    fail(FAIL_STACK_UNDERFLOW);
// L_FAIL_STACK_OVERFLOW:
//    fail(FAIL_STACK_OVERFLOW);
L_FAIL_INVALID_OPCODE:
    fail(FAIL_INVALID_OPCODE);
L_FAIL_DIV_ZERO:
    fail(FAIL_DIV_ZERO);
// L_FAIL_INVALID_MEMORY_ADDRESS:
//     fail(FAIL_INVALID_MEMORY_ADDRESS);
L_FAIL_INVALID_ARGUMENT:
    fail(FAIL_INVALID_ARGUMENT);
L_FAIL_TERMINATE:
    fail(FAIL_TERMINATE);
    
L_fail:
    SWAP_OUT(mp);
    return -1;
}
