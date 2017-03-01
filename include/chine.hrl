-ifndef(__CHINE_HRL__).
-define(__CHINE_HRL__, true).

-define(OPCODE1(X), ((X) band 0x7f)).
-define(OPCODE2(A,B), (16#80 bor (((B) band 15) bsl 3) bor ((A) band 7))).

%% op3 : first part of OPCODE2 also an OPCODE1
-define(ZBRAN_H,     0).  %% either 4 or 8 bits offset
-define(LIT_H,       1).  %% const: ( -- x ) either 4 or 8 bits
-define(DUP,         2).  %% ( a -- a a )
-define(ROT,         3).  %% ( x1 x2 x3 -- x2 x3 x1 )
-define(OVER,        4).  %% over: ( x1 x2 -- x1 x2 x1 )
-define(DROP,        5).  %% drop: ( x1 -- )
-define(SWAP,        6).  %% ( a b -- b a )
-define(SUB,         7).   %% ( x1 x2 -- (x1-x2) )

%% op4 : second part of OPCODE2 also an OPCODE1
-define(ADD,         8).   %% +  ( x1 x2 -- (x1+x2) )
-define(MUL,         9).   %% *: ( x1 x2 -- (x1*x2) )
-define(EQ,          10).  %% =: ( a b -- ( a==b) )
-define(AND,         11).  %% and: ( a b -- (a&b) )
-define(OR,          12).  %% or: ( a b -- (a|b) )
-define(ZEQ,         13).  %% 0=:  ( a -- (a==0) )
-define(ZLT,         14).  %% 0<:  ( a -- (a<0) )
-define(NOT,         15).  %% not: ( a -- not a)

%% op7 : reset of opcodes 
-define(DIV,         16).  %% ( a b -- (a/b) )
-define(MOD,         17).  %% ( a b -- (a%b) )
-define(XOR,         18).  %% ( a b -- (a^b) )
-define(NEG,         19).  %% ( a -- (-a) )
-define(INV,         20).  %% ( a -- (~a) )
-define(BSL,         21).  %% <<: ( a n -- (a << n) )
-define(BSR,         22).  %% >>: ( a n -- (a >> n) )
-define(ASR,         23).  %% >>a: ( a n -- (a >> n) )
-define(ULT,         24).  %% ( a b -- (a < b) )
-define(STORE,       25).  %% ( a i -- )
-define(FETCH,       26).  %% ( i -- a )
-define(NOP,         27).  %% nop: ( -- )
-define(ULTE,        28).  %% '-' '--' '0<'
-define(RET,         29).  %% ( -- ) R: ( addr -- )
-define(LIT_W,       30).  %% ( -- w )
-define(LIT_L,       31).  %% ( -- l )
-define(BRAN_B,      32).  %% ( -- )
-define(BRAN_W,      33).  %% ( -- )
-define(ZBRAN_W,     34).  %% ( cond -- )
-define(IBRAN_B,     35).  %% ( i -- )
-define(IBRAN_W,     36).  %% ( i -- )
-define(CALL_B,      37).  %% ( -- ) R: ( -- addr )
-define(CALL_W,      38).  %% ( -- ) R: ( -- addr )
-define(SYS_B,       39).  %% sys ( x1 .. xn -- xi .. xj )

-define(EXIT,        40).  %% ( -- )
-define(YIELD,       41).  %% ( -- )

%% when LIT_H/ZBRAN_H is a op7 then it's the byte versions
-define(LIT_B,         ?LIT_H).
-define(ZBRAN_B,       ?ZBRAN_H).

%% Failure codes
-define(FAIL_STACK_OVERFLOW,    -1).
-define(FAIL_STACK_UNDERFLOW,   -2).
-define(FAIL_RSTACK_OVERFLOW,   -3).
-define(FAIL_RSTACK_UNDERFLOW,  -4).
-define(FAIL_DIV_ZERO,          -5).
-define(FAIL_TIMER_OVERFLOW,    -6).
-define(FAIL_MEMORY_OVERFLOW,   -7).

%% SYSTEM CALLS
-define(SYS_PARAM_FETCH,   1).  %% ( i s -- n )
-define(SYS_PARAM_STORE,   2).  %% ( v i s -- )
-define(SYS_TIMER_INIT,    3).  %% ( i --  )
-define(SYS_TIMER_START,   4).  %% ( i time --  )
-define(SYS_TIMER_STOP,    5).  %% ( i --  )
-define(SYS_TIMER_TIMEOUT, 6).  %% ( i -- f )
-define(SYS_TIMER_RUNNING, 7).  %% ( i -- f )
-define(SYS_INPUT_FETCH,   8).  %% ( i k -- n )
-define(SYS_SELECT,        9).  %% ( tmask imask -- )
-define(SYS_EMIT,         10).  %% ( c -- )
-define(SYS_KEY,          11).  %% (   -- c )

%% INPUT kind (k)
-define(INPUT_BOOLEAN, 0).
-define(INPUT_ANALOG,  1).
-define(INPUT_ENCODER, 2).

-endif.
