# Build chine interpreter for unix and some debug/test programs
CC=gcc
LD=gcc
BIN = ../bin
CFLAGS=-MMD -MP -MF .$<.d
CFLAGS += -Wno-unused-result -I../include # -std=c99
CFLAGS += -Wall -O3 -static 

LDFLAGS += # -nostdlib
LIBS= #-lc

CHINE_OBJS =  chine.o chine_sys_unix.o
CHINE_EXEC_OBJS = chine_exec.o $(CHINE_OBJS)
CHINE_TEST = chine_test chine_test3
CHINE_BENCH = chine_bench
CHINE_ALL   = $(BIN)/chine_exec $(BIN)/chine_trace $(CHINE_TEST) $(CHINE_BENCH)

all: $(BIN)/chine_exec

chine_exec: $(BIN)/chine_exec

chine_trace: $(BIN)/chine_trace

test: $(CHINE_TEST)

bench: $(CHINE_BENCH)

tutti:	$(CHINE_ALL)

$(BIN)/chine_exec: $(CHINE_EXEC_OBJS)
	$(LD) $(LDFLAGS) -o $@ $(CHINE_EXEC_OBJS) $(LIBS)

$(BIN)/chine_trace: $(CHINE_EXEC_OBJS)
	$(LD) $(LDFLAGS) -o $@ $(CHINE_EXEC_OBJS) $(LIBS)

chine_test:	$(CHINE_OBJS) chine_test.o
	$(LD) $(LDFLAGS) -o $@ $(CHINE_OBJS) chine_test.o $(LIBS)

chine_test3: $(CHINE_OBJS) chine_test3.o
	$(LD) $(LDFLAGS) -o $@ $(CHINE_OBJS) chine_test3.o $(LIBS)

chine_bench:  $(CHINE_OBJS) chine_bench.o
	$(LD) $(LDFLAGS) -o $@ $(CHINE_OBJS) chine_bench.o $(LIBS)

%.o:	%.c
	$(CC) $(CFLAGS) -c $<

.%.d:	;

-include .*.d
