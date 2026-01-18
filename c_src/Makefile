# Build chine interpreter for unix and some debug/test programs
CC=gcc
LD=gcc
BIN = ../bin
CFLAGS += -Wno-unused-result -I../include # -std=c99
SRC = chine.c chine_exec.c chine_sys_unix.c

all: chine.opt chine.tos.opt chine.debug chine.tos.debug

chine.tos.debug: $(BIN)/chine.tos.debug
chine.tos.debug: CFLAGS += -g -USE_TOS_CACHE -DDEBUG -DTRACE

chine.debug:     $(BIN)/chine.debug
chine.debug:     CFLAGS += -g -DDEBUG -DTRACE

chine.opt:       $(BIN)/chine.opt
chine.opt:       CFLAGS += -O3

chine.tos.opt: $(BIN)/chine.tos.opt
chine.tos.opt: CFLAGS += -O3 -DUSE_TOS_CACHE

$(BIN)/chine.opt: $(SRC) ../include/chine.h
	$(CC) -o $@ $(CFLAGS) $(SRC)

$(BIN)/chine.debug: $(SRC) ../include/chine.h
	$(CC) -o $@ $(CFLAGS) $(SRC)

$(BIN)/chine.tos.opt: $(SRC) ../include/chine.h
	$(CC) -o $@ $(CFLAGS) $(SRC)

$(BIN)/chine.tos.debug: $(SRC) ../include/chine.h
	$(CC) -o $@ $(CFLAGS) $(SRC)
