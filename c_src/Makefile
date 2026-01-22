# Build chine interpreter for unix and some debug/test programs
CC=gcc
LD=gcc
BIN = ../bin
CFLAGS += -Wall -Wno-unused-result -I../include # -std=c99
SRC =  chine.c chine_exec.c chine_sys_unix.c

.phone: all

all: $(BIN)/chine.opt $(BIN)/chine.tos.opt $(BIN)/chine.tos.debug $(BIN)/chine.debug $(BIN)/chine.tos.small $(BIN)/chine.small

$(BIN)/chine.tos.debug: CFLAGS += -g -USE_TOS_CACHE -DDEBUG -DTRACE
$(BIN)/chine.debug:     CFLAGS += -g -DDEBUG -DTRACE
$(BIN)/chine.opt:       CFAGS  += -O3
$(BIN)/chine.tos.opt:   CFLAGS += -O3 -DUSE_TOS_CACHE
$(BIN)/chine.small:     CFAGS  += -Os
$(BIN)/chine.tos.small: CFLAGS += -Os -DUSE_TOS_CACHE

$(BIN)/chine.opt: $(SRC) ../include/chine.h
	$(CC) -o $@ $(CFLAGS) $(SRC)

$(BIN)/chine.tos.opt: $(SRC) ../include/chine.h
	$(CC) -o $@ $(CFLAGS) $(SRC)

$(BIN)/chine.tos.debug: $(SRC) ../include/chine.h
	$(CC) -o $@ $(CFLAGS) $(SRC)

$(BIN)/chine.debug: $(SRC) ../include/chine.h
	$(CC) -o $@ $(CFLAGS) $(SRC)

$(BIN)/chine.tos.small: $(SRC) ../include/chine.h
	$(CC) -o $@ $(CFLAGS) $(SRC)

$(BIN)/chine.small: $(SRC) ../include/chine.h
	$(CC) -o $@ $(CFLAGS) $(SRC)

