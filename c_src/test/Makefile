#
# Build test program
#

CFLAGS = -O3 -Wall
#CFLAGS = -DTRACE -Wall
#CFLAGS = -g -DTRACE -Wall
BIN = ../../bin

all:	$(BIN)/chine_exec $(BIN)/chine_trace chine_test chine_test3 chine_bench

$(BIN)/chine_exec: ../chine.c ../../include/chine.h ../unix/chine_sys_unix.c chine_exec.c
	$(CC) $(CFLAGS) -o $@ ../chine.c ../unix/chine_sys_unix.c chine_exec.c

$(BIN)/chine_trace: ../chine.c ../../include/chine.h ../unix/chine_sys_unix.c chine_exec.c
	$(CC) -DTRACE $(CFLAGS) -o $@ ../chine.c ../unix/chine_sys_unix.c chine_exec.c

chine_test: ../chine.c ../../include/chine.h ../unix/chine_sys_unix.c chine_test.c
	$(CC) $(CFLAGS) -o $@ ../chine.c ../unix/chine_sys_unix.c chine_test.c

chine_test3: ../chine.c ../../include/chine.h ../unix/chine_sys_unix.c chine_test3.c
	$(CC)  $(CFLAGS) -o $@ ../chine.c ../unix/chine_sys_unix.c chine_test3.c

chine_bench: ../chine.c ../../include/chine.h ../unix/chine_sys_unix.c chine_bench.c
	$(CC) $(CFLAGS) -o $@ ../chine.c ../unix/chine_sys_unix.c chine_bench.c
