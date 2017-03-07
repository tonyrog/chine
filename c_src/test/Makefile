#
# Build test program
#

CFLAGS = -O3 -Wall -DDISPATCH_WITH_SWITCH
#CFLAGS = -DTRACE -Wall -DDISPATCH_WITH_SWITCH

chine_test: ../chine.c ../../include/chine.h ../unix/chine_sys_unix.c chine_test.c
	$(CC) $(CFLAGS) -o $@ ../chine.c ../unix/chine_sys_unix.c chine_test.c

chine_test3: ../chine.c ../../include/chine.h ../unix/chine_sys_unix.c chine_test3.c
	$(CC)  $(CFLAGS) -o $@ ../chine.c ../unix/chine_sys_unix.c chine_test3.c

chine_bench: ../chine.c ../../include/chine.h ../unix/chine_sys_unix.c chine_bench.c
	$(CC) $(CFLAGS) -o $@ ../chine.c ../unix/chine_sys_unix.c chine_bench.c