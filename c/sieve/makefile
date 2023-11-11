### If you wish to use extra libraries (math.h for instance),
### add their flags here (-lm in our case) in the "LIBS" variable.

LIBS = -lm

###
CFLAGS  = -std=c99
CFLAGS += -g
CFLAGS += -Wall
CFLAGS += -Wextra
CFLAGS += -pedantic
CFLAGS += -Werror
CFLAGS += -Wmissing-declarations
CFLAGS += -DUNITY_SUPPORT_64

ASANFLAGS  = -fsanitize=address
ASANFLAGS += -fno-common
ASANFLAGS += -fno-omit-frame-pointer

test: tests.out
	@./tests.out

test_bitvector: tests_bv.out
	@./tests_bv.out

memcheck: test_sieve.c sieve.c bitvector.c sieve.h bitvector.h
	@echo Compiling $@
	@$(CC) $(ASANFLAGS) $(CFLAGS) sieve.c bitvector.c test-framework/unity.c test_sieve.c -o memcheck.out $(LIBS)
	@./memcheck.out
	@echo "Memory check passed"

clean:
	rm -rf *.o *.out *.out.dSYM

tests.out: test_sieve.c sieve.c bitvector.c sieve.h bitvector.h
	@echo Compiling $@
	@$(CC) $(CFLAGS) sieve.c bitvector.c test-framework/unity.c test_sieve.c -o $@ $(LIBS)

tests_bv.out: test_bitvector.c bitvector.c bitvector.h
	@echo Compiling $@
	@$(CC) $(CFLAGS) bitvector.c test-framework/unity.c test_bitvector.c -o $@ $(LIBS)
