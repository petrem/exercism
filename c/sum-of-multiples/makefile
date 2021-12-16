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
CFLAGS += -DUNITY_SUPPORT_64

ASANFLAGS  = -fsanitize=address
ASANFLAGS += -fno-common
ASANFLAGS += -fno-omit-frame-pointer

SUM_OF_MULTIPLES_SRC = $(wildcard src/*.c)
SUM_OF_MULTIPLES_OBJ = $(addsuffix .o, $(basename $(notdir $(SUM_OF_MULTIPLES_SRC))))

TEST_SRC = test/vendor/unity.c $(wildcard test/*.c)
TEST_OBJ = $(addsuffix .o, $(basename $(notdir $(TEST_SRC))))

.PHONY: clean all

all: test

depend: .depend

.depend: $(SUM_OF_MULTIPLES_SRC) $(TEST_SRC)
	@rm -f ./.depend
	@$(CC) $(CFLAGS) -MM $^ > ./.depend;

include .depend

%.o:
	$(CC) $(CFLAGS) -c $<

test: tests.out
	@./tests.out

memcheck: tests.out
	@echo Compiling $@
	@$(CC) $(ASANFLAGS) $(CFLAGS) $(SUM_OF_MULTIPLES_SRC) $(TEST_SRC) -o memcheck.out $(LIBS)
	@./memcheck.out
	@echo "Memory check passed"

clean:
	rm -rf *.o *.out *.out.dSYM

tests.out: $(TEST_OBJ) $(SUM_OF_MULTIPLES_OBJ)
	@echo Compiling $@
	@$(CC) $(CFLAGS) $^ -o tests.out $(LIBS)
