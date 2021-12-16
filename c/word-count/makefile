### If you wish to use extra libraries,
### add their flags in the "LIBS" variable (like we do with "CFLAGS").

###
CFLAGS  = -std=c99
CFLAGS += -g
CFLAGS += -Wall
CFLAGS += -Wextra
CFLAGS += -pedantic
CFLAGS += -Werror

#VFLAGS  = --quiet
VFLAGS += --tool=memcheck
VFLAGS += --leak-check=full
VFLAGS += --error-exitcode=1

WORD_COUNT_SRC = src/word_count.c src/bst.c src/alloc.c

WC_MAIN_SRC = src/wc.c
WC_MAIN_OBJ = wc.o

WORD_COUNT_OBJ = $(addsuffix .o, $(basename $(notdir $(WORD_COUNT_SRC))))

TEST_SRC = test/test_word_count.c test/vendor/unity.c
TEST_OBJ = $(addsuffix .o, $(basename $(notdir $(TEST_SRC))))

.PHONY: clean all debug

all: wc.out

debug: wc.out
debug: CFLAGS += -DDEBUG

depend: .depend

.depend: $(WORD_COUNT_SRC) $(WC_MAIN_SRC) $(TEST_SRC)
	rm -f ./.depend
	$(CC) $(CFLAGS) -MM $^ > ./.depend;

include .depend

%.o :
	$(CC) $(CFLAGS) -c $<

test: tests.out
	@./tests.out

memcheck: tests.out
	@valgrind $(VFLAGS) ./tests.out
	@echo "Memory check passed"

clean:
	rm -rf *.o *.out *.out.dSYM

tests.out: $(WORD_COUNT_OBJ) $(TEST_OBJ)
	@echo Compiling $@
	@$(CC) $(CFLAGS) $^ -o tests.out $(LIBS)

wc.out: $(WORD_COUNT_OBJ) $(WC_MAIN_OBJ)
	@echo Compiling $@
	@$(CC) $(CFLAGS) $^ -o $@ $(LIBS)


