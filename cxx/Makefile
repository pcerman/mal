EXT := $(shell sh -c "uname -s 2>/dev/null | sed -re '/cygwin/I {s/.*/\.exe/; q}; s/.*//'")

CXX = g++
LD = $(CXX)
AR = ar

CXXFLAGS = -std=c++17 -g
LDFLAGS = -L .

STEPS = step0_repl.cxx step1_read_print.cxx step2_eval.cxx \
	step3_env.cxx step4_if_fn_do.cxx step5_tco.cxx step6_file.cxx \
	step7_quote.cxx step8_macros.cxx step9_try.cxx stepA_mal.cxx

TARGETS = $(STEPS:%.cxx=%)
STEP_BINS = $(STEPS:%.cxx=%$(EXT))

LIB_SRCS = reader.cxx printer.cxx types.cxx core.cxx gc.cxx env.cxx
LIB_OBJS = $(LIB_SRCS:%.cxx=%.o)

$(TARGETS): %: %.o libmal.a
	$(LD) $^ -o $@$(EXT) $(LDFLAGS) -lreadline -lmal

libmal.a : $(LIB_OBJS)
	$(AR) rcs $@ $^

core.o    : core.h types.h env.h reader.h gc.h
env.o     : env.h types.h gc.h
gc.o      : types.h env.h gc.h
printer.o : printer.h types.h env.h
reader.o  : reader.h types.h env.h gc.h
types.o   : types.h env.h reader.h gc.h

INC1_FILES = types.h env.h reader.h printer.h  gc.h
INC2_FILES = types.h env.h reader.h printer.h core.h gc.h

step1_read_print.o : $(INC1_FILES)
step2_eval.o       : $(INC2_FILES)
step3_env.o        : $(INC2_FILES)
step4_if_fn_do.o   : $(INC2_FILES)
step5_tco.o        : $(INC2_FILES)
step6_file.o       : $(INC2_FILES)
step7_quote.o      : $(INC2_FILES)
step8_macros.o     : $(INC2_FILES)
step9_try.o	   : $(INC2_FILES)
stepA_mal.o	   : $(INC2_FILES)

.PHONY: all clean

.SUFFIXES: .cxx .o

all: $(TARGETS)

mal: stepA_mal$(EXT)
	cp $< $@

.cxx.o:
	$(CXX) $(CXXFLAGS) -c $< -o $@

clean:
	rm -rf *.o $(STEP_BINS) libmal.a mal$(EXT)
	rm -f *~
