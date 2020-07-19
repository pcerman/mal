DOTNET="/cygdrive/c/Program Files/dotnet"

STEPS = step0_repl.cs step1_read_print.cs step2_eval.cs \
	step3_env.cs step4_if_fn_do.cs step5_tco.cs step6_file.cs \
	step7_quote.cs step8_macros.cs step9_try.cs stepA_mal.cs

TARGETS = $(STEPS:%.cs=%)

$(TARGETS): %: %.cs
	dotnet build -o bin -c Release prjs/$(@).csproj

.PHONY: all clean

.SUFFIXES: .cxx .o

all: $(TARGETS)

mal: stepA_mal
	cp $< $@

clean:
	rm -rf bin
	rm -rf prjs/obj
	rm -f *~
