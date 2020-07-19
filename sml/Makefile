STEPS = step0_repl.sml step1_read_print.sml step2_eval.sml step3_env.sml \
	step4_if_fn_do.sml step5_tco.sml step6_file.sml step7_quote.sml \
	step8_macros.sml step9_try.sml stepA_mal.sml

STEP_BINS = $(STEPS:%.sml=%)
LAST_STEP_BIN = $(word $(words $(STEP_BINS)),$(STEP_BINS))

LIB1_FILES = avl-tree.sml types.sml printer.sml reader.sml
LIB2_FILES = $(LIB1_FILES) env.sml core.sml

all: $(STEP_BINS)

dist: mal

mal: $(LAST_STEP_BIN)
	cp $< $@

step1_read_print : $(LIB1_FILES)
step2_eval       : eval_step2.sml $(LIB2_FILES)
step3_env        : eval_step3.sml $(LIB2_FILES)
step4_if_fn_do   : eval_step4.sml $(LIB2_FILES)
step5_tco        : eval_step5.sml $(LIB2_FILES)
step6_file       : eval_step6.sml $(LIB2_FILES)
step7_quote      : eval_step7.sml $(LIB2_FILES)
step8_macros     : eval_step8.sml $(LIB2_FILES)
step9_try        : eval_step9.sml $(LIB2_FILES)
stepA_mal        : eval_stepA.sml $(LIB2_FILES)

$(STEP_BINS): %: %.sml
	mlton $@.mlb

clean:
	rm -rf $(STEP_BINS) mal
	rm -f *~

.PHONY: all clean
