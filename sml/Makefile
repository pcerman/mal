STEPS = step0_repl.sml step1_read_print.sml step2_eval.sml step3_env.sml \
	step4_if_fn_do.sml step5_tco.sml step6_file.sml step7_quote.sml \
	step8_macros.sml step9_try.sml stepA_mal.sml

STEP_BINS = $(STEPS:%.sml=%)
LAST_STEP_BIN = $(word $(words $(STEP_BINS)),$(STEP_BINS))

all: $(STEP_BINS)

dist: mal

mal: $(LAST_STEP_BIN)
	cp $< $@

$(STEP_BINS): %: %.sml
	polyc $< -o $@

clean:
	rm -rf $(STEP_BINS) mal

.PHONY: all clean
