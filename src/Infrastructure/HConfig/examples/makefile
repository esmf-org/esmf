# $Id$

ALL: build_examples
run: run_examples
run_uni: run_examples_uni

LOCDIR	  = src/Infrastructure/HConfig/examples


EXAMPLES_BUILD    = $(ESMF_EXDIR)/ESMF_HConfigEx

EXAMPLES_RUN      = run_ESMF_HConfigEx

EXAMPLES_RUN_UNI  = run_ESMF_HConfigEx_uni


include $(ESMF_DIR)/makefile

DIRS	  = 

CLEANDIRS   = 
CLEANFILES  = $(EXAMPLES_BUILD)
CLOBBERDIRS =

#
#  Run targets
#
run_ESMF_HConfigEx:
	$(MAKE) EXNAME=HConfig NP=4 exfrun

run_ESMF_HConfigEx_uni:
	$(MAKE) EXNAME=HConfig NP=1 exfrun
