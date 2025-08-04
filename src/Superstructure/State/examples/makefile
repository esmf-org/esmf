# $Id$ 

ALL:      build_examples
run:      run_examples
run_uni:  run_examples_uni

LOCDIR	  = src/Superstructure/State/examples

.NOTPARALLEL:
EXAMPLES_BUILD = $(ESMF_EXDIR)/ESMF_StateEx


EXAMPLES_RUN   = run_ESMF_StateEx

EXAMPLES_RUN_UNI = run_ESMF_StateEx_uni

                   
include $(ESMF_DIR)/makefile


CLEANDIRS   = 
CLEANFILES  = $(EXAMPLES_BUILD)
CLOBBERDIRS =

DIRS = 

#
# ESMF_StateEx
#
run_ESMF_StateEx:
	$(MAKE) EXNAME=State NP=4 exfrun

run_ESMF_StateEx_uni:
	$(MAKE) EXNAME=State NP=1 exfrun

