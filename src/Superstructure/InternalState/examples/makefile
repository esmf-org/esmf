# $Id$ 

ALL:      build_examples
run:      run_examples
run_uni:  run_examples_uni

LOCDIR	  = src/Superstructure/InternalState/examples


EXAMPLES_BUILD = $(ESMF_EXDIR)/ESMF_InternalStateEx

EXAMPLES_RUN   = run_ESMF_InternalStateEx

EXAMPLES_RUN_UNI = run_ESMF_InternalStateEx_uni
                   
include $(ESMF_DIR)/makefile


CLEANDIRS   = 
CLEANFILES  = $(EXAMPLES_BUILD)
CLOBBERDIRS =

DIRS = 

#
# ESMF_InternalStateEx
#
run_ESMF_InternalStateEx:
	$(MAKE) EXNAME=InternalState NP=4 exfrun

run_ESMF_InternalStateEx_uni:
	$(MAKE) EXNAME=InternalState NP=1 exfrun
