# $Id$ 

ALL:      build_examples
run:      run_examples
run_uni:  run_examples_uni

LOCDIR	  = src/Superstructure/InternalState/examples


EXAMPLES_BUILD = $(ESMF_EXDIR)/ESMF_InternalStateEx \
                 $(ESMF_EXDIR)/ESMF_InternalStateModEx

EXAMPLES_RUN   = run_ESMF_InternalStateEx \
                 run_ESMF_InternalStateModEx

EXAMPLES_RUN_UNI = run_ESMF_InternalStateEx_uni \
                   run_ESMF_InternalStateModEx_uni

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

#
# ESMF_InternalStateModEx
#
run_ESMF_InternalStateModEx:
	$(MAKE) EXNAME=InternalStateMod NP=4 exfrun

run_ESMF_InternalStateModEx_uni:
	$(MAKE) EXNAME=InternalStateMod NP=1 exfrun
