# $Id$ 

ALL:      build_examples
run:      run_examples
run_uni:  run_examples_uni

LOCDIR	  = src/Superstructure/AttachMethods/examples


EXAMPLES_BUILD = $(ESMF_EXDIR)/ESMF_AttachMethodsEx 

EXAMPLES_RUN   = run_ESMF_AttachMethodsEx

EXAMPLES_RUN_UNI = run_ESMF_AttachMethodsEx_uni
                   
include $(ESMF_DIR)/makefile


CLEANDIRS   = 
CLEANFILES  = $(EXAMPLES_BUILD)
CLOBBERDIRS =

DIRS = 

#
# ESMF_AttachMethodsEx
#
run_ESMF_AttachMethodsEx:
	$(MAKE) EXNAME=AttachMethods NP=4 exfrun

run_ESMF_AttachMethodsEx_uni:
	$(MAKE) EXNAME=AttachMethods NP=1 exfrun
