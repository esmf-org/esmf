# $Id$ 

ALL:      build_examples
run:      run_examples
run_uni:  run_examples_uni

LOCDIR	  = src/Superstructure/State/examples

.NOTPARALLEL:
EXAMPLES_BUILD = $(ESMF_EXDIR)/ESMF_StateReconcileEx

EXAMPLES_RUN   = run_ESMF_StateReconcileEx

EXAMPLES_RUN_UNI = run_ESMF_StateReconcileEx_uni
                   
include $(ESMF_DIR)/makefile


CLEANDIRS   = 
CLEANFILES  = $(EXAMPLES_BUILD)
CLOBBERDIRS =

DIRS =

#
# ESMF_StateReconcileEx
#
run_ESMF_StateReconcileEx:
	$(MAKE) EXNAME=StateReconcile NP=4 exfrun

run_ESMF_StateReconcileEx_uni:
	$(MAKE) EXNAME=StateReconcile NP=1 exfrun
