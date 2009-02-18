# $Id: makefile,v 1.15 2009/02/18 01:33:34 rokuingh Exp $ 

ALL:      build_examples
run:      run_examples
run_uni:  run_examples_uni

LOCDIR	  = src/Superstructure/State/examples


EXAMPLES_BUILD = $(ESMF_EXDIR)/ESMF_StateEx \
                 $(ESMF_EXDIR)/ESMF_StateReconcileEx 

EXAMPLES_RUN   = run_ESMF_StateEx \
                 run_ESMF_StateReconcileEx 

EXAMPLES_RUN_UNI = run_ESMF_StateEx_uni \
                   run_ESMF_StateReconcileEx_uni 
                   
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


#
# ESMF_StateReconcileEx
#
run_ESMF_StateReconcileEx:
	$(MAKE) EXNAME=StateReconcile NP=4 exfrun

run_ESMF_StateReconcileEx_uni:
	$(MAKE) EXNAME=StateReconcile NP=1 exfrun
