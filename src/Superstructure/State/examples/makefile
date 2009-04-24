# $Id: makefile,v 1.16 2009/04/24 05:20:17 theurich Exp $ 

ALL:      build_examples
run:      run_examples
run_uni:  run_examples_uni

LOCDIR	  = src/Superstructure/State/examples


EXAMPLES_BUILD = $(ESMF_EXDIR)/ESMF_StateEx \
                 $(ESMF_EXDIR)/ESMF_StateReconcileEx \
                 $(ESMF_EXDIR)/ESMF_AttachMethodsEx 

EXAMPLES_RUN   = run_ESMF_StateEx \
                 run_ESMF_StateReconcileEx \
                 run_ESMF_AttachMethodsEx

EXAMPLES_RUN_UNI = run_ESMF_StateEx_uni \
                   run_ESMF_StateReconcileEx_uni \
                   run_ESMF_AttachMethodsEx_uni
                   
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

#
# ESMF_AttachMethodsEx
#
run_ESMF_AttachMethodsEx:
	$(MAKE) EXNAME=AttachMethods NP=4 exfrun

run_ESMF_AttachMethodsEx_uni:
	$(MAKE) EXNAME=AttachMethods NP=1 exfrun
