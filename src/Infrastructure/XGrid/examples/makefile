# $Id$ 

ALL:      build_examples
run:      run_examples
run_uni:  run_examples_uni

LOCDIR	  = src/Infrastructure/XGrid/examples

.NOTPARALLEL:
EXAMPLES_BUILD = $(ESMF_EXDIR)/ESMF_XGridEx $(ESMF_EXDIR)/ESMF_XGridSparseMatEx

EXAMPLES_RUN = run_ESMF_XGridEx run_ESMF_XGridSparseMatEx

EXAMPLES_RUN_UNI = run_ESMF_XGridSparseMatEx_uni

include $(ESMF_DIR)/makefile

DIRS	  = 

CLEANDIRS   = 
CLEANFILES  = $(EXAMPLES_BUILD)
CLOBBERDIRS =

#
# ESMF_XGridEx
#
run_ESMF_XGridSparseMatEx:
	$(MAKE) EXNAME=XGridSparseMat NP=1 exfrun

run_ESMF_XGridSparseMatEx_uni:
	$(MAKE) EXNAME=XGridSparseMat NP=1 exfrun

#
# ESMF_XGrid
#
run_ESMF_XGridEx:
	$(MAKE) EXNAME=XGrid NP=4 exfrun
