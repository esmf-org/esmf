# $Id: makefile,v 1.1 2010/07/16 14:15:08 feiliu Exp $ 

ALL:      build_examples
run:      run_examples
run_uni:  run_examples_uni

LOCDIR	  = src/Infrastructure/XGrid/examples

EXAMPLES_BUILD = $(ESMF_EXDIR)/ESMF_XGridEx

EXAMPLES_RUN = run_ESMF_XGridEx

EXAMPLES_RUN_UNI = 

include $(ESMF_DIR)/makefile

DIRS	  = 

CLEANDIRS   = 
CLEANFILES  = $(EXAMPLES_BUILD)
CLOBBERDIRS =

#
# ESMF_XGridEx
#
run_ESMF_XGridEx:
	$(MAKE) EXNAME=XGrid NP=1 exfrun
