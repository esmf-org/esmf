# $Id: makefile,v 1.2 2012/03/21 16:08:59 feiliu Exp $ 

ALL:      build_examples
run:      run_examples
run_uni:  run_examples_uni

LOCDIR	  = src/Infrastructure/XGrid/examples

EXAMPLES_BUILD = $(ESMF_EXDIR)/ESMF_XGridEx $(ESMF_EXDIR)/ESMF_XGridOnlineEx

EXAMPLES_RUN = run_ESMF_XGridEx run_ESMF_XGridOnlineEx

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

run_ESMF_XGridOnlineEx:
	$(MAKE) EXNAME=XGridOnline NP=4 exfrun
