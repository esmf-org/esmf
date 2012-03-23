# $Id: makefile,v 1.3 2012/03/23 16:38:54 svasquez Exp $ 

ALL:      build_examples
run:      run_examples
run_uni:  run_examples_uni

LOCDIR	  = src/Infrastructure/XGrid/examples

EXAMPLES_BUILD = $(ESMF_EXDIR)/ESMF_XGridEx $(ESMF_EXDIR)/ESMF_XGridOnlineEx

EXAMPLES_RUN = run_ESMF_XGridEx run_ESMF_XGridOnlineEx

EXAMPLES_RUN_UNI = run_ESMF_XGridEx_uni

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

run_ESMF_XGridEx_uni:
	$(MAKE) EXNAME=XGrid NP=1 exfrun

#
# ESMF_XGridOnline
#
run_ESMF_XGridOnlineEx:
	$(MAKE) EXNAME=XGridOnline NP=4 exfrun
