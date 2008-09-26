# $Id: makefile,v 1.25 2008/09/26 04:17:26 cdeluca Exp $ 

ALL: build_examples

run: run_examples

LOCDIR	  = src/Infrastructure/LogErr/examples

EXAMPLES_BUILD = $(ESMF_EXDIR)/ESMF_LogErrEx

EXAMPLES_RUN = run_ESMF_LogErrEx

EXAMPLES_RUN_UNI = run_ESMF_LogErrEx_uni


include $(ESMF_DIR)/makefile

DIRS	  = 

CLEANDIRS   = 
CLEANFILES  = $(EXAMPLES_BUILD)
CLOBBERDIRS =


#
#  ESMF_LogErrEx
#
run_ESMF_LogErrEx:
	$(MAKE) EXNAME=LogErr NP=4 exfrun

run_ESMF_LogErrEx_uni:
	$(MAKE) EXNAME=LogErr NP=1 exfrun

