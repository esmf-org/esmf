# $Id: makefile,v 1.20 2007/07/18 21:45:31 oehmke Exp $

ALL:      build_examples
run:      run_examples
run_uni:  run_examples_uni

CFLAGS    =
FFLAGS    =
SOURCEC	  = 
SOURCEF	  = 
SOURCEH	  = 
OBJSC     = 
OBJSF     = 
TEXFILES  = 
MANSEC	  = 
LIBBASE	  = 
LOCDIR	  = src/Infrastructure/Grid/examples


EXAMPLES_BUILD    = $(ESMF_EXDIR)/ESMF_GridUsageEx \
	            $(ESMF_EXDIR)/ESMF_GridCreateRegFromDGEx

EXAMPLES_RUN      = run_ESMF_GridUsageEx \
	            run_ESMF_GridCreateRegFromDGEx

EXAMPLES_RUN_UNI  = run_ESMF_GridUsageEx_uni \
	            run_ESMF_GridCreateRegFromDGEx_uni

include $(ESMF_DIR)/makefile

DIRS	  = 

CLEANDIRS   = 
CLEANFILES  = $(EXAMPLES_BUILD)
CLOBBERDIRS =

#
#  ESMF_GridUsage
#
run_ESMF_GridUsageEx:
	$(MAKE) EXNAME=GridUsage NP=4 exfrun

run_ESMF_GridUsageEx_uni:
	$(MAKE) EXNAME=GridUsage NP=1 exfrun

#
#  ESMF_GridUsage
#
run_ESMF_GridCreateRegFromDGEx:
	$(MAKE) EXNAME=GridCreateRegFromDG NP=4 exfrun

run_ESMF_GridCreateRegFromDGEx_uni:
	$(MAKE) EXNAME=GridCreateRegFromDG NP=1 exfrun





