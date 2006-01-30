# $Id: makefile,v 1.2 2006/01/30 20:10:07 nscollins Exp $ 

ALL:  build_examples

run: run_examples

CFLAGS    =
FFLAGS    =
SOURCEC	  = 
SOURCEF	  =

SOURCEH	  = 
OBJSC	  =  
OBJSF	  = 
MANSEC	  = 
LIBBASE	  = 
LOCDIR	  = src/Infrastructure/Config/examples

EXAMPLES_BUILD = $(ESMF_EXDIR)/ESMF_ConfigOverviewEx

EXAMPLES_RUN = run_ESMF_ConfigOverviewEx

EXAMPLES_RUN_UNI = run_ESMF_ConfigOverviewEx_uni

include $(ESMF_DIR)/makefile

DIRS	  = 

CLEANDIRS   = 
CLEANFILES  = foo.nc $(EXAMPLES_BUILD) 0 1 2 3
CLOBBERDIRS =


#
# ESMF_ConfigOverviewEx
#
run_ESMF_ConfigOverviewEx:
	$(MAKE) EXNAME=ConfigOverview NP=4 exfrun

run_ESMF_ConfigOverviewEx_uni:
	$(MAKE) EXNAME=ConfigOverview NP=1 exfrun

