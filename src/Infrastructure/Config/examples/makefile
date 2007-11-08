# $Id: makefile,v 1.8 2007/11/08 21:39:35 svasquez Exp $ 

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
#                $(ESMF_EXDIR)/ESMC_ConfigOverviewEx

EXAMPLES_RUN = run_ESMF_ConfigOverviewEx
#              run_ESMC_ConfigOverviewEx

EXAMPLES_RUN_UNI = run_ESMF_ConfigOverviewEx_uni
#                  run_ESMC_ConfigOverviewEx_uni

include $(ESMF_DIR)/makefile

DIRS	  = 

CLEANDIRS   = 
CLEANFILES  = foo.nc $(EXAMPLES_BUILD) 0 1 2 3
CLOBBERDIRS =


#
# ESMF_ConfigOverviewEx
#
run_ESMF_ConfigOverviewEx:
	cp  -f myResourceFile.rc $(ESMF_EXDIR)/myResourceFile.rc
	$(MAKE) EXNAME=ConfigOverview NP=4 exfrun

run_ESMF_ConfigOverviewEx_uni:
	cp -f myResourceFile.rc $(ESMF_EXDIR)/myResourceFile.rc
	$(MAKE) EXNAME=ConfigOverview NP=1 exfrun

#
# ESMC_ConfigOverviewEx
#
run_ESMC_ConfigOverviewEx:
	cp -f myResourceFile.rc $(ESMF_EXDIR)/myResourceFile.rc
	$(MAKE) EXNAME=ConfigOverview NP=4 excrun

run_ESMC_ConfigOverviewEx_uni:
	cp -f myResourceFile.rc $(ESMF_EXDIR)/myResourceFile.rc
	$(MAKE) EXNAME=ConfigOverview NP=1 excrun

