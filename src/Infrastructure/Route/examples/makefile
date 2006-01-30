# $Id: makefile,v 1.12 2006/01/30 20:10:09 nscollins Exp $

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
LOCDIR	  = src/Infrastructure/Route/examples


EXAMPLES_BUILD   = $(ESMF_EXDIR)/ESMC_RouteEx \
                   $(ESMF_EXDIR)/ESMF_RouteEx

EXAMPLES_RUN     = run_ESMC_RouteEx \
                   run_ESMF_RouteEx

EXAMPLES_RUN_UNI = run_ESMC_RouteEx_uni \
                   run_ESMF_RouteEx_uni


include ${ESMF_DIR}/makefile

DIRS =


CLEANDIRS   = 
CLEANFILES  = $(EXAMPLES_BUILD)
CLOBBERDIRS =


#
# ESMC_RouteEx
#
run_ESMC_RouteEx:
	$(MAKE) EXNAME=Route NP=4 excrun

run_ESMC_RouteEx_uni:
	$(MAKE) EXNAME=Route NP=1 excrun


#
# ESMF_RouteEx
#
run_ESMF_RouteEx:
	$(MAKE) EXNAME=Route NP=4 exfrun

run_ESMF_RouteEx_uni:
	$(MAKE) EXNAME=Route NP=1 exfrun


