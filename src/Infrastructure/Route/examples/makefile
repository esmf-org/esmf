# $Id: makefile,v 1.13 2007/04/12 20:48:22 theurich Exp $

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


EXAMPLES_BUILD   = $(ESMF_EXDIR)/ESMF_RouteEx

EXAMPLES_RUN     = run_ESMF_RouteEx

EXAMPLES_RUN_UNI = run_ESMF_RouteEx_uni


include ${ESMF_DIR}/makefile

DIRS =


CLEANDIRS   = 
CLEANFILES  = $(EXAMPLES_BUILD)
CLOBBERDIRS =


#
# ESMF_RouteEx
#
run_ESMF_RouteEx:
	$(MAKE) EXNAME=Route NP=4 exfrun

run_ESMF_RouteEx_uni:
	$(MAKE) EXNAME=Route NP=1 exfrun


