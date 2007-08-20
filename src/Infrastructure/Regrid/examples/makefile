# $Id: makefile,v 1.12 2007/08/20 22:57:41 cdeluca Exp $

ALL: build_examples

run: run_examples

CFLAGS    =
FFLAGS    =
SOURCEC	  = 
SOURCEF	  = 
SOURCEH	  = 
OBJSC     = 
OBJSF     = 

EXAMPLES_BUILD   = 
# $(ESMF_EXDIR)/ESMF_RegridEx

EXAMPLES_RUN     = 
# run_ESMF_RegridEx

EXAMPLES_RUN_UNI = 
# run_ESMF_RegridEx_uni


include ${ESMF_DIR}/makefile

DIRS =


CLEANDIRS   = 
CLEANFILES  = $(EXAMPLES_BUILD)
CLOBBERDIRS =


#
# ESMF_RegridEx
#
run_ESMF_RegridEx:
	$(MAKE) EXNAME=Regrid NP=4 exfrun

run_ESMF_RegridEx_uni:
	$(MAKE) EXNAME=Regrid NP=1 exfrun


