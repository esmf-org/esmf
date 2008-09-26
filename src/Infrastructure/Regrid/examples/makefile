# $Id: makefile,v 1.13 2008/09/26 04:17:27 cdeluca Exp $

ALL: build_examples

run: run_examples

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


