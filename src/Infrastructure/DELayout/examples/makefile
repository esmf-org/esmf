# $Id: makefile,v 1.9 2006/01/30 20:10:08 nscollins Exp $

ALL: build_examples

run: run_examples

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
LOCDIR	  = src/Infrastructure/DELayout/examples


EXAMPLES_BUILD    = $(ESMF_EXDIR)/ESMF_DELayoutEx

EXAMPLES_RUN      = run_ESMF_DELayoutEx

EXAMPLES_RUN_UNI  = run_ESMF_DELayoutEx_uni


include $(ESMF_DIR)/makefile

DIRS	  = 

CLEANDIRS   = 
CLEANFILES  = $(EXAMPLES_BUILD)
CLOBBERDIRS =

#
#  Run targets
#
run_ESMF_DELayoutEx:
	$(MAKE) EXNAME=DELayout NP=4 exfrun

run_ESMF_DELayoutEx_uni:
	$(MAKE) EXNAME=DELayout NP=1 exfrun

