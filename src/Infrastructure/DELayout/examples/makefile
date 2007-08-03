# $Id: makefile,v 1.10 2007/08/03 22:28:34 theurich Exp $

ALL: build_examples
run: run_examples
run_uni: run_examples_uni

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
