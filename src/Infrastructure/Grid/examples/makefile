# $Id: makefile,v 1.14 2006/01/30 20:10:08 nscollins Exp $

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
LOCDIR	  = src/Infrastructure/Grid/examples


EXAMPLES_BUILD    = $(ESMF_EXDIR)/ESMF_GridCreateEx

EXAMPLES_RUN      = run_ESMF_GridCreateEx

EXAMPLES_RUN_UNI  = run_ESMF_GridCreateEx_uni


include $(ESMF_DIR)/makefile

DIRS	  = 

CLEANDIRS   = 
CLEANFILES  = $(EXAMPLES_BUILD)
CLOBBERDIRS =

#
#  ESMF_GridCreate
#
run_ESMF_GridCreateEx:
	$(MAKE) EXNAME=GridCreate NP=4 exfrun

run_ESMF_GridCreateEx_uni:
	echo "PASS: ESMF_GridCreateEx not set up to run on one processor." > $(ESMF_EXDIR)/ESMF_GridCreateEx.stdout




