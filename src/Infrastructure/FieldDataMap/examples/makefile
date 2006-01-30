# $Id: makefile,v 1.10 2006/01/30 20:10:08 nscollins Exp $

ALL: build_examples

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
LOCDIR	  = src/Infrastructure/FieldDataMap/examples


EXAMPLES_BUILD = $(ESMF_EXDIR)/ESMF_FieldDataMapEx

EXAMPLES_RUN = run_ESMF_FieldDataMapEx

EXAMPLES_RUN_UNI = run_ESMF_FieldDataMapEx_uni


include ${ESMF_DIR}/makefile

DIRS = 

CLEANDIRS   = 
CLEANFILES  = $(EXAMPLES_BUILD)
CLOBBERDIRS =

run_ESMF_FieldDataMapEx:
	$(MAKE) EXNAME=FieldDataMap NP=4 exfrun

run_ESMF_FieldDataMapEx_uni:
	$(MAKE) EXNAME=FieldDataMap NP=1 exfrun


