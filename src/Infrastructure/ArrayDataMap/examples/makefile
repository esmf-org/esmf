# $Id: makefile,v 1.9 2006/01/30 20:10:06 nscollins Exp $

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
LOCDIR	  = src/Infrastructure/ArrayDataMap/examples

EXAMPLES_BUILD = $(ESMF_EXDIR)/ESMF_ArrayDataMapEx

# List the example run targets in EXAMPLES_RUN and
# EXAMPLES_RUN_UNI.  All of the run targets should be defined
# in this makefile.
#       
EXAMPLES_RUN = run_ESMF_ArrayDataMapEx

EXAMPLES_RUN_UNI = run_ESMF_ArrayDataMapEx_uni


include ${ESMF_DIR}/makefile

DIRS = 

CLEANDIRS   = 
CLEANFILES  = $(EXAMPLES_BUILD)
CLOBBERDIRS =

run_ESMF_ArrayDataMapEx:
	$(MAKE) EXNAME=ArrayDataMap NP=4 exfrun

run_ESMF_ArrayDataMapEx_uni:
	$(MAKE) EXNAME=ArrayDataMap NP=1 exfrun

