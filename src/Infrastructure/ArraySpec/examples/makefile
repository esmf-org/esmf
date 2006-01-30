# $Id: makefile,v 1.6 2006/01/30 20:10:06 nscollins Exp $

# makefile template for the examples directory.

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
LOCDIR	  = src/Infrastructure/ArraySpec/examples

EXAMPLES_BUILD = $(ESMF_EXDIR)/ESMF_ArraySpecEx

# List the example run targets in EXAMPLES_RUN and
# EXAMPLES_RUN_UNI.  All of the run targets should be defined
# in this makefile.
#       
EXAMPLES_RUN = run_ESMF_ArraySpecEx

EXAMPLES_RUN_UNI = run_ESMF_ArraySpecEx_uni


include ${ESMF_DIR}/makefile

DIRS = 

CLEANDIRS   = 
CLEANFILES  = $(EXAMPLES_BUILD)
CLOBBERDIRS =

run_ESMF_ArraySpecEx:
	$(MAKE) EXNAME=ArraySpec NP=4 exfrun

run_ESMF_ArraySpecEx_uni:
	$(MAKE) EXNAME=ArraySpec NP=1 exfrun

