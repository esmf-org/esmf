# $Id: makefile,v 1.10 2006/01/30 20:10:07 nscollins Exp $

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
LOCDIR	  = src/Infrastructure/BundleDataMap/examples

EXAMPLES_BUILD = $(ESMF_EXDIR)/ESMF_BundleDataMapEx

# List the example run targets in EXAMPLES_RUN and
# EXAMPLES_RUN_UNI.  All of the run targets should be defined
# in this makefile.
#       
EXAMPLES_RUN = run_ESMF_BundleDataMapEx

EXAMPLES_RUN_UNI = run_ESMF_BundleDataMapEx_uni


include ${ESMF_DIR}/makefile

DIRS = 

CLEANDIRS   = 
CLEANFILES  = $(EXAMPLES_BUILD)
CLOBBERDIRS =

run_ESMF_BundleDataMapEx:
	$(MAKE) EXNAME=BundleDataMap NP=4 exfrun

run_ESMF_BundleDataMapEx_uni:
	$(MAKE) EXNAME=BundleDataMap NP=1 exfrun

