# $Id: makefile,v 1.8 2004/08/03 19:33:45 nscollins Exp $

# makefile template for the examples directory.

ALL: build_examples

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

CLEANDIRS   = coredir.*
CLEANFILES  = *.o *.mod *.txt *.stdout NULL core ESMF_LogFile $(EXAMPLES_BUILD)
CLOBBERDIRS =

run_ESMF_BundleDataMapEx:
	-${MPIRUN} -np 4 $(ESMF_EXDIR)/ESMF_BundleDataMapEx > $(ESMF_EXDIR)/ESMF_BundleDataMapEx.stdout

run_ESMF_BundleDataMapEx_uni:
	-${MPIRUN} -np 1 $(ESMF_EXDIR)/ESMF_BundleDataMapEx > $(ESMF_EXDIR)/ESMF_BundleDataMapEx.stdout

