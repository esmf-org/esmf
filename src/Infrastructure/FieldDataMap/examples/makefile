# $Id: makefile,v 1.9 2005/03/01 21:02:09 nscollins Exp $

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
LOCDIR	  = src/Infrastructure/FieldDataMap/examples

EXAMPLES_BUILD = $(ESMF_EXDIR)/ESMF_FieldDataMapEx

# List the example run targets in EXAMPLES_RUN and
# EXAMPLES_RUN_UNI.  All of the run targets should be defined
# in this makefile.
#       
EXAMPLES_RUN = run_ESMF_FieldDataMapEx

EXAMPLES_RUN_UNI = run_ESMF_FieldDataMapEx_uni


include ${ESMF_DIR}/makefile

DIRS = 

CLEANDIRS   = 
CLEANFILES  = $(EXAMPLES_BUILD)
CLOBBERDIRS =

run_ESMF_FieldDataMapEx:
	-${MPIRUN} -np 4 $(ESMF_EXDIR)/ESMF_FieldDataMapEx > $(ESMF_EXDIR)/ESMF_FieldDataMapEx.stdout

run_ESMF_FieldDataMapEx_uni:
	-${MPIRUN} -np 1 $(ESMF_EXDIR)/ESMF_FieldDataMapEx > $(ESMF_EXDIR)/ESMF_FieldDataMapEx.stdout

