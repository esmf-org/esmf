# $Id: makefile,v 1.3 2004/06/03 15:34:11 nscollins Exp $

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

EXAPMLES_RUN_UNI = run_ESMF_FieldDataMapEx_uni


include ${ESMF_DIR}/makefile

DIRS = 

CLEANDIRS   = coredir.*
CLEANFILES  = *.o *.mod *.txt *.stdout NULL core $(EXAMPLES_BUILD)
CLOBBERDIRS =

run_ESMF_FieldDataMapEx:
	${MPIRUN} -np 4 ESMF_FieldDataMapEx > ESMF_FieldDataMapEx.stdout

run_ESMF_FieldDataMapEx_uni:
	${MPIRUN} -np 1 ESMF_FieldDataMapEx > ESMF_FieldDataMapEx.stdout

