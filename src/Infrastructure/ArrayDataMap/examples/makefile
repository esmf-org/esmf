# $Id: makefile,v 1.3 2004/06/04 12:09:05 nscollins Exp $

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
LOCDIR	  = src/Infrastructure/ArrayDataMap/examples

EXAMPLES_BUILD = $(ESMF_EXDIR)/ESMF_ArrayDataMapEx

# List the example run targets in EXAMPLES_RUN and
# EXAMPLES_RUN_UNI.  All of the run targets should be defined
# in this makefile.
#       
EXAMPLES_RUN = run_ESMF_ArrayDataMapEx

EXAPMLES_RUN_UNI = run_ESMF_ArrayDataMapEx_uni


include ${ESMF_DIR}/makefile

DIRS = 

CLEANDIRS   = coredir.*
CLEANFILES  = *.o *.mod *.txt *.stdout NULL core $(EXAMPLES_BUILD)
CLOBBERDIRS =

run_ESMF_ArrayDataMapEx:
	${MPIRUN} -np 4 ESMF_ArrayDataMapEx > ESMF_ArrayDataMapEx.stdout

run_ESMF_ArrayDataMapEx_uni:
	${MPIRUN} -np 1 ESMF_ArrayDataMapEx > ESMF_ArrayDataMapEx.stdout

