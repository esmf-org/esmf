# $Id: makefile,v 1.1 2004/06/09 08:36:43 nscollins Exp $

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
LOCDIR	  = src/Infrastructure/ArraySpec/examples

EXAMPLES_BUILD = $(ESMF_EXDIR)/ESMF_ArraySpecEx

# List the example run targets in EXAMPLES_RUN and
# EXAMPLES_RUN_UNI.  All of the run targets should be defined
# in this makefile.
#       
EXAMPLES_RUN = run_ESMF_ArraySpecEx

EXAPMLES_RUN_UNI = run_ESMF_ArraySpecEx_uni


include ${ESMF_DIR}/makefile

DIRS = 

CLEANDIRS   = coredir.*
CLEANFILES  = *.o *.mod *.txt *.stdout NULL core $(EXAMPLES_BUILD)
CLOBBERDIRS =

run_ESMF_ArraySpecEx:
	${MPIRUN} -np 4 $(ESMF_EXDIR)/ESMF_ArraySpecEx > $(ESMF_EXDIR)/ESMF_ArraySpecEx.stdout

run_ESMF_ArraySpecEx_uni:
	${MPIRUN} -np 1 $(ESMF_EXDIR)/ESMF_ArraySpecEx > $(ESMF_EXDIR)/ESMF_ArraySpecEx.stdout

