# $Id: makefile,v 1.10 2005/03/01 21:01:40 nscollins Exp $

# makefile template for the examples directory.

ALL:  build_examples

CFLAGS    =
FFLAGS    =
SOURCEC	  = 
SOURCEF	  =

SOURCEH	  = 
OBJSC	  =  
OBJSF	  = 
MANSEC	  = 
LIBBASE	  = 
LOCDIR	  = src/Infrastructure/FieldComm/examples


EXAMPLES_BUILD  = $(ESMF_EXDIR)/ESMF_FieldCommEx

#
# List the example run targets in EXAMPLES_RUN and 
# EXAMPLES_RUN_UNI.  All of the run targets should be defined
# in this makefile.
#

EXAMPLES_RUN = run_ESMF_FieldCommEx
               
EXAMPLES_RUN_UNI = run_ESMF_FieldCommEx_uni
                   


include ${ESMF_DIR}/makefile

DIRS = 


CLEANDIRS   = 
CLEANFILES  = $(EXAMPLES_BUILD)
CLOBBERDIRS = 



run_ESMF_FieldCommEx:
	-${MPIRUN} -np 4 $(ESMF_EXDIR)/ESMF_FieldCommEx > $(ESMF_EXDIR)/ESMF_FieldCommEx.stdout

run_ESMF_FieldCommEx_uni:
	-${MPIRUN} -np 1 $(ESMF_EXDIR)/ESMF_FieldCommEx > $(ESMF_EXDIR)/ESMF_FieldCommEx.stdout

