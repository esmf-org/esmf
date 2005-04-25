# $Id: makefile,v 1.11 2005/04/25 22:03:46 jwolfe Exp $

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


EXAMPLES_BUILD  = $(ESMF_EXDIR)/ESMF_FieldCommEx \
		  $(ESMF_EXDIR)/ESMF_FieldRedistEx

#
# List the example run targets in EXAMPLES_RUN and 
# EXAMPLES_RUN_UNI.  All of the run targets should be defined
# in this makefile.
#

EXAMPLES_RUN = run_ESMF_FieldCommEx \
	       run_ESMF_FieldRedistEx
               
EXAMPLES_RUN_UNI = run_ESMF_FieldCommEx_uni \
		   run_ESMF_FieldRedistEx_uni
                   


include ${ESMF_DIR}/makefile

DIRS = 


CLEANDIRS   = 
CLEANFILES  = $(EXAMPLES_BUILD)
CLOBBERDIRS = 

#
# ESMF_FieldCommEx
#
run_ESMF_FieldCommEx:
	-${MPIRUN} -np 4 $(ESMF_EXDIR)/ESMF_FieldCommEx > $(ESMF_EXDIR)/ESMF_FieldCommEx.stdout

run_ESMF_FieldCommEx_uni:
	-${MPIRUN} -np 1 $(ESMF_EXDIR)/ESMF_FieldCommEx > $(ESMF_EXDIR)/ESMF_FieldCommEx.stdout

#
# ESMF_FieldRedistEx
#
run_ESMF_FieldRedistEx:
	-${MPIRUN} -np 6 $(ESMF_EXDIR)/ESMF_FieldRedistEx > $(ESMF_EXDIR)/ESMF_FieldRedistEx.stdout

run_ESMF_FieldRedistEx_uni:
	echo "PASS: ESMF_FieldRedistEx not set up to run on one processor." > $(ESMF_EXDIR)/ESMF_FieldRedistEx.stdout
