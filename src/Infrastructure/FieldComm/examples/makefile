# $Id: makefile,v 1.7 2004/06/18 09:17:34 nscollins Exp $

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
               
EXAMPLES_RUN_UNI = run_ESMF_FieldCommEx
                   


include ${ESMF_DIR}/makefile

DIRS = 


CLEANDIRS   = coredir.*
CLEANFILES  = *.o *.mod *.txt *.stdout NULL core $(EXAMPLES_BUILD)
CLOBBERDIRS = 



run_ESMF_FieldCommEx:
	-@rm -f testlog stdlog.* logfile.* ESMF_FieldCommEx.tmps
	-${MPIRUN} -np 4 $(ESMF_EXDIR)/ESMF_FieldCommEx > $(ESMF_EXDIR)/ESMF_FieldCommEx.stdout

run_ESMF_FieldCommEx_uni:
	-@rm -f testlog stdlog.* logfile.* ESMF_FieldCommEx.tmps
	-${MPIRUN} -np 1 $(ESMF_EXDIR)/ESMF_FieldCommEx > $(ESMF_EXDIR)/ESMF_FieldCommEx.stdout

