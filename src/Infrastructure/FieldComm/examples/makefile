# $Id: makefile,v 1.4 2004/06/08 09:27:17 nscollins Exp $

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

include ${ESMF_DIR}/makefile

DIRS = 


run_ESMF_FieldCommEx:
	-@rm -f testlog stdlog.* logfile.* ESMF_FieldCommEx.tmps
	${MPIRUN} -np 4 ESMF_FieldCommEx > ESMF_FieldCommEx.stdout

run_ESMF_FieldCommEx_uni:
	-@rm -f testlog stdlog.* logfile.* ESMF_FieldCommEx.tmps
	${MPIRUN} -np 1 ESMF_FieldCommEx > ESMF_FieldCommEx.stdout

