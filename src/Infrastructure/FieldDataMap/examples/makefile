# $Id: makefile,v 1.1 2004/05/04 13:53:27 nscollins Exp $

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
LOCDIR	  = src/Infrastructure/FieldDataMap/examples

TESTEXAMPLES_3  = \
	ESMF_FieldDataMapEx RUN_ESMF_FieldDataMapEx ESMF_FieldDataMapEx.rm

TESTEXAMPLES_9  = \
	ESMF_FieldDataMapEx RUN_ESMF_FieldDataMapExUNI ESMF_FieldDataMapEx.rm

EXAMPLESF       = ESMF_FieldDataMapEx.F90

include ${ESMF_DIR}/makefile

DIRS = 

ESMF_FieldDataMapDir:
	mkdir -p ${ESMC_TESTDIR}

ESMF_FieldDataMapEx: ESMF_FieldDataMapEx.o  chkopts
	${F90CXXLD} -g -o ESMF_FieldDataMapEx ESMF_FieldDataMapEx.o -lesmf ${F90CXXLIBS} ${MPI_LIB} ${MP_LIB} ${THREAD_LIB} ${PCL_LIB} $(SL_LINKOPTS)
	${RM} ESMF_FieldDataMapEx.o

RUN_ESMF_FieldDataMapEx:
	-@rm -f testlog stdlog.* logfile.* ESMF_FieldDataMapEx.tmps
	${MPIRUN} -np 4 ESMF_FieldDataMapEx > ESMF_FieldDataMapEx.stdout

RUN_ESMF_FieldDataMapExUNI:
	-@rm -f testlog stdlog.* logfile.* ESMF_FieldDataMapEx.tmps
	${MPIRUN} -np 1 ESMF_FieldDataMapEx > ESMF_FieldDataMapEx.stdout

ESMF_FieldDataMapEx.rm:
	echo "ESMF_FieldDataMapEx.rm target called here."

