# $Id: makefile,v 1.3 2004/05/14 22:34:25 slswift Exp $

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

TESTEXAMPLES_3  = \
	ESMF_FieldCommEx RUN_ESMF_FieldCommEx ESMF_FieldCommEx.rm

TESTEXAMPLES_9  = \
	ESMF_FieldCommEx RUN_ESMF_FieldCommExUNI ESMF_FieldCommEx.rm

EXAMPLESF       = ESMF_FieldCommEx.F90

include ${ESMF_DIR}/makefile

DIRS = 

ESMF_FieldCommDir:
	mkdir -p ${ESMC_TESTDIR}

ESMF_FieldCommEx: ESMF_FieldCommEx.o  chkopts
	${F90CXXLD} -g -o ESMF_FieldCommEx ESMF_FieldCommEx.o -lesmf ${F90CXXLIBS} ${MPI_LIB} ${MP_LIB} ${THREAD_LIB} ${PCL_LIB} ${NETCDF_LIB} $(SL_LINKOPTS)
	${RM} ESMF_FieldCommEx.o

RUN_ESMF_FieldCommEx:
	-@rm -f testlog stdlog.* logfile.* ESMF_FieldCommEx.tmps
	${MPIRUN} -np 4 ESMF_FieldCommEx > ESMF_FieldCommEx.stdout

RUN_ESMF_FieldCommExUNI:
	-@rm -f testlog stdlog.* logfile.* ESMF_FieldCommEx.tmps
	${MPIRUN} -np 1 ESMF_FieldCommEx > ESMF_FieldCommEx.stdout

ESMF_FieldCommEx.rm:
	echo "ESMF_FieldCommEx.rm target called here."

