# $Id: makefile,v 1.2 2004/05/14 22:34:24 slswift Exp $

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
LOCDIR	  = src/Infrastructure/ArrayDataMap/examples

TESTEXAMPLES_3  = \
	ESMF_ArrayDataMapEx RUN_ESMF_ArrayDataMapEx ESMF_ArrayDataMapEx.rm

TESTEXAMPLES_9  = \
	ESMF_ArrayDataMapEx RUN_ESMF_ArrayDataMapExUNI ESMF_ArrayDataMapEx.rm

EXAMPLESF       = ESMF_ArrayDataMapEx.F90

include ${ESMF_DIR}/makefile

DIRS = 

ESMF_ArrayDataMapDir:
	mkdir -p ${ESMC_TESTDIR}

ESMF_ArrayDataMapEx: ESMF_ArrayDataMapEx.o  chkopts
	${F90CXXLD} -g -o ESMF_ArrayDataMapEx ESMF_ArrayDataMapEx.o -lesmf ${F90CXXLIBS} ${MPI_LIB} ${MP_LIB} ${THREAD_LIB} ${PCL_LIB} ${NETCDF_LIB} $(SL_LINKOPTS)
	${RM} ESMF_ArrayDataMapEx.o

RUN_ESMF_ArrayDataMapEx:
	-@rm -f testlog stdlog.* logfile.* ESMF_ArrayDataMapEx.tmps
	${MPIRUN} -np 4 ESMF_ArrayDataMapEx > ESMF_ArrayDataMapEx.stdout

RUN_ESMF_ArrayDataMapExUNI:
	-@rm -f testlog stdlog.* logfile.* ESMF_ArrayDataMapEx.tmps
	${MPIRUN} -np 1 ESMF_ArrayDataMapEx > ESMF_ArrayDataMapEx.stdout

ESMF_ArrayDataMapEx.rm:
	echo "ESMF_ArrayDataMapEx.rm target called here."

