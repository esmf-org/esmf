# $Id: makefile,v 1.1 2004/05/05 15:40:26 nscollins Exp $

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
LOCDIR	  = src/Infrastructure/BundleDataMap/examples

TESTEXAMPLES_3  = \
	ESMF_BundleDataMapEx RUN_ESMF_BundleDataMapEx ESMF_BundleDataMapEx.rm

TESTEXAMPLES_9  = \
	ESMF_BundleDataMapEx RUN_ESMF_BundleDataMapExUNI ESMF_BundleDataMapEx.rm

EXAMPLESF       = ESMF_BundleDataMapEx.F90

include ${ESMF_DIR}/makefile

DIRS = 

ESMF_BundleDataMapDir:
	mkdir -p ${ESMC_TESTDIR}

ESMF_BundleDataMapEx: ESMF_BundleDataMapEx.o  chkopts
	${F90CXXLD} -g -o ESMF_BundleDataMapEx ESMF_BundleDataMapEx.o -lesmf ${F90CXXLIBS} ${MPI_LIB} ${MP_LIB} ${THREAD_LIB} ${PCL_LIB} $(SL_LINKOPTS)
	${RM} ESMF_BundleDataMapEx.o

RUN_ESMF_BundleDataMapEx:
	-@rm -f testlog stdlog.* logfile.* ESMF_BundleDataMapEx.tmps
	${MPIRUN} -np 4 ESMF_BundleDataMapEx > ESMF_BundleDataMapEx.stdout

RUN_ESMF_BundleDataMapExUNI:
	-@rm -f testlog stdlog.* logfile.* ESMF_BundleDataMapEx.tmps
	${MPIRUN} -np 1 ESMF_BundleDataMapEx > ESMF_BundleDataMapEx.stdout

ESMF_BundleDataMapEx.rm:
	echo "ESMF_BundleDataMapEx.rm target called here."

