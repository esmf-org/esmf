# $Id: makefile,v 1.3 2004/05/14 22:34:24 slswift Exp $

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
LOCDIR	  = src/Infrastructure/BundleComm/examples

TESTEXAMPLES_3  = \
	ESMF_BundleCommEx RUN_ESMF_BundleCommEx ESMF_BundleCommEx.rm

TESTEXAMPLES_9  = \
	ESMF_BundleCommEx RUN_ESMF_BundleCommExUNI ESMF_BundleCommEx.rm

EXAMPLESF       = ESMF_BundleCommEx.F90

include ${ESMF_DIR}/makefile

DIRS = 

ESMF_BundleCommDir:
	mkdir -p ${ESMC_TESTDIR}

ESMF_BundleCommEx: ESMF_BundleCommEx.o  chkopts
	${F90CXXLD} -g -o ESMF_BundleCommEx ESMF_BundleCommEx.o -lesmf ${F90CXXLIBS} ${MPI_LIB} ${MP_LIB} ${THREAD_LIB} ${PCL_LIB} ${NETCDF_LIB} $(SL_LINKOPTS)
	${RM} ESMF_BundleCommEx.o

RUN_ESMF_BundleCommEx:
	-@rm -f testlog stdlog.* logfile.* ESMF_BundleCommEx.tmps
	${MPIRUN} -np 4 ESMF_BundleCommEx > ESMF_BundleCommEx.stdout

RUN_ESMF_BundleCommExUNI:
	-@rm -f testlog stdlog.* logfile.* ESMF_BundleCommEx.tmps
	${MPIRUN} -np 1 ESMF_BundleCommEx > ESMF_BundleCommEx.stdout

ESMF_BundleCommEx.rm:
	echo "ESMF_BundleCommEx.rm target called here."

