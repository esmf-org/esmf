# $Id: makefile,v 1.4 2004/06/11 19:04:44 svasquez Exp $

# makefile template for the examples directory.

ALL:   build_examples

CFLAGS    =
FFLAGS    =
SOURCEC	  = 
SOURCEF	  =

SOURCEH	  = 
OBJSC	  =  
OBJSF	  = 
MANSEC	  = 
LIBBASE	  = 
LOCDIR	  = src/Superstructure/ESMFMod/examples

TESTEXAMPLES_3  = \
	ESMF_BaseEx RUN_ESMF_BaseEx ESMF_BaseEx.rm

TESTEXAMPLES_9  = \
	ESMF_BaseEx RUN_ESMF_BaseExUNI ESMF_BaseEx.rm

EXAMPLESF       = ESMF_BaseEx.F90

include ${ESMF_DIR}/makefile

DIRS = 

ESMF_BaseDir:
	mkdir -p ${ESMC_TESTDIR}

ESMF_BaseEx: ESMF_BaseEx.o  chkopts
	${F90CXXLD} -g -o ESMF_BaseEx ESMF_BaseEx.o -lesmf ${F90CXXLIBS} ${MPI_LIB} ${MP_LIB} ${THREAD_LIB} ${PCL_LIB} ${NETCDF_LIB} $(SL_LINKOPTS)
	${RM} ESMF_BaseEx.o

RUN_ESMF_BaseEx:
	-@rm -f testlog stdlog.* logfile.* ESMF_BaseEx.tmps
	-${MPIRUN} -np 4 ESMF_BaseEx > ESMF_BaseEx.stdout

RUN_ESMF_BaseExUNI:
	-@rm -f testlog stdlog.* logfile.* ESMF_BaseEx.tmps
	-${MPIRUN} -np 1 ESMF_BaseEx > ESMF_BaseEx.stdout

ESMF_BaseEx.rm:
	echo "ESMF_BaseEx.rm target called here."

