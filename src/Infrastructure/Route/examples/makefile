# $Id: makefile,v 1.2 2003/03/05 20:58:56 nscollins Exp $

# makefile template for the examples directory.

ALL:  examples

CFLAGS    =
FFLAGS    =
SOURCEC	  = 
SOURCEF	  =

SOURCEH	  = 
OBJSC	  =  
OBJSF	  = 
MANSEC	  = 
LIBBASE	  = 
DIRS	  = 
LOCDIR	  = src/Infrastructure/Route/examples

TESTEXAMPLES_3  = \
	ESMF_RouteCreateEx RUN_ESMF_RouteCreateEx ESMF_RouteCreateEx.rm \
	ESMC_RouteCreateEx RUN_ESMC_RouteCreateEx ESMC_RouteCreateEx.rm

TESTEXAMPLES_9  = \
	ESMF_RouteCreateEx RUN_ESMF_RouteCreateExUNI ESMF_RouteCreateEx.rm \
	ESMC_RouteCreateEx RUN_ESMC_RouteCreateExUNI ESMC_RouteCreateEx.rm

EXAMPLESF       = ESMF_RouteCreateEx.F90
EXAMPLESC       = ESMC_RouteCreateEx.C

include ${ESMF_DIR}/makefile

ESMF_RouteDir:
	mkdir -p ${ESMC_TESTDIR}

ESMF_RouteCreateEx: ESMF_RouteCreateEx.o  chkopts
	${F90CXXLD} -g -o ESMF_RouteCreateEx ESMF_RouteCreateEx.o -lesmf ${F90CXXLIBS} ${MPI_LIB} ${MP_LIB} ${THREAD_LIB} ${PCL_LIB} 
	${RM} ESMF_RouteCreateEx.o

RUN_ESMF_RouteCreateEx:
	-@rm -f testlog stdlog.* logfile.* ESMF_RouteCreateEx.tmps
	${MPIRUN} -np 4 ESMF_RouteCreateEx > ESMF_RouteCreateEx.stdout

RUN_ESMF_RouteCreateExUNI:
	-@rm -f testlog stdlog.* logfile.* ESMF_RouteCreateEx.tmps
	${MPIRUN} -np 1 ESMF_RouteCreateEx > ESMF_RouteCreateEx.stdout

ESMF_RouteCreateEx.rm:
	echo "ESMF_RouteCreateEx.rm target called here."

ESMC_RouteCreateEx: ESMC_RouteCreateEx.o  chkopts
	${CXXF90LD} -g -o ESMC_RouteCreateEx ESMC_RouteCreateEx.o -lesmf ${CXXF90LIBS} ${MPI_LIB} ${MP_LIB} ${THREAD_LIB} ${PCL_LIB} 
	${RM} ESMC_RouteCreateEx.o

RUN_ESMC_RouteCreateEx:
	-@rm -f testlog stdlog.* logfile.* ESMC_RouteCreateEx.tmps
	${MPIRUN} -np 4 ESMC_RouteCreateEx > ESMC_RouteCreateEx.stdout

RUN_ESMC_RouteCreateExUNI:
	-@rm -f testlog stdlog.* logfile.* ESMC_RouteCreateEx.tmps
	${MPIRUN} -np 1 ESMC_RouteCreateEx > ESMC_RouteCreateEx.stdout

ESMC_RouteCreateEx.rm:
	echo "ESMC_RouteCreateEx.rm target called here."

