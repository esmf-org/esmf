# $Id: makefile,v 1.3 2003/03/14 15:25:11 nscollins Exp $

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
	ESMF_RouteEx RUN_ESMF_RouteEx ESMF_RouteEx.rm \
	ESMC_RouteEx RUN_ESMC_RouteEx ESMC_RouteEx.rm

TESTEXAMPLES_9  = \
	ESMF_RouteEx RUN_ESMF_RouteExUNI ESMF_RouteEx.rm \
	ESMC_RouteEx RUN_ESMC_RouteExUNI ESMC_RouteEx.rm

EXAMPLESF       = ESMF_RouteEx.F90
EXAMPLESC       = ESMC_RouteEx.C

include ${ESMF_DIR}/makefile

ESMF_RouteDir:
	mkdir -p ${ESMC_TESTDIR}

ESMF_RouteEx: ESMF_RouteEx.o  chkopts
	${F90CXXLD} -g -o ESMF_RouteEx ESMF_RouteEx.o -lesmf ${F90CXXLIBS} ${MPI_LIB} ${MP_LIB} ${THREAD_LIB} ${PCL_LIB} 
	${RM} ESMF_RouteEx.o

RUN_ESMF_RouteEx:
	-@rm -f testlog stdlog.* logfile.* ESMF_RouteEx.tmps
	${MPIRUN} -np 4 ESMF_RouteEx > ESMF_RouteEx.stdout

RUN_ESMF_RouteExUNI:
	-@rm -f testlog stdlog.* logfile.* ESMF_RouteEx.tmps
	${MPIRUN} -np 1 ESMF_RouteEx > ESMF_RouteEx.stdout

ESMF_RouteEx.rm:
	echo "ESMF_RouteEx.rm target called here."

ESMC_RouteEx: ESMC_RouteEx.o  chkopts
	${CXXF90LD} -g -o ESMC_RouteEx ESMC_RouteEx.o -lesmf ${CXXF90LIBS} ${MPI_LIB} ${MP_LIB} ${THREAD_LIB} ${PCL_LIB} 
	${RM} ESMC_RouteEx.o

RUN_ESMC_RouteEx:
	-@rm -f testlog stdlog.* logfile.* ESMC_RouteEx.tmps
	${MPIRUN} -np 4 ESMC_RouteEx > ESMC_RouteEx.stdout

RUN_ESMC_RouteExUNI:
	-@rm -f testlog stdlog.* logfile.* ESMC_RouteEx.tmps
	${MPIRUN} -np 1 ESMC_RouteEx > ESMC_RouteEx.stdout

ESMC_RouteEx.rm:
	echo "ESMC_RouteEx.rm target called here."

