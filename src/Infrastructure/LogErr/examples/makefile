# $Id: makefile,v 1.1 2003/03/28 23:02:55 nscollins Exp $ 

ALL: 

CFLAGS    =
FFLAGS    =
SOURCEC	  = 
SOURCEF	  =

SOURCEH	  = 
OBJSC	  =  
OBJSF	  = 
MANSEC	  = 
LIBBASE	  = 
LOCDIR	  = src/Infrastructure/LogErr/examples

TESTEXAMPLES_3  = \
	ESMF_TestErr RUN_ESMF_TestErr ESMF_TestErr.rm \
	ESMF_TestErr2 RUN_ESMF_TestErr2 ESMF_TestErr2.rm 

TESTEXAMPLES_9  = \
	ESMF_TestErr RUN_ESMF_TestErrUNI ESMF_TestErr.rm \
	ESMF_TestErr2 RUN_ESMF_TestErr2UNI ESMF_TestErr2.rm 

EXAMPLESF       = ESMF_TestErr.F90  ESMF_TestErr2.F90
EXAMPLESC       = 

include $(ESMF_DIR)/makefile

DIRS	  = 

CLEANDIRS   =
CLEANFILES  = *.o *.mod ESMF_TestErr ESMF_TestErr2
CLOBBERDIRS =


examples: ESMF_TestErr ESMF_TestErr2

ESMF_TestErrCreateDir:
	mkdir -p ${ESMC_TESTDIR}

ESMF_TestErr: ESMF_TestErr.o  chkopts
	-${F90CXXLD} -o ESMF_TestErr ESMF_TestErr.o -lesmf ${F90CXXLIBS} ${MPI_LIB} ${MP_LIB} ${THREAD_LIB} ${PCL_LIB}
	${RM} ESMF_TestErr.o

RUN_ESMF_TestErr:
	-@rm -f testlog stdlog.* logfile.* ESMF_TestErr.tmps
	${MPIRUN} -np 4 ESMF_TestErr > ESMF_TestErr.stdout

RUN_ESMF_TestErrUNI:
	-@rm -f testlog stdlog.* logfile.* ESMF_TestErr.tmps
	${MPIRUN} -np 1 ESMF_TestErr > ESMF_TestErr.stdout

ESMF_TestErr.rm:
	echo "ESMF_TestErr.rm target called here."


ESMF_TestErr2: ESMF_TestErr2.o  chkopts
	-${F90CXXLD} -o ESMF_TestErr2 ESMF_TestErr2.o -lesmf ${F90CXXLIBS} ${MPI_LIB} ${MP_LIB} ${THREAD_LIB} ${PCL_LIB}
	${RM} ESMF_TestErr2.o

RUN_ESMF_TestErr2:
	-@rm -f testlog stdlog.* logfile.* ESMF_TestErr2.tmps
	${MPIRUN} -np 4 ESMF_TestErr2 > ESMF_TestErr2.stdout

RUN_ESMF_TestErr2UNI:
	-@rm -f testlog stdlog.* logfile.* ESMF_TestErr2.tmps
	${MPIRUN} -np 1 ESMF_TestErr2 > ESMF_TestErr2.stdout

ESMF_TestErr2.rm:
	echo "ESMF_TestErr2.rm target called here."

