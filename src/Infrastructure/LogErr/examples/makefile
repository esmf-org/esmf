# $Id: makefile,v 1.2 2003/04/03 20:05:11 nscollins Exp $ 

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

# these should be examples, not tests, but leave like this
# for now.
TESTS_BUILD  = \
	ESMF_TestErr \
	ESMF_TestErr2 \
	ESMF_TestErr3 

TESTS_RUN  = \
	RUN_ESMF_TestErr \
	RUN_ESMF_TestErr2 \
	RUN_ESMF_TestErr3

TESTS_RUN_UNI  = \
	RUN_ESMF_TestErrUNI \
	RUN_ESMF_TestErr2UNI \
	RUN_ESMF_TestErr3UNI

TESTS_RM  = \
	ESMF_TestErr.rm \
	ESMF_TestErr2.rm \
	ESMF_TestErr3.rm 

include $(ESMF_DIR)/makefile

DIRS	  = 

CLEANDIRS   =
CLEANFILES  = *.o *.mod 
CLOBBERDIRS =


examples: ESMF_TestErr ESMF_TestErr2 ESMF_TestErr3

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


ESMF_TestErr3: ESMF_TestErr3.o  chkopts
	-${F90CXXLD} -o ESMF_TestErr3 ESMF_TestErr3.o -lesmf ${F90CXXLIBS} ${MPI_LIB} ${MP_LIB} ${THREAD_LIB} ${PCL_LIB}
	${RM} ESMF_TestErr3.o

RUN_ESMF_TestErr3:
	-@rm -f testlog stdlog.* logfile.* ESMF_TestErr3.tmps
	${MPIRUN} -np 4 ESMF_TestErr3 > ESMF_TestErr3.stdout

RUN_ESMF_TestErr3UNI:
	-@rm -f testlog stdlog.* logfile.* ESMF_TestErr3.tmps
	${MPIRUN} -np 1 ESMF_TestErr3 > ESMF_TestErr3.stdout

ESMF_TestErr3.rm:
	echo "ESMF_TestErr3.rm target called here."

