# $Id: makefile,v 1.7 2003/07/28 17:39:14 jwolfe Exp $ 

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
	ESMF_TestErr3 \
	ESMF_TestLog \
	ESMF_TestLog2 \

TESTS_RUN  = \
	RUN_ESMF_TestErr \
	RUN_ESMF_TestErr2 \
	RUN_ESMF_TestErr3 \
	RUN_ESMF_TestLog \
	RUN_ESMF_TestLog2 \

TESTS_RUN_UNI  = \
	RUN_ESMF_TestErrUNI \
	RUN_ESMF_TestErr2UNI \
	RUN_ESMF_TestErr3UNI \
	RUN_ESMF_TestLogUNI \
	RUN_ESMF_TestLog2UNI \

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
	-${RM} ESMF_TestErr.o

RUN_ESMF_TestErr:
	-@rm -f testlog stdlog.* logfile.* ESMF_TestErr.tmps
	-${MPIRUN} -np 4 ESMF_TestErr > ESMF_TestErr.stdout

RUN_ESMF_TestErrUNI:
	-@rm -f testlog stdlog.* logfile.* ESMF_TestErr.tmps
	-${MPIRUN} -np 1 ESMF_TestErr > ESMF_TestErr.stdout

ESMF_TestErr.rm:
	echo "ESMF_TestErr.rm target called here."


ESMF_TestErr2: ESMF_TestErr2.o  chkopts
	-${F90CXXLD} -o ESMF_TestErr2 ESMF_TestErr2.o -lesmf ${F90CXXLIBS} ${MPI_LIB} ${MP_LIB} ${THREAD_LIB} ${PCL_LIB}
	-${RM} ESMF_TestErr2.o

RUN_ESMF_TestErr2:
	-@rm -f testlog stdlog.* logfile.* ESMF_TestErr2.tmps
	-${MPIRUN} -np 4 ESMF_TestErr2 > ESMF_TestErr2.stdout

RUN_ESMF_TestErr2UNI:
	-@rm -f testlog stdlog.* logfile.* ESMF_TestErr2.tmps
	-${MPIRUN} -np 1 ESMF_TestErr2 > ESMF_TestErr2.stdout

ESMF_TestErr2.rm:
	echo "ESMF_TestErr2.rm target called here."


ESMF_TestErr3: ESMF_TestErr3.o  chkopts
	-${F90CXXLD} -o ESMF_TestErr3 ESMF_TestErr3.o -lesmf ${F90CXXLIBS} ${MPI_LIB} ${MP_LIB} ${THREAD_LIB} ${PCL_LIB}
	-${RM} ESMF_TestErr3.o

RUN_ESMF_TestErr3:
	-@rm -f testlog stdlog.* logfile.* ESMF_TestErr3.tmps
	-${MPIRUN} -np 4 ESMF_TestErr3 > ESMF_TestErr3.stdout

RUN_ESMF_TestErr3UNI:
	-@rm -f testlog stdlog.* logfile.* ESMF_TestErr3.tmps
	-${MPIRUN} -np 1 ESMF_TestErr3 > ESMF_TestErr3.stdout

ESMF_TestErr3.rm:
	echo "ESMF_TestErr3.rm target called here."

ESMF_TestLog: ESMF_TestLog.o  chkopts
	-${F90CXXLD} -o ESMF_TestLog ESMF_TestLog.o -lesmf ${F90CXXLIBS} ${MPI_LIB} ${MP_LIB} ${THREAD_LIB} ${PCL_LIB}
	-${RM} ESMF_TestLog.o

RUN_ESMF_TestLog:
	-@rm -f testlog stdlog.* logfile.* ESMF_TestLog.tmps
	-${MPIRUN} -np 4 ESMF_TestLog > ESMF_TestLog.stdout

RUN_ESMF_TestLogUNI:
	-@rm -f testlog stdlog.* logfile.* ESMF_TestLog.tmps
	-${MPIRUN} -np 1 ESMF_TestLog > ESMF_TestLog.stdout

ESMF_TestLog.rm:
	echo "ESMF_TestLog.rm target called here."

ESMF_TestLog2: ESMF_TestLog2.o  chkopts
	-${F90CXXLD} -o ESMF_TestLog2 ESMF_TestLog2.o -lesmf ${F90CXXLIBS} ${MPI_LIB} ${MP_LIB} ${THREAD_LIB} ${PCL_LIB}
	-${RM} ESMF_TestLog2.o

RUN_ESMF_TestLog2:
	-@rm -f testlog stdlog.* logfile.* ESMF_TestLog2.tmps
	-${MPIRUN} -np 4 ESMF_TestLog2 > ESMF_TestLog2.stdout

RUN_ESMF_TestLog2UNI:
	-@rm -f testlog stdlog.* logfile.* ESMF_TestLog2.tmps
	-${MPIRUN} -np 1 ESMF_TestLog2 > ESMF_TestLog2.stdout

ESMF_TestLog2.rm:
	echo "ESMF_TestLog2.rm target called here."

