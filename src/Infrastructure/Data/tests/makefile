# $Id: makefile,v 1.5 2003/02/13 22:02:58 nscollins Exp $ 

ALL:  tests

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
LOCDIR	  = src/Infrastructure/Data/tests


TESTEXAMPLES_3  = \
	ESMF_ArrayTest RUN_ESMF_ArrayTest ESMF_ArrayTest.rm \
	ESMF_ArrayBasic RUN_ESMF_ArrayBasic ESMF_ArrayBasic.rm

TESTEXAMPLES_9  = \
	ESMF_ArrayTest RUN_ESMF_ArrayTestUNI ESMF_ArrayTest.rm \
	ESMF_ArrayBasic RUN_ESMF_ArrayBasicUNI ESMF_ArrayBasic.rm

TESTSF       = ESMF_ArrayTest.F90 ESMF_ArrayBasic.F90

test tests: chkdir_tests ESMF_ArrayTest ESMF_ArrayBasic

include ${ESMF_DIR}/build/${ESMF_ARCH}/base


##

ESMF_ArrayTest: ESMF_ArrayTest.o  
	${F90CXXLD} -g -o ESMF_ArrayTest ESMF_ArrayTest.o -lesmf ${F90CXXLIBS} ${MPI_LIB} ${MP_LIB} ${THREAD_LIB} ${PCL_LIB} 
	${RM} ESMF_ArrayTest.o

RUN_ESMF_ArrayTest:
	-@rm -f testlog stdlog.* logfile.* ESMF_ArrayTest.tmps
	${MPIRUN} -np 4 ESMF_ArrayTest > ESMF_ArrayTest.stdout

RUN_ESMF_ArrayTestUNI:
	-@rm -f testlog stdlog.* logfile.* ESMF_ArrayTest.tmps
	${MPIRUN} -np 1 ESMF_ArrayTest > ESMF_ArrayTest.stdout

ESMF_ArrayTest.rm:
	-@echo "ESMF_ArrayTest.rm target called here."
##

##
ESMF_ArrayBasic: ESMF_ArrayBasic.o  
	${F90CXXLD} -g -o ESMF_ArrayBasic ESMF_ArrayBasic.o -lesmf ${F90CXXLIBS} ${MPI_LIB} ${MP_LIB} ${THREAD_LIB} ${PCL_LIB} 
	${RM} ESMF_ArrayBasic.o

RUN_ESMF_ArrayBasic:
	-@rm -f testlog stdlog.* logfile.* ESMF_ArrayBasic.tmps
	${MPIRUN} -np 4 ESMF_ArrayBasic > ESMF_ArrayBasic.stdout

RUN_ESMF_ArrayBasicUNI:
	-@rm -f testlog stdlog.* logfile.* ESMF_ArrayBasic.tmps
	${MPIRUN} -np 1 ESMF_ArrayBasic > ESMF_ArrayBasic.stdout

ESMF_ArrayBasic.rm:
	-@echo "ESMF_ArrayBasic.rm target called here."
##

