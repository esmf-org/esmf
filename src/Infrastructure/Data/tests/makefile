# $Id: makefile,v 1.8 2003/02/21 18:32:31 nscollins Exp $ 

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
	ESMF_ArrayF90Ptr RUN_ESMF_ArrayF90Ptr ESMF_ArrayF90Ptr.rm \
	ESMF_ArrayBasic RUN_ESMF_ArrayBasic ESMF_ArrayBasic.rm

TESTEXAMPLES_9  = \
	ESMF_ArrayTest RUN_ESMF_ArrayTestUNI ESMF_ArrayTest.rm \
	ESMF_ArrayF90Ptr RUN_ESMF_ArrayF90PtrUNI ESMF_ArrayF90Ptr.rm \
	ESMF_ArrayBasic RUN_ESMF_ArrayBasicUNI ESMF_ArrayBasic.rm

TESTSF       = ESMF_ArrayTest.F90 ESMF_ArrayBasic.F90 ESMF_ArrayF90Ptr.F90

test tests: chkdir_tests ESMF_ArrayF90Ptr ESMF_ArrayBasic ESMF_ArrayTest

run: 
	ESMF_ArrayF90Ptr 
	ESMF_ArrayBasic 
	ESMF_ArrayTest

include ${ESMF_DIR}/build/${ESMF_ARCH}/base


##

ESMF_ArrayF90Ptr: ESMF_ArrayF90Ptr.o  
	${F90CXXLD} -g -o ESMF_ArrayF90Ptr ESMF_ArrayF90Ptr.o -lesmf ${F90CXXLIBS} ${MPI_LIB} ${MP_LIB} ${THREAD_LIB} ${PCL_LIB} 
	${RM} ESMF_ArrayF90Ptr.o

RUN_ESMF_ArrayF90Ptr:
	-@rm -f testlog stdlog.* logfile.* ESMF_ArrayF90Ptr.tmps
	${MPIRUN} -np 4 ESMF_ArrayF90Ptr > ESMF_ArrayF90Ptr.stdout

RUN_ESMF_ArrayF90PtrUNI:
	-@rm -f testlog stdlog.* logfile.* ESMF_ArrayF90Ptr.tmps
	${MPIRUN} -np 1 ESMF_ArrayF90Ptr > ESMF_ArrayF90Ptr.stdout

ESMF_ArrayF90Ptr.rm:
	-@echo "ESMF_ArrayF90Ptr.rm target called here."
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

