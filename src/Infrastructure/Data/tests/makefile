# $Id: makefile,v 1.2 2003/01/10 19:00:14 nscollins Exp $ 

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
DIRS	  = 
LOCDIR	  = src/Infrastructure/Data/tests

TESTEXAMPLES_3  = \
	ESMF_ArrayTest RUN_ESMF_ArrayTest ESMF_ArrayTest.rm

TESTEXAMPLES_9  = \
	ESMF_ArrayTest RUN_ESMF_ArrayTestUNI ESMF_ArrayTest.rm

EXAMPLESF       = ESMF_ArrayTest.F90

test:	ESMF_ArrayTest

include ${ESMF_DIR}/build/${ESMF_ARCH}/base

ESMF_ArrayCreateDir:
	mkdir -p ${ESMC_TESTDIR}

ESMF_ArrayTest: ESMF_ArrayTest.o  chkopts
	${F90CXXLD} -g -o ESMF_ArrayTest ESMF_ArrayTest.o -lesmf ${F90CXXLIBS} ${MPI_LIB} ${MP_LIB} ${THREAD_LIB} ${PCL_LIB} 
	${RM} ESMF_ArrayTest.o

RUN_ESMF_ArrayTest:
	-@rm -f testlog stdlog.* logfile.* ESMF_ArrayTest.tmps
	${MPIRUN} -np 4 ESMF_ArrayTest > ESMF_ArrayTest.stdout
	#@sort -t, -k2 testlog | cut -d, -f4- > ESMF_ArrayTest.tmp
	#@if (diff ESMF_ArrayTest.tmp ${ESMF_DIR}/${LOCDIR}/output/ESMF_ArrayTest.out) then \
	#echo "PASS ArrayTest test" > ${ESMC_TESTDIR}/ESMF_ArrayTest.log; else \
	#echo "FAIL ArrayTest test" > ${ESMC_TESTDIR}/ESMF_ArrayTest.log; fi
	#@rm -f testlog stdlog.* logfile.* ESMF_ArrayTest.tmp optlog

RUN_ESMF_ArrayTestUNI:
	-@rm -f testlog stdlog.* logfile.* ESMF_ArrayTest.tmps
	${MPIRUN} -np 1 ESMF_ArrayTest > ESMF_ArrayTest.stdout
	#@sort -t, -k2 testlog | cut -d, -f4- > ESMF_ArrayTest.tmp
	#@if (diff ESMF_ArrayTest.tmp ${ESMF_DIR}/${LOCDIR}/output/ESMF_ArrayTest.outuni) then \
	#echo "PASS ArrayTest test" > ${ESMC_TESTDIR}/ESMF_ArrayTest.log; else \
	#echo "FAIL ArrayTest test" > ${ESMC_TESTDIR}/ESMF_ArrayTest.log; fi
	#@rm -f testlog stdlog.* logfile.* ESMF_ArrayTest.tmp optlog

ESMF_ArrayTest.rm:
	echo "ESMF_ArrayTest.rm target called here."

