# $Id: makefile,v 1.1 2002/12/13 16:18:03 nscollins Exp $ 

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
	ESMF_ArrayCreateEx RUN_ESMF_ArrayCreateEx ESMF_ArrayCreateEx.rm

TESTEXAMPLES_9  = \
	ESMF_ArrayCreateEx RUN_ESMF_ArrayCreateExUNI ESMF_ArrayCreateEx.rm

EXAMPLESF       = ESMF_ArrayCreateEx.F90

include ${ESMF_DIR}/build/${ESMF_ARCH}/base

ESMF_ArrayCreateDir:
	mkdir -p ${ESMC_TESTDIR}

ESMF_ArrayCreateEx: ESMF_ArrayCreateEx.o  chkopts
	${F90CXXLD} -g -o ESMF_ArrayCreateEx ESMF_ArrayCreateEx.o -lesmf ${F90CXXLIBS} ${MPI_LIB} ${MP_LIB} ${THREAD_LIB} ${PCL_LIB} 
	${RM} ESMF_ArrayCreateEx.o

RUN_ESMF_ArrayCreateEx:
	-@rm -f testlog stdlog.* logfile.* ESMF_ArrayCreateEx.tmps
	${MPIRUN} -np 4 ESMF_ArrayCreateEx > ESMF_ArrayCreateEx.stdout
	#@sort -t, -k2 testlog | cut -d, -f4- > ESMF_ArrayCreateEx.tmp
	#@if (diff ESMF_ArrayCreateEx.tmp ${ESMF_DIR}/${LOCDIR}/output/ESMF_ArrayCreateEx.out) then \
	#echo "PASS ArrayCreateEx test" > ${ESMC_TESTDIR}/ESMF_ArrayCreateEx.log; else \
	#echo "FAIL ArrayCreateEx test" > ${ESMC_TESTDIR}/ESMF_ArrayCreateEx.log; fi
	#@rm -f testlog stdlog.* logfile.* ESMF_ArrayCreateEx.tmp optlog

RUN_ESMF_ArrayCreateExUNI:
	-@rm -f testlog stdlog.* logfile.* ESMF_ArrayCreateEx.tmps
	${MPIRUN} -np 1 ESMF_ArrayCreateEx > ESMF_ArrayCreateEx.stdout
	#@sort -t, -k2 testlog | cut -d, -f4- > ESMF_ArrayCreateEx.tmp
	#@if (diff ESMF_ArrayCreateEx.tmp ${ESMF_DIR}/${LOCDIR}/output/ESMF_ArrayCreateEx.outuni) then \
	#echo "PASS ArrayCreateEx test" > ${ESMC_TESTDIR}/ESMF_ArrayCreateEx.log; else \
	#echo "FAIL ArrayCreateEx test" > ${ESMC_TESTDIR}/ESMF_ArrayCreateEx.log; fi
	#@rm -f testlog stdlog.* logfile.* ESMF_ArrayCreateEx.tmp optlog

ESMF_ArrayCreateEx.rm:
	echo "ESMF_ArrayCreateEx.rm target called here."

