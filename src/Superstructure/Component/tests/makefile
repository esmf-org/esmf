# $Id: makefile,v 1.3 2003/02/11 18:24:00 nscollins Exp $ 

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
LOCDIR	  = src/Superstructure/Component/tests

TESTEXAMPLES_3  = \
	ESMF_CompCreate RUN_ESMF_CompCreate ESMF_CompCreate.rm \
	ESMF_StateCreate RUN_ESMF_StateCreate ESMF_StateCreate.rm

TESTEXAMPLES_9  = \
	ESMF_CompCreate RUN_ESMF_CompCreateUNI ESMF_CompCreate.rm \
	ESMF_StateCreate RUN_ESMF_StateCreateUNI ESMF_StateCreate.rm

TESTSF       = ESMF_CompCreate.F90  ESMF_StateCreate.F90

test tests:  ESMF_CompCreate ESMF_StateCreate

include ${ESMF_DIR}/build/${ESMF_ARCH}/base

ESMF_CompCreateDir:
	mkdir -p ${ESMC_TESTDIR}

ESMF_CompCreate: ESMF_CompCreate.o  chkopts
	${F90CXXLD} -g -o ESMF_CompCreate ESMF_CompCreate.o -lesmf ${F90CXXLIBS} ${MPI_LIB} ${MP_LIB} ${THREAD_LIB} ${PCL_LIB} 
	${RM} ESMF_CompCreate.o

RUN_ESMF_CompCreate:
	-@rm -f testlog stdlog.* logfile.* ESMF_CompCreate.tmps
	${MPIRUN} -np 4 ESMF_CompCreate > ESMF_CompCreate.stdout
	#@sort -t, -k2 testlog | cut -d, -f4- > ESMF_CompCreate.tmp
	#@if (diff ESMF_CompCreate.tmp ${ESMF_DIR}/${LOCDIR}/output/ESMF_CompCreate.out) then \
	#echo "PASS CompCreate test" > ${ESMC_TESTDIR}/ESMF_CompCreate.log; else \
	#echo "FAIL CompCreate test" > ${ESMC_TESTDIR}/ESMF_CompCreate.log; fi
	#@rm -f testlog stdlog.* logfile.* ESMF_CompCreate.tmp optlog

RUN_ESMF_CompCreateUNI:
	-@rm -f testlog stdlog.* logfile.* ESMF_CompCreate.tmps
	${MPIRUN} -np 1 ESMF_CompCreate > ESMF_CompCreate.stdout
	#@sort -t, -k2 testlog | cut -d, -f4- > ESMF_CompCreate.tmp
	#@if (diff ESMF_CompCreate.tmp ${ESMF_DIR}/${LOCDIR}/output/ESMF_CompCreate.outuni) then \
	#echo "PASS CompCreate test" > ${ESMC_TESTDIR}/ESMF_CompCreate.log; else \
	#echo "FAIL CompCreate test" > ${ESMC_TESTDIR}/ESMF_CompCreate.log; fi
	#@rm -f testlog stdlog.* logfile.* ESMF_CompCreate.tmp optlog

ESMF_CompCreate.rm:
	echo "ESMF_CompCreate.rm target called here."


ESMF_StateCreateDir:
	mkdir -p ${ESMC_TESTDIR}

ESMF_StateCreate: ESMF_StateCreate.o  chkopts
	${F90CXXLD} -g -o ESMF_StateCreate ESMF_StateCreate.o -lesmf ${F90CXXLIBS} ${MPI_LIB} ${MP_LIB} ${THREAD_LIB} ${PCL_LIB} 
	${RM} ESMF_StateCreate.o

RUN_ESMF_StateCreate:
	-@rm -f testlog stdlog.* logfile.* ESMF_StateCreate.tmps
	${MPIRUN} -np 4 ESMF_StateCreate > ESMF_StateCreate.stdout
	#@sort -t, -k2 testlog | cut -d, -f4- > ESMF_StateCreate.tmp
	#@if (diff ESMF_StateCreate.tmp ${ESMF_DIR}/${LOCDIR}/output/ESMF_StateCreate.out) then \
	#echo "PASS StateCreate test" > ${ESMC_TESTDIR}/ESMF_StateCreate.log; else \
	#echo "FAIL StateCreate test" > ${ESMC_TESTDIR}/ESMF_StateCreate.log; fi
	#@rm -f testlog stdlog.* logfile.* ESMF_StateCreate.tmp optlog

RUN_ESMF_StateCreateUNI:
	-@rm -f testlog stdlog.* logfile.* ESMF_StateCreate.tmps
	${MPIRUN} -np 1 ESMF_StateCreate > ESMF_StateCreate.stdout
	#@sort -t, -k2 testlog | cut -d, -f4- > ESMF_StateCreate.tmp
	#@if (diff ESMF_StateCreate.tmp ${ESMF_DIR}/${LOCDIR}/output/ESMF_StateCreate.outuni) then \
	#echo "PASS StateCreate test" > ${ESMC_TESTDIR}/ESMF_StateCreate.log; else \
	#echo "FAIL StateCreate test" > ${ESMC_TESTDIR}/ESMF_StateCreate.log; fi
	#@rm -f testlog stdlog.* logfile.* ESMF_StateCreate.tmp optlog

ESMF_StateCreate.rm:
	echo "ESMF_StateCreate.rm target called here."


