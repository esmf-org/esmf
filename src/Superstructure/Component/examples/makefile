# $Id: makefile,v 1.4 2003/01/29 23:33:28 nscollins Exp $ 

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
LOCDIR	  = src/Superstructure/Component/examples

TESTEXAMPLES_3  = \
	ESMF_CompCreateEx RUN_ESMF_CompCreateEx ESMF_CompCreateEx.rm \
	ESMF_StateCreateEx RUN_ESMF_StateCreateEx ESMF_StateCreateEx.rm

TESTEXAMPLES_9  = \
	ESMF_CompCreateEx RUN_ESMF_CompCreateExUNI ESMF_CompCreateEx.rm \
	ESMF_StateCreateEx RUN_ESMF_StateCreateExUNI ESMF_StateCreateEx.rm

EXAMPLESF       = ESMF_CompCreateEx.F90 ESMF_StateCreateEx.F90

test:  ESMF_CompCreateEx ESMF_StateCreateEx

include ${ESMF_DIR}/build/${ESMF_ARCH}/base

ESMF_CompCreateDir:
	mkdir -p ${ESMC_TESTDIR}

ESMF_CompCreateEx: ESMF_CompCreateEx.o  chkopts
	${F90CXXLD} -g -o ESMF_CompCreateEx ESMF_CompCreateEx.o -lesmf ${F90CXXLIBS} ${MPI_LIB} ${MP_LIB} ${THREAD_LIB} ${PCL_LIB} 
	${RM} ESMF_CompCreateEx.o

RUN_ESMF_CompCreateEx:
	-@rm -f testlog stdlog.* logfile.* ESMF_CompCreateEx.tmps
	${MPIRUN} -np 4 ESMF_CompCreateEx > ESMF_CompCreateEx.stdout
	#@sort -t, -k2 testlog | cut -d, -f4- > ESMF_CompCreateEx.tmp
	#@if (diff ESMF_CompCreateEx.tmp ${ESMF_DIR}/${LOCDIR}/output/ESMF_CompCreateEx.out) then \
	#echo "PASS CompCreateEx test" > ${ESMC_TESTDIR}/ESMF_CompCreateEx.log; else \
	#echo "FAIL CompCreateEx test" > ${ESMC_TESTDIR}/ESMF_CompCreateEx.log; fi
	#@rm -f testlog stdlog.* logfile.* ESMF_CompCreateEx.tmp optlog

RUN_ESMF_CompCreateExUNI:
	-@rm -f testlog stdlog.* logfile.* ESMF_CompCreateEx.tmps
	${MPIRUN} -np 1 ESMF_CompCreateEx > ESMF_CompCreateEx.stdout
	#@sort -t, -k2 testlog | cut -d, -f4- > ESMF_CompCreateEx.tmp
	#@if (diff ESMF_CompCreateEx.tmp ${ESMF_DIR}/${LOCDIR}/output/ESMF_CompCreateEx.outuni) then \
	#echo "PASS CompCreateEx test" > ${ESMC_TESTDIR}/ESMF_CompCreateEx.log; else \
	#echo "FAIL CompCreateEx test" > ${ESMC_TESTDIR}/ESMF_CompCreateEx.log; fi
	#@rm -f testlog stdlog.* logfile.* ESMF_CompCreateEx.tmp optlog

ESMF_CompCreateEx.rm:
	echo "ESMF_CompCreateEx.rm target called here."

ESMF_StateCreateDir:
	mkdir -p ${ESMC_TESTDIR}

ESMF_StateCreateEx: ESMF_StateCreateEx.o  chkopts
	${F90CXXLD} -g -o ESMF_StateCreateEx ESMF_StateCreateEx.o -lesmf ${F90CXXLIBS} ${MPI_LIB} ${MP_LIB} ${THREAD_LIB} ${PCL_LIB} 
	${RM} ESMF_StateCreateEx.o

RUN_ESMF_StateCreateEx:
	-@rm -f testlog stdlog.* logfile.* ESMF_StateCreateEx.tmps
	${MPIRUN} -np 4 ESMF_StateCreateEx > ESMF_StateCreateEx.stdout
	#@sort -t, -k2 testlog | cut -d, -f4- > ESMF_StateCreateEx.tmp
	#@if (diff ESMF_StateCreateEx.tmp ${ESMF_DIR}/${LOCDIR}/output/ESMF_StateCreateEx.out) then \
	#echo "PASS StateCreateEx test" > ${ESMC_TESTDIR}/ESMF_StateCreateEx.log; else \
	#echo "FAIL StateCreateEx test" > ${ESMC_TESTDIR}/ESMF_StateCreateEx.log; fi
	#@rm -f testlog stdlog.* logfile.* ESMF_StateCreateEx.tmp optlog

RUN_ESMF_StateCreateExUNI:
	-@rm -f testlog stdlog.* logfile.* ESMF_StateCreateEx.tmps
	${MPIRUN} -np 1 ESMF_StateCreateEx > ESMF_StateCreateEx.stdout
	#@sort -t, -k2 testlog | cut -d, -f4- > ESMF_StateCreateEx.tmp
	#@if (diff ESMF_StateCreateEx.tmp ${ESMF_DIR}/${LOCDIR}/output/ESMF_StateCreateEx.outuni) then \
	#echo "PASS StateCreateEx test" > ${ESMC_TESTDIR}/ESMF_StateCreateEx.log; else \
	#echo "FAIL StateCreateEx test" > ${ESMC_TESTDIR}/ESMF_StateCreateEx.log; fi
	#@rm -f testlog stdlog.* logfile.* ESMF_StateCreateEx.tmp optlog

ESMF_StateCreateEx.rm:
	echo "ESMF_StateCreateEx.rm target called here."

