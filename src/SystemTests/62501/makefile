# $Id: makefile,v 1.1 2003/01/23 17:27:28 nscollins Exp $ 

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
LOCDIR	  = src/SystemTests/62501

TESTEXAMPLES_3  = \
	ESMF_SysTest62501  RUN_ESMF_SysTest62501  ESMF_SysTest62501.rm

TESTEXAMPLES_9  = \
	ESMF_SysTest62501  RUN_ESMF_SysTest62501UNI  ESMF_SysTest62501.rm 

EXAMPLESF       = ESMF_SysTest62501.F90 

EXAMPLESC       = 

test:	ESMF_SysTest62501 

include ${ESMF_DIR}/build/${ESMF_ARCH}/base

ESMF_SysTest62501Dir:
	mkdir -p ${ESMC_TESTDIR}

ESMF_SysTest62501: ESMF_SysTest62501.o  chkopts
	-${F90CXXLD} -o ESMF_SysTest62501 ESMF_SysTest62501.o -lesmf ${F90CXXLIBS} ${MPI_LIB} ${MP_LIB} ${THREAD_LIB} ${PCL_LIB}
	${RM} ESMF_SysTest62501.o

RUN_ESMF_SysTest62501:
	-@rm -f testlog stdlog.* logfile.* ESMF_SysTest62501.tmps
	${MPIRUN} -np 4 ESMF_SysTest62501 > ESMF_SysTest62501.stdout
	#@sort -t, -k2 testlog | cut -d, -f4- > ESMF_SysTest62501.tmp
	#@if (diff ESMF_SysTest62501.tmp ${ESMF_DIR}/${LOCDIR}/output/ESMF_SysTest62501.out) then \
	#echo "PASS SysTest62501 test" > ${ESMC_TESTDIR}/ESMF_SysTest62501.log; else \
	#echo "FAIL SysTest62501 test" > ${ESMC_TESTDIR}/ESMF_SysTest62501.log; fi
	#@rm -f testlog stdlog.* logfile.* ESMF_SysTest62501.tmp optlog

RUN_ESMF_SysTest62501UNI:
	-@rm -f testlog stdlog.* logfile.* ESMF_SysTest62501.tmps
	${MPIRUN} -np 1 ESMF_SysTest62501 > ESMF_SysTest62501.stdout
	#@sort -t, -k2 testlog | cut -d, -f4- > ESMF_SysTest62501.tmp
	#@if (diff ESMF_SysTest62501.tmp ${ESMF_DIR}/${LOCDIR}/output/ESMF_SysTest62501.outuni) then \
	#echo "PASS SysTest62501 test" > ${ESMC_TESTDIR}/ESMF_SysTest62501.log; else \
	#echo "FAIL SysTest62501 test" > ${ESMC_TESTDIR}/ESMF_SysTest62501.log; fi
	#@rm -f testlog stdlog.* logfile.* ESMF_SysTest62501.tmp optlog

ESMF_SysTest62501.rm:
	echo "ESMF_SysTest62501.rm target called here."



