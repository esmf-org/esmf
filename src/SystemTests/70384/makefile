# $Id: makefile,v 1.1 2003/02/13 21:12:45 nscollins Exp $ 

ALL:  test

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
LOCDIR	  = src/SystemTests/70384

TESTEXAMPLES_3  = \
	ESMF_SysTest70384  RUN_ESMF_SysTest70384  ESMF_SysTest70384.rm

TESTEXAMPLES_9  = \
	ESMF_SysTest70384  RUN_ESMF_SysTest70384UNI  ESMF_SysTest70384.rm 

EXAMPLESF       = ESMF_SysTest70384.F90 

EXAMPLESC       = 

test:	ESMF_SysTest70384 

include ${ESMF_DIR}/build/${ESMF_ARCH}/base

ESMF_SysTest70384Dir:
	mkdir -p ${ESMC_TESTDIR}

ESMF_SysTest70384: ESMF_SysTest70384.o  chkopts
	-${F90CXXLD} -o ESMF_SysTest70384 ESMF_SysTest70384.o -lesmf ${F90CXXLIBS} ${MPI_LIB} ${MP_LIB} ${THREAD_LIB} ${PCL_LIB}
	${RM} ESMF_SysTest70384.o

RUN_ESMF_SysTest70384:
	-@rm -f testlog stdlog.* logfile.* ESMF_SysTest70384.tmps
	${MPIRUN} -np 4 ESMF_SysTest70384 > ESMF_SysTest70384.stdout
	#@sort -t, -k2 testlog | cut -d, -f4- > ESMF_SysTest70384.tmp
	#@if (diff ESMF_SysTest70384.tmp ${ESMF_DIR}/${LOCDIR}/output/ESMF_SysTest70384.out) then \
	#echo "PASS SysTest70384 test" > ${ESMC_TESTDIR}/ESMF_SysTest70384.log; else \
	#echo "FAIL SysTest70384 test" > ${ESMC_TESTDIR}/ESMF_SysTest70384.log; fi
	#@rm -f testlog stdlog.* logfile.* ESMF_SysTest70384.tmp optlog

RUN_ESMF_SysTest70384UNI:
	-@rm -f testlog stdlog.* logfile.* ESMF_SysTest70384.tmps
	${MPIRUN} -np 1 ESMF_SysTest70384 > ESMF_SysTest70384.stdout
	#@sort -t, -k2 testlog | cut -d, -f4- > ESMF_SysTest70384.tmp
	#@if (diff ESMF_SysTest70384.tmp ${ESMF_DIR}/${LOCDIR}/output/ESMF_SysTest70384.outuni) then \
	#echo "PASS SysTest70384 test" > ${ESMC_TESTDIR}/ESMF_SysTest70384.log; else \
	#echo "FAIL SysTest70384 test" > ${ESMC_TESTDIR}/ESMF_SysTest70384.log; fi
	#@rm -f testlog stdlog.* logfile.* ESMF_SysTest70384.tmp optlog

ESMF_SysTest70384.rm:
	echo "ESMF_SysTest70384.rm target called here."



