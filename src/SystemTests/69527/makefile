# $Id: makefile,v 1.1 2003/01/23 17:27:29 nscollins Exp $ 

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
LOCDIR	  = src/SystemTests/69527

TESTEXAMPLES_3  = \
	ESMF_SysTest69527  RUN_ESMF_SysTest69527  ESMF_SysTest69527.rm 

TESTEXAMPLES_9  = \
	ESMF_SysTest69527  RUN_ESMF_SysTest69527UNI  ESMF_SysTest69527.rm 

EXAMPLESF       = ESMF_SysTest69527.F90 

EXAMPLESC       = 

test:	ESMF_SysTest69527

include ${ESMF_DIR}/build/${ESMF_ARCH}/base


ESMF_SysTest69527: ESMF_SysTest69527.o  chkopts
	-${F90CXXLD} -o ESMF_SysTest69527 ESMF_SysTest69527.o -lesmf ${F90CXXLIBS} ${MPI_LIB} ${MP_LIB} ${THREAD_LIB} ${PCL_LIB}
	${RM} ESMF_SysTest69527.o

RUN_ESMF_SysTest69527:
	-@rm -f testlog stdlog.* logfile.* ESMF_SysTest69527.tmps
	${MPIRUN} -np 2 ESMF_SysTest69527 > ESMF_SysTest69527.stdout
	#@sort -t, -k2 testlog | cut -d, -f4- > ESMF_SysTest69527.tmp
	#@if (diff ESMF_SysTest69527.tmp ${ESMF_DIR}/${LOCDIR}/output/ESMF_SysTest69527.out) then \
	#echo "PASS SysTest69527 test" > ${ESMC_TESTDIR}/ESMF_SysTest69527.log; else \
	#echo "FAIL SysTest69527 test" > ${ESMC_TESTDIR}/ESMF_SysTest69527.log; fi
	#@rm -f testlog stdlog.* logfile.* ESMF_SysTest69527.tmp optlog

RUN_ESMF_SysTest69527UNI:
	-@rm -f testlog stdlog.* logfile.* ESMF_SysTest69527.tmps
	${MPIRUN} -np 1 ESMF_SysTest69527 > ESMF_SysTest69527.stdout
	#@sort -t, -k2 testlog | cut -d, -f4- > ESMF_SysTest69527.tmp
	#@if (diff ESMF_SysTest69527.tmp ${ESMF_DIR}/${LOCDIR}/output/ESMF_SysTest69527.outuni) then \
	#echo "PASS SysTest69527 test" > ${ESMC_TESTDIR}/ESMF_SysTest69527.log; else \
	#echo "FAIL SysTest69527 test" > ${ESMC_TESTDIR}/ESMF_SysTest69527.log; fi
	#@rm -f testlog stdlog.* logfile.* ESMF_SysTest69527.tmp optlog

ESMF_SysTest69527.rm:
	echo "ESMF_SysTest69527.rm target called here."


