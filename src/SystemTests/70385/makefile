# $Id: makefile,v 1.1 2003/02/21 20:57:11 jwolfe Exp $ 

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
LOCDIR	  = src/SystemTests/70385

TESTEXAMPLES_3  = \
	ESMF_SysTest70385  RUN_ESMF_SysTest70385  ESMF_SysTest70385.rm 

TESTEXAMPLES_9  = 

EXAMPLESF       = ESMF_SysTest70385.F90 

EXAMPLESC       = 

test:	ESMF_SysTest70385

include ${ESMF_DIR}/build/${ESMF_ARCH}/base


ESMF_SysTest70385: ESMF_SysTest70385.o  chkopts
	-${F90CXXLD} -o ESMF_SysTest70385 ESMF_SysTest70385.o -lesmf ${F90CXXLIBS} ${MPI_LIB} ${MP_LIB} ${THREAD_LIB} ${PCL_LIB}
	${RM} ESMF_SysTest70385.o

RUN_ESMF_SysTest70385:
	-@rm -f testlog stdlog.* logfile.* ESMF_SysTest70385.tmps
	${MPIRUN} -np 2 ESMF_SysTest70385 > ESMF_SysTest70385.stdout
	#@sort -t, -k2 testlog | cut -d, -f4- > ESMF_SysTest70385.tmp
	#@if (diff ESMF_SysTest70385.tmp ${ESMF_DIR}/${LOCDIR}/output/ESMF_SysTest70385.out) then \
	#echo "PASS SysTest70385 test" > ${ESMC_TESTDIR}/ESMF_SysTest70385.log; else \
	#echo "FAIL SysTest70385 test" > ${ESMC_TESTDIR}/ESMF_SysTest70385.log; fi
	#@rm -f testlog stdlog.* logfile.* ESMF_SysTest70385.tmp optlog

ESMF_SysTest70385.rm:
	echo "ESMF_SysTest70385.rm target called here."


