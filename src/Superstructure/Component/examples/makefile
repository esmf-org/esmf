# $Id: makefile,v 1.3 2003/01/10 21:45:32 nscollins Exp $ 

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
	ESMF_CompCreateEx RUN_ESMF_CompCreateEx ESMF_CompCreateEx.rm

TESTEXAMPLES_9  = \
	ESMF_CompCreateEx RUN_ESMF_CompCreateExUNI ESMF_CompCreateEx.rm

EXAMPLESF       = ESMF_CompCreateEx.F90

test:  ESMF_CompCreateEx

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

