# $Id: makefile,v 1.1 2003/03/10 21:54:20 cdeluca Exp $ 

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
LOCDIR	  = src/Infrastructure/Fields/examples

TESTEXAMPLES_3  = \
	ESMF_FieldCreateEx RUN_ESMF_FieldCreateEx ESMF_FieldCreateEx.rm \
	ESMC_FieldCreateEx RUN_ESMC_FieldCreateEx ESMC_FieldCreateEx.rm \
	ESMF_BundleCreateEx RUN_ESMF_BundleCreateEx ESMF_BundleCreateEx.rm 

TESTEXAMPLES_9  = \
	ESMF_FieldCreateEx RUN_ESMF_FieldCreateExUNI ESMF_FieldCreateEx.rm \
	ESMC_FieldCreateEx RUN_ESMC_FieldCreateExUNI ESMC_FieldCreateEx.rm \
	ESMF_BundleCreateEx RUN_ESMF_BundleCreateExUNI ESMF_BundleCreateEx.rm 

EXAMPLESF       = ESMF_FieldCreateEx.F90 ESMF_BundleCreateEx.F90
EXAMPLESC       = ESMC_FieldCreateEx.C

include $(ESMF_DIR)/makefile

DIRS	  = 


examples: ESMF_FieldCreateEx ESMF_BundleCreateEx ESMC_FieldCreateEx

ESMF_FieldCreateDir:
	mkdir -p ${ESMC_TESTDIR}

ESMF_FieldCreateEx: ESMF_FieldCreateEx.o  chkopts
	-${F90CXXLD} -o ESMF_FieldCreateEx ESMF_FieldCreateEx.o -lesmf ${F90CXXLIBS} ${MPI_LIB} ${MP_LIB} ${THREAD_LIB} ${PCL_LIB}
	${RM} ESMF_FieldCreateEx.o

RUN_ESMF_FieldCreateEx:
	-@rm -f testlog stdlog.* logfile.* ESMF_FieldCreateEx.tmps
	${MPIRUN} -np 4 ESMF_FieldCreateEx > ESMF_FieldCreateEx.stdout
	#@sort -t, -k2 testlog | cut -d, -f4- > ESMF_FieldCreateEx.tmp
	#@if (diff ESMF_FieldCreateEx.tmp ${ESMF_DIR}/${LOCDIR}/output/ESMF_FieldCreateEx.out) then \
	#echo "PASS FieldCreateEx test" > ${ESMC_TESTDIR}/ESMF_FieldCreateEx.log; else \
	#echo "FAIL FieldCreateEx test" > ${ESMC_TESTDIR}/ESMF_FieldCreateEx.log; fi
	#@rm -f testlog stdlog.* logfile.* ESMF_FieldCreateEx.tmp optlog

RUN_ESMF_FieldCreateExUNI:
	-@rm -f testlog stdlog.* logfile.* ESMF_FieldCreateEx.tmps
	${MPIRUN} -np 1 ESMF_FieldCreateEx > ESMF_FieldCreateEx.stdout
	#@sort -t, -k2 testlog | cut -d, -f4- > ESMF_FieldCreateEx.tmp
	#@if (diff ESMF_FieldCreateEx.tmp ${ESMF_DIR}/${LOCDIR}/output/ESMF_FieldCreateEx.outuni) then \
	#echo "PASS FieldCreateEx test" > ${ESMC_TESTDIR}/ESMF_FieldCreateEx.log; else \
	#echo "FAIL FieldCreateEx test" > ${ESMC_TESTDIR}/ESMF_FieldCreateEx.log; fi
	#@rm -f testlog stdlog.* logfile.* ESMF_FieldCreateEx.tmp optlog

ESMF_FieldCreateEx.rm:
	echo "ESMF_FieldCreateEx.rm target called here."


ESMF_BundleCreateDir:
	mkdir -p ${ESMC_TESTDIR}

ESMF_BundleCreateEx: ESMF_BundleCreateEx.o  chkopts
	-${F90CXXLD} -o ESMF_BundleCreateEx ESMF_BundleCreateEx.o -lesmf ${F90CXXLIBS} ${MPI_LIB} ${MP_LIB} ${THREAD_LIB} ${PCL_LIB}
	${RM} ESMF_BundleCreateEx.o

RUN_ESMF_BundleCreateEx:
	-@rm -f testlog stdlog.* logfile.* ESMF_BundleCreateEx.tmps
	${MPIRUN} -np 4 ESMF_BundleCreateEx > ESMF_BundleCreateEx.stdout
	#@sort -t, -k2 testlog | cut -d, -f4- > ESMF_BundleCreateEx.tmp
	#@if (diff ESMF_BundleCreateEx.tmp ${ESMF_DIR}/${LOCDIR}/output/ESMF_BundleCreateEx.out) then \
	#echo "PASS BundleCreateEx test" > ${ESMC_TESTDIR}/ESMF_BundleCreateEx.log; else \
	#echo "FAIL BundleCreateEx test" > ${ESMC_TESTDIR}/ESMF_BundleCreateEx.log; fi
	#@rm -f testlog stdlog.* logfile.* ESMF_BundleCreateEx.tmp optlog

RUN_ESMF_BundleCreateExUNI:
	-@rm -f testlog stdlog.* logfile.* ESMF_BundleCreateEx.tmps
	${MPIRUN} -np 1 ESMF_BundleCreateEx > ESMF_BundleCreateEx.stdout
	#@sort -t, -k2 testlog | cut -d, -f4- > ESMF_BundleCreateEx.tmp
	#@if (diff ESMF_BundleCreateEx.tmp ${ESMF_DIR}/${LOCDIR}/output/ESMF_BundleCreateEx.outuni) then \
	#echo "PASS BundleCreateEx test" > ${ESMC_TESTDIR}/ESMF_BundleCreateEx.log; else \
	#echo "FAIL BundleCreateEx test" > ${ESMC_TESTDIR}/ESMF_BundleCreateEx.log; fi
	#@rm -f testlog stdlog.* logfile.* ESMF_BundleCreateEx.tmp optlog

ESMF_BundleCreateEx.rm:
	echo "ESMF_BundleCreateEx.rm target called here."


ESMC_FieldCreateEx: ESMC_FieldCreateEx.o  chkopts
	${CXXF90LD} -o ESMC_FieldCreateEx ESMC_FieldCreateEx.o ${ESMC_TIME_LIB} -lesmf ${CXXF90LIBS} ${MPI_LIB} ${MP_LIB} ${THREAD_LIB} ${PCL_LIB}
	${RM} ESMC_FieldCreateEx.o

RUN_ESMC_FieldCreateEx:
	-@rm -f testlog stdlog.* logfile.* ESMC_FieldCreateEx.tmps
	${MPIRUN} -np 4 ESMC_FieldCreateEx > ESMC_FieldCreateEx.stdout
	#@sort -t, -k2 testlog | cut -d, -f4- > ESMC_FieldCreateEx.tmp
	#@if (diff ESMC_FieldCreateEx.tmp ${ESMC_DIR}/${LOCDIR}/output/ESMC_FieldCreateEx.out) then \
	#echo "PASS FieldCreateEx test" > ${ESMC_TESTDIR}/ESMC_FieldCreateEx.log; else \
	#echo "FAIL FieldCreateEx test" > ${ESMC_TESTDIR}/ESMC_FieldCreateEx.log; fi
	#@rm -f testlog stdlog.* logfile.* ESMC_FieldCreateEx.tmp optlog

RUN_ESMC_FieldCreateExUNI:
	-@rm -f testlog stdlog.* logfile.* ESMC_FieldCreateEx.tmps
	${MPIRUN} -np 1 ESMC_FieldCreateEx > ESMC_FieldCreateEx.stdout
	#@sort -t, -k2 testlog | cut -d, -f4- > ESMC_FieldCreateEx.tmp
	#@if (diff ESMC_FieldCreateEx.tmp ${ESMC_DIR}/${LOCDIR}/output/ESMC_FieldCreateEx.outuni) then \
	#echo "PASS FieldCreateEx test" > ${ESMC_TESTDIR}/ESMC_FieldCreateEx.log; else \
	#echo "FAIL FieldCreateEx test" > ${ESMC_TESTDIR}/ESMC_FieldCreateEx.log; fi
	#@rm -f testlog stdlog.* logfile.* ESMC_FieldCreateEx.tmp optlog

ESMC_FieldCreateEx.rm:
	echo "ESMC_FieldCreateEx.rm target called here."

