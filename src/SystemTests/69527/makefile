# $Id: makefile,v 1.2 2003/02/27 17:55:11 flanigan Exp $ 

ALL: system_tests

CFLAGS    =
FFLAGS    =
SOURCEC	  = 
SOURCEF	  =

SOURCEH	  = 
OBJSC	  =  
OBJSF	  = 
MANSEC	  = 
LIBBASE	  = 
LOCDIR	  = src/SystemTests/69527

SYSTEM_TESTS_BUILD  = $(ESMC_TESTDIR)/ESMF_SysTest69527  

SYSTEM_TESTS_RUN    = RUN_ESMF_SysTest69527  

SYSTEM_TESTS_RM     = ESMF_SysTest69527.rm 


include $(ESMF_DIR)/makefile

DIRS = 

$(ESMC_TESTDIR)/ESMF_SysTest69527: ESMF_SysTest69527.o  chkopts
	${F90CXXLD} -o ESMF_SysTest69527 ESMF_SysTest69527.o -lesmf ${F90CXXLIBS} ${MPI_LIB} ${MP_LIB} ${THREAD_LIB} ${PCL_LIB}
	mv -f ESMF_SysText69527 $(ESMC_TESTDIR)
	${RM} ESMF_SysTest69527.o

RUN_ESMF_SysTest69527:
	-@rm -f testlog stdlog.* logfile.* ESMF_SysTest69527.tmps
	${MPIRUN} -np 2 $(ESMC_TESTDIR)/ESMF_SysTest69527 > $(ESMC_TESTDIR)/ESMF_SysTest69527.stdout
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


