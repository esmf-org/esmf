# $Id: makefile,v 1.2 2003/02/27 17:55:09 flanigan Exp $ 

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
LOCDIR	  = src/SystemTests/62501

SYSTEM_TESTS_BUILD = $(ESMC_TESTDIR)/ESMF_SysTest62501  

SYSTEM_TESTS_RUN   = RUN_ESMF_SysTest62501  

SYSTEM_TESTS_RM    = ESMF_SysTest62501.rm


include $(ESMF_DIR)/makefile

DIRS	  = 

$(ESMC_TESTDIR)/ESMF_SysTest62501: ESMF_SysTest62501.o  chkopts
	${F90CXXLD} -o ESMF_SysTest62501 ESMF_SysTest62501.o -lesmf ${F90CXXLIBS} ${MPI_LIB} ${MP_LIB} ${THREAD_LIB} ${PCL_LIB}
	mv -f ESMF_SysTest62501 $(ESMC_TESTDIR)
	${RM} ESMF_SysTest62501.o
	

RUN_ESMF_SysTest62501:  
	-@rm -f testlog stdlog.* logfile.* ESMF_SysTest62501.tmps
	${MPIRUN} -np 4 $(EMSC_TESTDIR)/ESMF_SysTest62501 > $(ESMC_TESTDIR)/ESMF_SysTest62501.stdout
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



