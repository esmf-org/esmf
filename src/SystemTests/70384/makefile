# $Id: makefile,v 1.3 2003/02/27 23:28:17 nscollins Exp $ 

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
LOCDIR	  = src/SystemTests/70384

SYSTEM_TESTS_BUILD = $(ESMC_TESTDIR)/ESMF_SysTest70384

SYSTEM_TESTS_RUN   = RUN_ESMF_SysTest70384  

SYSTEM_TESTS_RM    = ESMF_SysTest70384.rm

include $(ESMF_DIR)/makefile

DIRS = 

$(ESMC_TESTDIR)/ESMF_SysTest70384: ESMF_SysTest70384.o  chkopts
	${F90CXXLD} -o ESMF_SysTest70384 ESMF_SysTest70384.o -lesmf ${F90CXXLIBS} ${MPI_LIB} ${MP_LIB} ${THREAD_LIB} ${PCL_LIB}
	mv -f ESMF_SysTest70384 $(ESMC_TESTDIR)
	${RM} ESMF_SysTest70384.o

RUN_ESMF_SysTest70384:
	-@rm -f testlog stdlog.* logfile.* ESMF_SysTest70384.tmps
	${MPIRUN} -np 6 $(ESMC_TESTDIR)/ESMF_SysTest70384 > $(ESMC_TESTDIR)/ESMF_SysTest70384.stdout
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



