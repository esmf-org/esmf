# $Id: makefile,v 1.6 2003/03/17 22:30:08 flanigan Exp $ 

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

SYSTEM_TESTS_BUILD   = $(ESMC_TESTDIR)/ESMF_SysTest70384

SYSTEM_TESTS_RUN     = RUN_ESMF_SysTest70384  

SYSTEM_TESTS_RUN_UNI = RUN_ESMF_SysTest70384UNI

SYSTEM_TESTS_RM      = ESMF_SysTest70384.rm

include $(ESMF_DIR)/makefile

DIRS = 

$(ESMC_TESTDIR)/ESMF_SysTest70384: ESMF_SysTest70384.o  chkopts
	${F90CXXLD} -o ESMF_SysTest70384 ESMF_SysTest70384.o -lesmf ${F90CXXLIBS} ${MPI_LIB} ${MP_LIB} ${THREAD_LIB} ${PCL_LIB} $(SL_LINKOPTS)
	mv -f ESMF_SysTest70384 $(ESMC_TESTDIR)
	${RM} ESMF_SysTest70384.o

RUN_ESMF_SysTest70384:
	-@rm -f testlog stdlog.* logfile.* ESMF_SysTest70384.tmps
	${MPIRUN} -np 6 $(ESMC_TESTDIR)/ESMF_SysTest70384 > $(ESMC_TESTDIR)/ESMF_SysTest70384.stdout

RUN_ESMF_SysTest70384UNI:
	-@rm -f testlog stdlog.* logfile.* ESMF_SysTest70384.tmps
	${MPIRUN} -np 1 $(ESMC_TESTDIR)/ESMF_SysTest70384 > $(ESMC_TESTDIR)/ESMF_SysTest70384.stdout

ESMF_SysTest70384.rm:
	echo "ESMF_SysTest70384.rm target called here."



