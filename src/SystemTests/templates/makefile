# $Id: makefile,v 1.4 2003/03/28 17:45:32 svasquez Exp $ 

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
LOCDIR	  = src/SystemTests/NNNNN

SYSTEM_TESTS_BUILD    = $(ESMC_TESTDIR)/ESMF_SysTestNNNNN

SYSTEM_TESTS_RUN      = RUN_ESMF_SysTestNNNNN

SYSTEM_TESTS_RUN_UNI  = RUN_ESMF_SysTestNNNNNUNI

SYSTEM_TESTS_RM       = ESMF_SysTestNNNNN.rm

include $(ESMF_DIR)/makefile

DIRS	  = 

$(ESMC_TESTDIR)/ESMF_SysTestNNNNN: ESMF_SysTestNNNNN.o  chkopts
	${F90CXXLD} -o ESMF_SysTestNNNNN ESMF_SysTestNNNNN.o -lesmf ${F90CXXLIBS} ${MPI_LIB} ${MP_LIB} ${THREAD_LIB} ${PCL_LIB} $(SL_LINKOPTS)
	mv -f ESMF_SysTestNNNNN $(ESMC_TESTDIR)
	${RM} *.o *.mod

RUN_ESMF_SysTestNNNNN:
	-@rm -f testlog stdlog.* logfile.* ESMF_SysTestNNNNN.tmps
	${MPIRUN} -np 4 $(ESMC_TESTDIR)/ESMF_SysTestNNNNN > $(ESMC_TESTDIR)/ESMF_SysTestNNNNN.stdout


RUN_ESMF_SysTestNNNNNUNI:
	-@rm -f testlog stdlog.* logfile.* ESMF_SysTestNNNNN.tmps
	${MPIRUN} -np 1 $(ESMC_TESTDIR)/ESMF_SysTestNNNNN > $(ESMC_TESTDIR)/ESMF_SysTestNNNNN.stdout


ESMF_SysTestNNNNN.rm:
	echo "ESMF_SysTestNNNNN.rm target called here."



