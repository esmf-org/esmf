# $Id: makefile,v 1.8 2003/03/06 15:28:45 flanigan Exp $ 

ALL:  system_tests

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


SYSTEM_TESTS_BUILD = $(ESMC_TESTDIR)/ESMF_SysTest70385

SYSTEM_TESTS_RUN   = RUN_ESMF_SysTest70385  

SYSTEM_TESTS_RM    = ESMF_SysTest70385.rm

include $(ESMF_DIR)/makefile

DIRS = 


$(ESMC_TESTDIR)/ESMF_SysTest70385: ESMF_SysTest70385.o  chkopts
	-${F90CXXLD} -o ESMF_SysTest70385 ESMF_SysTest70385.o -lesmf ${F90CXXLIBS} ${MPI_LIB} ${MP_LIB} ${THREAD_LIB} ${PCL_LIB}
	mv -f ESMF_SysTest70385 $(ESMC_TESTDIR)
	${RM} ESMF_SysTest70385.o

RUN_ESMF_SysTest70385:
	-@rm -f testlog stdlog.* logfile.* ESMF_SysTest70385.tmps
	${MPIRUN} -np 12 $(ESMC_TESTDIR)/ESMF_SysTest70385 > $(ESMC_TESTDIR)/ESMF_SysTest70385.stdout

RUN_ESMF_SysTest70385UNI:
	-@rm -f testlog stdlog.* logfile.* ESMF_SysTest70385.tmps
	${MPIRUN} -np 1 ESMF_SysTest70385 > ESMF_SysTest70385.stdout

ESMF_SysTest70385.rm:
	echo "ESMF_SysTest70385.rm target called here."


