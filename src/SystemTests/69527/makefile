# $Id: makefile,v 1.7 2003/04/02 21:05:14 flanigan Exp $ 

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

SYSTEM_TESTS_BUILD    = $(ESMC_TESTDIR)/ESMF_SysTest69527  

SYSTEM_TESTS_RUN      = RUN_ESMF_SysTest69527  

SYSTEM_TESTS_RUN_UNI  = RUN_ESMF_SysTest69527UNI

SYSTEM_TESTS_RM       = ESMF_SysTest69527.rm 


include $(ESMF_DIR)/makefile

DIRS = 

CLEANDIRS   =
CLEANFILES  = *.o *.mod
CLOBBERDIRS =

$(ESMC_TESTDIR)/ESMF_SysTest69527: ESMF_SysTest69527.o  chkopts
	${F90CXXLD} -o ESMF_SysTest69527 ESMF_SysTest69527.o -lesmf ${F90CXXLIBS} ${MPI_LIB} ${MP_LIB} ${THREAD_LIB} ${PCL_LIB} $(SL_LINKOPTS)
	mv -f ESMF_SysTest69527 $(ESMC_TESTDIR)
	${RM} -f *.o *.mod

RUN_ESMF_SysTest69527:
	-@rm -f testlog stdlog.* logfile.* ESMF_SysTest69527.tmps
	${MPIRUN} -np 2 $(ESMC_TESTDIR)/ESMF_SysTest69527 > $(ESMC_TESTDIR)/ESMF_SysTest69527.stdout

RUN_ESMF_SysTest69527UNI:
	-@rm -f testlog stdlog.* logfile.* ESMF_SysTest69527.tmps
	${MPIRUN} -np 1 $(ESMC_TESTDIR)/ESMF_SysTest69527 > $(ESMC_TESTDIR)/ESMF_SysTest69527.stdout

ESMF_SysTest69527.rm:
	echo "ESMF_SysTest69527.rm target called here."


