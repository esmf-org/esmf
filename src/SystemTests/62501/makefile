# $Id: makefile,v 1.6 2003/03/25 00:18:34 flanigan Exp $ 

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

SYSTEM_TESTS_BUILD    = $(ESMC_TESTDIR)/ESMF_SysTest62501  

SYSTEM_TESTS_RUN      = RUN_ESMF_SysTest62501

SYSTEM_TESTS_RUN_UNI  = RUN_ESMF_SysTest62501UNI

SYSTEM_TESTS_RM       = ESMF_SysTest62501.rm


include $(ESMF_DIR)/makefile

DIRS	  = 

CLEANDIRS   =
CLEANFILES  = *.o *.mod
CLOBBERDIRS =


$(ESMC_TESTDIR)/ESMF_SysTest62501: ESMF_SysTest62501.o  chkopts
	${F90CXXLD} -o ESMF_SysTest62501 ESMF_SysTest62501.o -lesmf ${F90CXXLIBS} ${MPI_LIB} ${MP_LIB} ${THREAD_LIB} ${PCL_LIB} $(SL_LINKOPTS)
	mv -f ESMF_SysTest62501 $(ESMC_TESTDIR)
	${RM} ESMF_SysTest62501.o

RUN_ESMF_SysTest62501:  
	-@rm -f testlog stdlog.* logfile.* ESMF_SysTest62501.tmps
	${MPIRUN} -np 4 $(EMSC_TESTDIR)/ESMF_SysTest62501 > $(ESMC_TESTDIR)/ESMF_SysTest62501.stdout

RUN_ESMF_SysTest62501UNI:
	-@rm -f testlog stdlog.* logfile.* ESMF_SysTest62501.tmps
	${MPIRUN} -np 1 $(EMSC_TESTDIR)/ESMF_SysTest62501 > $(ESMC_TESTDIR)/ESMF_SysTest62501.stdout

ESMF_SysTest62501.rm:  
	echo "ESMF_SysTest62501.rm target called here."



