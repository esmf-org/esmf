# $Id: makefile,v 1.12 2003/04/14 14:51:43 nscollins Exp $ 

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
LOCDIR	  = src/SystemTests/62502


SYSTEM_TESTS_BUILD    = $(ESMC_TESTDIR)/ESMF_SysTest62502

SYSTEM_TESTS_RUN      = RUN_ESMF_SysTest62502  

SYSTEM_TESTS_RUN_UNI  = RUN_ESMF_SysTest62502UNI

SYSTEM_TESTS_RM       = ESMF_SysTest62502.rm

include $(ESMF_DIR)/makefile

DIRS = 

CLEANDIRS   =
CLEANFILES  = *.o *.mod
CLOBBERDIRS =


# user_models (and .mod files) have to be built first
ESMF_SysTest62502.o: user_model1.o user_model2.o user_coupler.o

$(ESMC_TESTDIR)/ESMF_SysTest62502: ESMF_SysTest62502.o user_model1.o \
		user_model2.o user_coupler.o chkopts
	$(SL_F_LINKER) -o ESMF_SysTest62502 ESMF_SysTest62502.o user_model1.o \
	    user_model2.o user_coupler.o -lesmf ${F90CXXLIBS} \
	    ${MPI_LIB} ${MP_LIB} ${THREAD_LIB} ${PCL_LIB} $(SL_LINKOPTS)
	mv -f ESMF_SysTest62502 $(ESMC_TESTDIR)
	${RM} -f *.o *.mod

RUN_ESMF_SysTest62502:
	-@rm -f testlog stdlog.* logfile.* ESMF_SysTest62502.tmps
	${MPIRUN} -np 4 $(ESMC_TESTDIR)/ESMF_SysTest62502 >$(ESMC_TESTDIR)/ESMF_SysTest62502.stdout

RUN_ESMF_SysTest62502UNI:
	-@rm -f testlog stdlog.* logfile.* ESMF_SysTest62502.tmps
	${MPIRUN} -np 1 $(ESMC_TESTDIR)/ESMF_SysTest62502 >$(ESMC_TESTDIR)/ESMF_SysTest62502.stdout

ESMF_SysTest62502.rm:
	echo "ESMF_SysTest62502.rm target called here."



