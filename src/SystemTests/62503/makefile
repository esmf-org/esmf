# $Id: makefile,v 1.3 2003/04/09 23:48:45 flanigan Exp $ 

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
LOCDIR	  = src/SystemTests/62503


SYSTEM_TESTS_BUILD    = $(ESMC_TESTDIR)/ESMF_SysTest62503

SYSTEM_TESTS_RUN      = RUN_ESMF_SysTest62503  

SYSTEM_TESTS_RUN_UNI  = RUN_ESMF_SysTest62503UNI

SYSTEM_TESTS_RM       = ESMF_SysTest62503.rm

include $(ESMF_DIR)/makefile

DIRS = 

CLEANDIRS   =
CLEANFILES  = *.o *.mod
CLOBBERDIRS =


# user_models (and .mod files) have to be built first
ESMF_SysTest62503.o: user_model1.o user_model2.o user_coupler.o

$(ESMC_TESTDIR)/ESMF_SysTest62503: ESMF_SysTest62503.o user_model1.o \
		user_model2.o user_coupler.o chkopts
	$(SL_F_LINKER) -o ESMF_SysTest62503 ESMF_SysTest62503.o user_model1.o \
	    user_model2.o user_coupler.o -lesmf ${F90CXXLIBS} \
	    ${MPI_LIB} ${MP_LIB} ${THREAD_LIB} ${PCL_LIB} $(SL_LINKOPTS)
	mv -f ESMF_SysTest62503 $(ESMC_TESTDIR)
	${RM} *.o *.mod

RUN_ESMF_SysTest62503:
	-@rm -f testlog stdlog.* logfile.* ESMF_SysTest62503.tmps
	${MPIRUN} -np 8 $(ESMC_TESTDIR)/ESMF_SysTest62503 >$(ESMC_TESTDIR)/ESMF_SysTest62503.stdout

RUN_ESMF_SysTest62503UNI:
	echo "ESMF_SysTest62503 cannot be run as uniprocessor."

ESMF_SysTest62503.rm:
	echo "ESMF_SysTest62503.rm target called here."



