# $Id: makefile,v 1.7 2003/03/03 16:34:14 flanigan Exp $ 

ALL:

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
LOCDIR	  = src/SystemTests/63029


SYSTEM_TESTS_BUILD = $(ESMC_TESTDIR)/ESMF_SysTest63029

SYSTEM_TESTS_RUN   = RUN_ESMF_SysTest63029  

SYSTEM_TESTS_RM    = ESMF_SysTest63029.rm

include $(ESMF_DIR)/makefile

DIRS = 

# user_model (and .mod file) has to be built first
ESMF_SysTest63029.o: user_model.o

$(ESMC_TESTDIR)/ESMF_SysTest63029: ESMF_SysTest63029.o  user_model.o chkopts
	-${F90CXXLD} -o ESMF_SysTest63029 ESMF_SysTest63029.o user_model.o \
	    -lesmf ${F90CXXLIBS} ${MPI_LIB} ${MP_LIB} ${THREAD_LIB} ${PCL_LIB}
	mv -f ESMF_SysTest63029 $(ESMC_TEST_DIR)
	${RM} ESMF_SysTest63029.o user_model.o *.mod

RUN_ESMF_SysTest63029:
	-@rm -f testlog stdlog.* logfile.* ESMF_SysTest63029.tmps
	${MPIRUN} -np 2 ESMF_SysTest63029 > ESMF_SysTest63029.stdout

RUN_ESMF_SysTest63029UNI:
	-@rm -f testlog stdlog.* logfile.* ESMF_SysTest63029.tmps
	${MPIRUN} -np 1 ESMF_SysTest63029 > ESMF_SysTest63029.stdout

ESMF_SysTest63029.rm:
	echo "ESMF_SysTest63029.rm target called here."



