# $Id: makefile,v 1.6 2003/02/28 00:05:36 flanigan Exp $ 

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
	${RM} ESMF_SysTest63029.o user_model.o *.mod

RUN_ESMF_SysTest63029:
	-@rm -f testlog stdlog.* logfile.* ESMF_SysTest63029.tmps
	${MPIRUN} -np 2 ESMF_SysTest63029 > ESMF_SysTest63029.stdout

RUN_ESMF_SysTest63029UNI:
	-@rm -f testlog stdlog.* logfile.* ESMF_SysTest63029.tmps
	${MPIRUN} -np 1 ESMF_SysTest63029 > ESMF_SysTest63029.stdout

ESMF_SysTest63029.rm:
	echo "ESMF_SysTest63029.rm target called here."



