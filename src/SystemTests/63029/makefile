# $Id: makefile,v 1.3 2003/02/27 17:55:10 flanigan Exp $ 

ALL:  test

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

# model has to be built first
ESMF_SysTest63029.o: user_model.o

ESMF_SysTest63029: ESMF_SysTest63029.o  user_model.o chkopts
	-${F90CXXLD} -o ESMF_SysTest63029 ESMF_SysTest63029.o user_model.o \
	    -lesmf ${F90CXXLIBS} ${MPI_LIB} ${MP_LIB} ${THREAD_LIB} ${PCL_LIB}
	${RM} ESMF_SysTest63029.o

RUN_ESMF_SysTest63029:
	-@rm -f testlog stdlog.* logfile.* ESMF_SysTest63029.tmps
	${MPIRUN} -np 2 ESMF_SysTest63029 > ESMF_SysTest63029.stdout

RUN_ESMF_SysTest63029UNI:
	-@rm -f testlog stdlog.* logfile.* ESMF_SysTest63029.tmps
	${MPIRUN} -np 1 ESMF_SysTest63029 > ESMF_SysTest63029.stdout

ESMF_SysTest63029.rm:
	echo "ESMF_SysTest63029.rm target called here."



