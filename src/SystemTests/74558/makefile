# $Id: makefile,v 1.1 2003/04/04 18:28:45 jwolfe Exp $ 

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
LOCDIR	  = src/SystemTests/74558


SYSTEM_TESTS_BUILD    = $(ESMC_TESTDIR)/ESMF_SysTest74558

SYSTEM_TESTS_RUN      = RUN_ESMF_SysTest74558  

SYSTEM_TESTS_RUN_UNI  = RUN_ESMF_SysTest74558UNI

SYSTEM_TESTS_RM       = ESMF_SysTest74558.rm

include $(ESMF_DIR)/makefile

DIRS = 

CLEANDIRS   =
CLEANFILES  = *.o *.mod
CLOBBERDIRS =


# user_model (and .mod files) have to be built first

ESMF_SysTest74558.o: 	FlowMod.o 

FlowMod.o:		ArraysGlobalMod.o

$(ESMC_TESTDIR)/ESMF_SysTest74558: ESMF_SysTest74558.o FlowMod.o \
				   ArraysGlobalMod.o chkopts
	${F90CXXLD} -o ESMF_SysTest74558 ESMF_SysTest74558.o \
	    FlowMod.o ArraysGlobalMod.o -lesmf ${F90CXXLIBS} \
	    ${MPI_LIB} ${MP_LIB} ${THREAD_LIB} $(SL_LINKOPTS)
	mv -f ESMF_SysTest74558 $(ESMC_TESTDIR)
	${RM} *.o *.mod

RUN_ESMF_SysTest74558:
	-@rm -f testlog stdlog.* logfile.* ESMF_SysTest74558.tmps
	${MPIRUN} -np 1 $(ESMC_TESTDIR)/ESMF_SysTest74558 >$(ESMC_TESTDIR)/ESMF_SysTest74558.stdout

RUN_ESMF_SysTest74558UNI:
	echo "ESMF_SysTest74558 cannot be run as uniprocessor."

ESMF_SysTest74558.rm:
	echo "ESMF_SysTest74558.rm target called here."



