# $Id: makefile,v 1.2 2003/04/17 17:31:23 nscollins Exp $ 

ALL: system_tests

CFLAGS    =
FFLAGS    =
SOURCEC	  = 
SOURCEF	  = FlowMod.F90 ArraysGlobalMod.F90 ESMF_SysTest74559.F90

SOURCEH	  = 
OBJSC	  =  
OBJSF	  =  ArraysGlobalMod.o FlowMod.o user_coupler.o
MANSEC	  = 
LIBBASE	  = 
LOCDIR	  = src/SystemTests/74559


SYSTEM_TESTS_BUILD    = $(ESMC_TESTDIR)/ESMF_SysTest74559

SYSTEM_TESTS_RUN      = RUN_ESMF_SysTest74559  

SYSTEM_TESTS_RUN_UNI  = RUN_ESMF_SysTest74559UNI

SYSTEM_TESTS_RM       = ESMF_SysTest74559.rm

include $(ESMF_DIR)/makefile

DIRS = 

CLEANDIRS   =
CLEANFILES  = *.o *.mod
CLOBBERDIRS =


# user_models (and .mod files) have to be built first
ESMF_SysTest74559.o: FlowMod.o HeatMod.o user_coupler.o
FlowMod.o: ArraysGlobalMod.o
HeatMod.o: ArraysGlobalMod.o

$(ESMC_TESTDIR)/ESMF_SysTest74559: ESMF_SysTest74559.o ${OBJSF} chkopts
	$(SL_F_LINKER) -o ESMF_SysTest74559 ESMF_SysTest74559.o ${OBJSF} \
	    -lesmf ${F90CXXLIBS} ${MPI_LIB} ${MP_LIB} ${THREAD_LIB} \
            ${PCL_LIB} $(SL_LINKOPTS)
	mv -f ESMF_SysTest74559 $(ESMC_TESTDIR)
	${RM} -f *.o *.mod

RUN_ESMF_SysTest74559:
	-@rm -f testlog stdlog.* logfile.* ESMF_SysTest74559.tmps
	${MPIRUN} -np 4 $(ESMC_TESTDIR)/ESMF_SysTest74559 >$(ESMC_TESTDIR)/ESMF_SysTest74559.stdout

RUN_ESMF_SysTest74559UNI:
	-@rm -f testlog stdlog.* logfile.* ESMF_SysTest74559.tmps
	${MPIRUN} -np 1 $(ESMC_TESTDIR)/ESMF_SysTest74559 >$(ESMC_TESTDIR)/ESMF_SysTest74559.stdout

ESMF_SysTest74559.rm:
	echo "ESMF_SysTest74559.rm target called here."



