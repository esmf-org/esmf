# $Id: makefile,v 1.6 2003/04/15 20:13:42 svasquez Exp $ 

ALL:  tests

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
LOCDIR	  = src/Infrastructure/Base/tests

TESTS_BUILD   = $(ESMC_TESTDIR)/ESMF_BaseUTest \
		$(ESMC_TESTDIR)/ESMF_StringTest

TESTS_RUN     = RUN_ESMF_BaseUTest \
		RUN_ESMF_StringTest

TESTS_RUN_UNI = RUN_ESMF_BaseUTestUNI \
		RUN_ESMF_StringTestUNI

TESTS_RM      = ESMF_BaseUTest.rm \
		ESMF_StringTest.rm

include ${ESMF_DIR}/makefile

CLEANDIRS   =
CLEANFILES  = *.o *.mod 
CLOBBERDIRS =


#
#  Base class
#
$(ESMC_TESTDIR)/ESMF_BaseUTest: ESMF_BaseUTest.o
	-$(SL_F_LINKER) -o $(ESMC_TESTDIR)/ESMF_BaseUTest ESMF_BaseUTest.o -lesmf ${F90CXXLIBS} ${MPI_LIB} ${MP_LIB} ${THREAD_LIB} ${PCL_LIB} $(SL_LINKOPTS)
	${RM} ESMF_BaseUTest.o

RUN_ESMF_BaseUTest:
	-@rm -f testlog stdlog.* logfile.* ESMF_BaseUTest.tmps
	-${MPIRUN} -np 4 $(ESMC_TESTDIR)/ESMF_BaseUTest > $(ESMC_TESTDIR)/ESMF_BaseUTest.stdout

RUN_ESMF_BaseUTestUNI:
	-@rm -f testlog stdlog.* logfile.* ESMF_BaseUTest.tmps
	-${MPIRUN} -np 1 $(ESMC_TESTDIR)/ESMF_BaseUTest > $(ESMC_TESTDIR)/ESMF_BaseUTest.stdout

ESMF_BaseUTest.rm:
	echo "ESMF_BaseUTest.rm target called here."

#
# Special for strings passed between C++ and F90
#
ESMF_StringTest.o : ESMC_StringSubr.o

$(ESMC_TESTDIR)/ESMF_StringTest: ESMC_StringSubr.o ESMF_StringTest.o
	-$(SL_F_LINKER) -o $(ESMC_TESTDIR)/ESMF_StringTest ESMF_StringTest.o ESMC_StringSubr.o -lesmf ${F90CXXLIBS} ${MPI_LIB} ${MP_LIB} ${THREAD_LIB} ${PCL_LIB} $(SL_LINKOPTS)
	${RM} ESMF_StringTest.o ESMC_StringSubr.o 

RUN_ESMF_StringTest:
	-@rm -f testlog stdlog.* logfile.* ESMF_StringTest.tmps
	-${MPIRUN} -np 4 $(ESMC_TESTDIR)/ESMF_StringTest > $(ESMC_TESTDIR)/ESMF_StringTest.stdout

RUN_ESMF_StringTestUNI:
	-@rm -f testlog stdlog.* logfile.* ESMF_StringTest.tmps
	-${MPIRUN} -np 1 $(ESMC_TESTDIR)/ESMF_StringTest > $(ESMC_TESTDIR)/ESMF_StringTest.stdout

ESMF_StringTest.rm:
	echo "ESMF_StringTest.rm target called here."


