# $Id: makefile,v 1.26 2004/08/03 19:33:47 nscollins Exp $ 

ALL:  build_tests

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
LOCDIR	  = src/Infrastructure/Field/tests

TESTS_BUILD   = $(ESMC_TESTDIR)/ESMF_FieldUTest \
		$(ESMC_TESTDIR)/ESMF_FRouteUTest 

TESTS_RUN     = RUN_ESMF_FieldUTest \
		RUN_ESMF_FRouteUTest 

TESTS_RUN_UNI = RUN_ESMF_FieldUTestUNI 
# Don't run ESMF_FRouteUtest on one processor

TESTS_RM      = ESMF_FieldUTest.rm \
		ESMF_FRouteUTest.rm 

include ${ESMF_DIR}/makefile

CLEANDIRS   = coredir.*
CLEANFILES  = *.o *.mod NULL *.txt stdoutfiles core ESMF_LogFile $(TESTS_BUILD)
CLOBBERDIRS =


#
# Field
#
RUN_ESMF_FieldUTest:
	-@rm -f testlog stdlog.* logfile.* ESMF_FieldUTest.tmps
	-${MPIRUN} -np 4 $(ESMC_TESTDIR)/ESMF_FieldUTest > $(ESMC_TESTDIR)/ESMF_FieldUTest.stdout

RUN_ESMF_FieldUTestUNI:
	-@rm -f testlog stdlog.* logfile.* ESMF_FieldUTest.tmps
	-${MPIRUN} -np 1 $(ESMC_TESTDIR)/ESMF_FieldUTest > $(ESMC_TESTDIR)/ESMF_FieldUTest.stdout

ESMF_FieldUTest.rm:
	echo "ESMF_FieldUTest.rm target called here."

#
# Field Route function
#
RUN_ESMF_FRouteUTest:
	-@rm -f testlog stdlog.* logfile.* ESMF_FRouteUTest.tmps
	-${MPIRUN} -np 4 $(ESMC_TESTDIR)/ESMF_FRouteUTest > $(ESMC_TESTDIR)/ESMF_FRouteUTest4.stdout
	-${MPIRUN} -np 8 $(ESMC_TESTDIR)/ESMF_FRouteUTest > $(ESMC_TESTDIR)/ESMF_FRouteUTest8.stdout
	# for now comment out the larger tests.  as far as i know, these
	# run, but can cause problems for the automatic overnight builds.
	#-${MPIRUN} -np 12 $(ESMC_TESTDIR)/ESMF_FRouteUTest > $(ESMC_TESTDIR)/ESMF_FRouteUTest12.stdout
	#-${MPIRUN} -np 16 $(ESMC_TESTDIR)/ESMF_FRouteUTest > $(ESMC_TESTDIR)/ESMF_FRouteUTest16.stdout
	#-${MPIRUN} -np 20 $(ESMC_TESTDIR)/ESMF_FRouteUTest > $(ESMC_TESTDIR)/ESMF_FRouteUTest20.stdout
	#-${MPIRUN} -np 24 $(ESMC_TESTDIR)/ESMF_FRouteUTest > $(ESMC_TESTDIR)/ESMF_FRouteUTest24.stdout
	#-${MPIRUN} -np 32 $(ESMC_TESTDIR)/ESMF_FRouteUTest > $(ESMC_TESTDIR)/ESMF_FRouteUTest24.stdout

RUN_ESMF_FRouteUTestUNI:
	-@rm -f testlog stdlog.* logfile.* ESMF_FRouteUTest.tmps
	-${MPIRUN} -np 1 $(ESMC_TESTDIR)/ESMF_FRouteUTest > $(ESMC_TESTDIR)/ESMF_FRouteUTest.stdout

ESMF_FRouteUTest.rm:
	echo "ESMF_FRouteUTest.rm target called here."



