# $Id: makefile,v 1.30 2005/02/28 16:34:56 nscollins Exp $ 

ALL:  build_tests

CFLAGS    =
FFLAGS    =
SOURCEC	  = 
SOURCEF	  =
CPPFILES  = ESMF_FRoute8UTest.F90

SOURCEH	  = 
OBJSC	  =  
OBJSF	  = 
MANSEC	  = 
LIBBASE	  = 
DIRS	  = 
LOCDIR	  = src/Infrastructure/Field/tests

TESTS_BUILD   = $(ESMC_TESTDIR)/ESMF_FieldUTest \
		$(ESMC_TESTDIR)/ESMF_FRoute4UTest \
		$(ESMC_TESTDIR)/ESMF_FRoute8UTest 

TESTS_RUN     = RUN_ESMF_FieldUTest \
		RUN_ESMF_FRouteUTest 

TESTS_RUN_UNI = RUN_ESMF_FieldUTestUNI \
                RUN_ESMF_FRoute4UTestUNI \
                RUN_ESMF_FRoute8UTestUNI

include ${ESMF_DIR}/makefile

CLEANDIRS   = coredir.*
CLEANFILES  = *.o *.mod NULL *.txt stdoutfiles core ESMF_LogFile \
               $(TESTS_BUILD) $(CPPFILES)
CLOBBERDIRS =

ESMF_FRoute8UTest.F90:  ESMF_FRoute4UTest.F90
	cp -f ESMF_FRoute4UTest.F90 ESMF_FRoute8UTest.F90

#
# Field
#
RUN_ESMF_FieldUTest:
	-${MPIRUN} -np 4 $(ESMC_TESTDIR)/ESMF_FieldUTest > $(ESMC_TESTDIR)/ESMF_FieldUTest.stdout

RUN_ESMF_FieldUTestUNI:
	-${MPIRUN} -np 1 $(ESMC_TESTDIR)/ESMF_FieldUTest > $(ESMC_TESTDIR)/ESMF_FieldUTest.stdout


#
# Field Route function
#
RUN_ESMF_FRouteUTest:
	-${MPIRUN} -np 4 $(ESMC_TESTDIR)/ESMF_FRoute4UTest > $(ESMC_TESTDIR)/ESMF_FRoute4UTest.stdout
	-${MPIRUN} -np 8 $(ESMC_TESTDIR)/ESMF_FRoute8UTest > $(ESMC_TESTDIR)/ESMF_FRoute8UTest.stdout

	# for now comment out the larger tests.  as far as i know, these
	# run, but can cause problems for the automatic overnight builds.
	#-${MPIRUN} -np 12 $(ESMC_TESTDIR)/ESMF_FRouteUTest > $(ESMC_TESTDIR)/ESMF_FRouteUTest12.stdout
	#-${MPIRUN} -np 16 $(ESMC_TESTDIR)/ESMF_FRouteUTest > $(ESMC_TESTDIR)/ESMF_FRouteUTest16.stdout
	#-${MPIRUN} -np 20 $(ESMC_TESTDIR)/ESMF_FRouteUTest > $(ESMC_TESTDIR)/ESMF_FRouteUTest20.stdout
	#-${MPIRUN} -np 24 $(ESMC_TESTDIR)/ESMF_FRouteUTest > $(ESMC_TESTDIR)/ESMF_FRouteUTest24.stdout
	#-${MPIRUN} -np 32 $(ESMC_TESTDIR)/ESMF_FRouteUTest > $(ESMC_TESTDIR)/ESMF_FRouteUTest24.stdout

RUN_ESMF_FRouteUTestUNI:
	echo "PASS: ESMF_FRouteUTest not set up to run on one processor." > $(ESMF_EXDIR)/ESMF_FRouteUTest.stdout


