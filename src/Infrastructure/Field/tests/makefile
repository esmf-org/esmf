# $Id: makefile,v 1.20 2003/05/23 16:12:46 flanigan Exp $ 

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
LOCDIR	  = src/Infrastructure/Field/tests

TESTS_BUILD   = $(ESMC_TESTDIR)/ESMF_FieldUTest \
		$(ESMC_TESTDIR)/ESMF_FRouteUTest \
		$(ESMC_TESTDIR)/ESMF_BundleUTest

TESTS_RUN     = RUN_ESMF_FieldUTest \
		RUN_ESMF_FRouteUTest \
		RUN_ESMF_BundleUTest 

TESTS_RUN_UNI = RUN_ESMF_FieldUTestUNI \
		RUN_ESMF_BundleUTestUNI 
# Don't run ESMF_FRouteUtest on one processor

TESTS_RM      = ESMF_FieldUTest.rm \
		ESMF_FRouteUTest.rm \
		ESMF_BundleUTest.rm 

include ${ESMF_DIR}/makefile

CLEANDIRS   = coredir.*
CLEANFILES  = *.o *.mod NULL *.txt stdoutfiles core $(TESTS_BUILD)
CLOBBERDIRS =


#
#  Bundles
#
RUN_ESMF_BundleUTest:
	-@rm -f testlog stdlog.* logfile.* ESMF_BundleUTest.tmps
	-${MPIRUN} -np 4 $(ESMC_TESTDIR)/ESMF_BundleUTest > $(ESMC_TESTDIR)/ESMF_BundleUTest.stdout

RUN_ESMF_BundleUTestUNI:
	-@rm -f testlog stdlog.* logfile.* ESMF_BundleUTest.tmps
	-${MPIRUN} -np 1 $(ESMC_TESTDIR)/ESMF_BundleUTest > $(ESMC_TESTDIR)/ESMF_BundleUTest.stdout

ESMF_BundleUTest.rm:
	echo "ESMF_BundleUTest.rm target called here."

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
	-${MPIRUN} -np 12 $(ESMC_TESTDIR)/ESMF_FRouteUTest > $(ESMC_TESTDIR)/ESMF_FRouteUTest12.stdout
	-${MPIRUN} -np 16 $(ESMC_TESTDIR)/ESMF_FRouteUTest > $(ESMC_TESTDIR)/ESMF_FRouteUTest16.stdout

RUN_ESMF_FRouteUTestUNI:
	-@rm -f testlog stdlog.* logfile.* ESMF_FRouteUTest.tmps
	-${MPIRUN} -np 1 $(ESMC_TESTDIR)/ESMF_FRouteUTest > $(ESMC_TESTDIR)/ESMF_FRouteUTest.stdout

ESMF_FRouteUTest.rm:
	echo "ESMF_FRouteUTest.rm target called here."



