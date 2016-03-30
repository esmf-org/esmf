# $Id$ 

ALL:      build_unit_tests
run:      run_unit_tests
run_uni:  run_unit_tests_uni

LOCDIR	  = src/addon/MAPL/tests

.NOTPARALLEL:
TESTS_BUILD   = \
		$(ESMF_TESTDIR)/ESMF_MAPL_GenericSetServicesUTest

TESTS_RUN     = \
		RUN_ESMF_MAPL_GenericSetServicesUTest

#TESTS_RUN_UNI = \
#		RUN_ESMF_MAPL_GenericSetServicesUTestUNI

include ${ESMF_DIR}/makefile

# add MAPL includes and libs
MAPL_INSTALL_ROOT = $(ESMF_DIR)/src/addon/MAPL/ESMA/Linux
MAPL_INCLUDEPATHS = -I$(MAPL_INSTALL_ROOT)/include/MAPL_Base
MAPL_LINKPATHS    = -L$(MAPL_INSTALL_ROOT)/lib
#MAPL_LINKLIBS     = -lGMAO_mpeu -lGMAO_eu -lMAPL_cfio -lMAPL_Base_stubs -lMAPL_Base
MAPL_LINKLIBS     = -lMAPL_Base -lMAPL_Base_stubs -lMAPL_cfio_r4 -lGMAO_mpeu

ESMF_F90COMPILEPATHS      += $(MAPL_INCLUDEPATHS)
ESMF_F90LINKPATHS         += $(MAPL_LINKPATHS)
ESMF_F90LINKLIBS          += $(MAPL_LINKLIBS)

CLEANDIRS   = 
CLEANFILES  = $(TESTS_BUILD) $(CPFILES) *.nc
CLOBBERDIRS =

DIRS        = 

#
# MAPL
#

RUN_ESMF_MAPL_GenericSetServicesUTest:
	cp MAPL.rc $(ESMF_TESTDIR)
	$(MAKE) TNAME=MAPL_GenericSetServices NP=4 ftest

