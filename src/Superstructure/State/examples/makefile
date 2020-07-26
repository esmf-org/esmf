# $Id$ 

ALL:      build_examples
run:      run_examples
run_uni:  run_examples_uni

LOCDIR	  = src/Superstructure/State/examples

.NOTPARALLEL:
EXAMPLES_BUILD = $(ESMF_EXDIR)/ESMF_StateEx \
                 $(ESMF_EXDIR)/ESMF_StateReadWriteEx

EXAMPLES_RUN   = run_ESMF_StateEx \
                 run_ESMF_StateReadWriteEx

EXAMPLES_RUN_UNI = run_ESMF_StateEx_uni \
                   run_ESMF_StateReadWriteEx_uni
                   
include $(ESMF_DIR)/makefile


CLEANDIRS   = 
CLEANFILES  = $(EXAMPLES_BUILD)
CLOBBERDIRS =

DIRS = 

#
# ESMF_StateEx
#
run_ESMF_StateEx:
	$(MAKE) EXNAME=State NP=4 exfrun

run_ESMF_StateEx_uni:
	$(MAKE) EXNAME=State NP=1 exfrun

#
# ESMF_StateReadWriteEx
#
run_ESMF_StateReadWriteEx:
	cp -f $(ESMF_DIR)/src/Infrastructure/IO/tests/io_netcdf_testdata.nc $(ESMF_EXDIR)
	$(MAKE) EXNAME=StateReadWrite NP=4 exfrun

run_ESMF_StateReadWriteEx_uni:
	cp -f $(ESMF_DIR)/src/Infrastructure/IO/tests/io_netcdf_testdata.nc $(ESMF_EXDIR)
	$(MAKE) EXNAME=StateReadWrite NP=1 exfrun

