# $Id: makefile,v 1.17 2009/10/16 05:58:29 eschwab Exp $ 

ALL:      build_examples
run:      run_examples
run_uni:  run_examples_uni

LOCDIR	  = src/Superstructure/State/examples


EXAMPLES_BUILD = $(ESMF_EXDIR)/ESMF_StateEx \
                 $(ESMF_EXDIR)/ESMF_StateReconcileEx \
                 $(ESMF_EXDIR)/ESMF_StateReadWriteEx \
                 $(ESMF_EXDIR)/ESMF_AttachMethodsEx 

EXAMPLES_RUN   = run_ESMF_StateEx \
                 run_ESMF_StateReconcileEx \
                 run_ESMF_StateReadWriteEx \
                 run_ESMF_AttachMethodsEx

EXAMPLES_RUN_UNI = run_ESMF_StateEx_uni \
                   run_ESMF_StateReconcileEx_uni \
                   run_ESMF_StateReadWriteEx_uni \
                   run_ESMF_AttachMethodsEx_uni
                   
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
# ESMF_StateReconcileEx
#
run_ESMF_StateReconcileEx:
	$(MAKE) EXNAME=StateReconcile NP=4 exfrun

run_ESMF_StateReconcileEx_uni:
	$(MAKE) EXNAME=StateReconcile NP=1 exfrun

#
# ESMF_StateReadWriteEx
#
run_ESMF_StateReadWriteEx:
	cp -f $(ESMF_DIR)/src/Infrastructure/IO/tests/io_netcdf_testdata.nc $(ESMF_EXDIR)
	$(MAKE) EXNAME=StateReadWrite NP=4 exfrun

run_ESMF_StateReadWriteEx_uni:
	cp -f $(ESMF_DIR)/src/Infrastructure/IO/tests/io_netcdf_testdata.nc $(ESMF_EXDIR)
	$(MAKE) EXNAME=StateReadWrite NP=1 exfrun

#
# ESMF_AttachMethodsEx
#
run_ESMF_AttachMethodsEx:
	$(MAKE) EXNAME=AttachMethods NP=4 exfrun

run_ESMF_AttachMethodsEx_uni:
	$(MAKE) EXNAME=AttachMethods NP=1 exfrun
