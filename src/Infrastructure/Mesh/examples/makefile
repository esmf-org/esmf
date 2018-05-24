# $Id$

ALL: build_examples
run: run_examples
run_uni: run_examples_uni

# Work around an MPICH(2) issue
ESMF_CXXCOMPILECPPFLAGS += -DMPICH_IGNORE_CXX_SEEK

LOCDIR	  = src/Infrastructure/Mesh/examples


.NOTPARALLEL:
EXAMPLES_BUILD    = \
                      $(ESMF_EXDIR)/ESMF_MeshEx \
                      $(ESMF_EXDIR)/ESMC_DCatEx \
                      $(ESMF_EXDIR)/ESMC_RefineEx \

EXAMPLES_RUN      = run_ESMF_MeshEx 

EXAMPLES_RUN_UNI  =


include $(ESMF_DIR)/makefile

DIRS	  = 

CLEANDIRS   = 
CLEANFILES  = $(EXAMPLES_BUILD)
CLOBBERDIRS =

ESMF_CXXCOMPILECPPFLAGS += -I$(ESMF_DIR)/$(LOCDIR)/../include \
                           -I$(ESMF_DIR)/$(LOCDIR)/../include/Legacy \
                           -I$(ESMF_DIR)/$(LOCDIR)/../include/Regridding

ifeq ($(ESMF_MOAB),internal)
ESMF_CXXCOMPILECPPFLAGS += -I$(ESMF_DIR)/$(LOCDIR)/../src/Moab \
                           -I$(ESMF_DIR)/$(LOCDIR)/../src/Moab/parallel \
                           -I$(ESMF_DIR)/$(LOCDIR)/../src/Moab/LocalDiscretization

endif

#
#  Run targets
#
run_ESMC_RefineEx:
	$(MAKE) EXNAME=Refine NP=4 excrun

run_ESMC_RefineEx_uni:
	echo "PASS: ESMF_RefineEx not set up to run on one processor." > $(ESMF_EXDIR)/ESMF_RefineEx.stdout

run_ESMF_MeshEx:
	cp -r data $(ESMF_EXDIR)
	chmod u+rw $(ESMF_EXDIR)/data/*
	$(MAKE) EXNAME=Mesh NP=4 exfrun

