# $Id: makefile,v 1.24 2011/07/29 18:40:50 rokuingh Exp $

ALL: build_examples
run: run_examples
run_uni: run_examples_uni

# Work around an MPICH(2) issue
ESMF_CXXCOMPILECPPFLAGS += -DMPICH_IGNORE_CXX_SEEK

LOCDIR	  = src/Infrastructure/Mesh/examples


EXAMPLES_BUILD    = \
                      $(ESMF_EXDIR)/ESMC_TriangleEx \

EXAMPLES_RUN      = run_ESMF_MeshEx 

EXAMPLES_RUN_UNI  =


include $(ESMF_DIR)/makefile

DIRS	  = 

CLEANDIRS   = 
CLEANFILES  = $(EXAMPLES_BUILD)
CLOBBERDIRS =

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

run_ESMC_TriangleEx:
	cp data/grid-trianglesOct5level.vtk $(ESMF_EXDIR)
	chmod u+rw $(ESMF_EXDIR)/grid-trianglesOct5level.vtk
	$(MAKE) EXNAME=Triangle NP=1 excrun
