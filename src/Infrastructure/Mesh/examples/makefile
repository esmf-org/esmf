# $Id: makefile,v 1.16 2009/10/29 22:57:08 theurich Exp $

ALL: build_examples
run: run_examples
run_uni: run_examples_uni

# Work around an MPICH(2) issue
ESMF_CXXCOMPILECPPFLAGS += -DMPICH_IGNORE_CXX_SEEK

LOCDIR	  = src/Infrastructure/Mesh/examples


EXAMPLES_BUILD    = \
                      $(ESMF_EXDIR)/ESMC_RendEx \
                      $(ESMF_EXDIR)/ESMC_DCatEx \
                      $(ESMF_EXDIR)/ESMC_RefineEx \
                      $(ESMF_EXDIR)/ESMC_PatchWghtEx \
                      $(ESMF_EXDIR)/ESMF_MeshEx 
                      
#                      $(ESMF_EXDIR)/ESMC_BreckEx \

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
	$(MAKE) EXNAME=Mesh NP=6 exfrun

