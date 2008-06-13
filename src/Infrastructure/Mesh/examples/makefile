# $Id: makefile,v 1.7 2008/06/13 04:32:30 theurich Exp $

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
                      $(ESMF_EXDIR)/ESMF_MeshEx \
                      
#                      $(ESMF_EXDIR)/ESMC_BreckEx \

EXAMPLES_RUN      = run_ESMC_RefineEx 

EXAMPLES_RUN_UNI  = run_ESMC_RefineEx_uni


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
	$(MAKE) EXNAME=Refine NP=1 excrun

