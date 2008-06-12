# $Id: makefile,v 1.6 2008/06/12 18:58:48 dneckels Exp $

ALL: build_examples
run: run_examples
run_uni: run_examples_uni

CFLAGS    =
FFLAGS    =
SOURCEC	  = 
SOURCEF	  = 
SOURCEH	  = 
OBJSC     = 
OBJSF     = 
TEXFILES  = 
MANSEC	  = 
LIBBASE	  = 
LOCDIR	  = src/Infrastructure/Mesh/examples


EXAMPLES_BUILD    = \
                      $(ESMF_EXDIR)/ESMC_RendEx \
                      $(ESMF_EXDIR)/ESMC_DCatEx \
                      $(ESMF_EXDIR)/ESMC_RefineEx \
                      $(ESMF_EXDIR)/ESMC_PatchWghtEx \
                      $(ESMF_EXDIR)/ESMC_BreckEx \
                      $(ESMF_EXDIR)/ESMF_MeshEx \

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

