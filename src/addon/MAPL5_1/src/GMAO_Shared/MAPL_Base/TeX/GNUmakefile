#
# Makefile to build MAPL_Manual
#
# REVISION HISTORY:
#
# 17Aug2012  Purnendu	First version
#------------------------------------

# Make sure ESMADIR is defined
# ----------------------------
ifndef ESMADIR
       ESMADIR := $(shell pwd)/../../../..
endif

THIS := $(shell basename `pwd`)

# Compilation rules, flags, etc
# -----------------------------
  include $(ESMADIR)/Config/ESMA_base.mk  # Generic stuff
  include $(ESMADIR)/Config/ESMA_arch.mk  # System dependencies
  include $(ESMADIR)/Config/GMAO_base.mk

#                  ---------------------
#                  Standard ESMA Targets
#                  ---------------------

esma_help help:
	@echo "Standard ESMA targets:"
	@echo "% make esma_install    (builds and install under ESMADIR)"
	@echo "% make esma_clean      (removes deliverables: *.[aox], etc)"
	@echo "% make esma_distclean  (leaves in the same state as cvs co)"
	@echo "% make esma_doc        (generates PDF, installs under ESMADIR)"
	@echo "% make esma_help       (this message)"
	@echo "Environment:"
	@echo "      ESMADIR = $(ESMADIR)"
	@echo "      BASEDIR = $(BASEDIR)"
	@echo "         ARCH = $(ARCH)"
	@echo "         SITE = $(SITE) "
	@echo "         THIS = $(THIS) "



esma_install install:
	@echo "Nothing to be installed here"

#                  --------------------
#                     Documentation
#                  --------------------

MAPLUG = MAPL_UsersGuide

esma_doc doc: $(MAPLUG).tex
	  $(PDFLATEX) $(MAPLUG).tex
	  $(PDFLATEX) $(MAPLUG).tex
	  $(MKINDX) $(MAPLUG).idx
	  $(PDFLATEX) $(MAPLUG).tex
	  $(PDFLATEX) $(MAPLUG).tex
	  $(CP) $(MAPLUG).pdf $(ESMADOC)

# Sources
MAN_SRC :=  MAPL_Intro.tex

API_SRC := ../MAPL_Cap.F90 ../MAPL_Generic.F90 ../MAPL_CFIO.F90 \
	   ../MAPL_LocStreamMod.F90 ../MAPL_Base.F90 \
	   ../ESMFL_Mod.P90 ../MAPL_HistoryGridComp.F90 \
	   ../MAPL_GenericCplComp.F90 ../MAPL_ExtDataGridCompMod.F90

$(MAPLUG).tex: $(MAN_SRC) MAPL_Preamble.tex MAPL_API__.tex
	  $(CAT) MAPL_Preamble.tex $(MAN_SRC) > $(MAPLUG).tex
	  @echo "\end{document}" >> $(MAPLUG).tex

MAPL_API__.tex: $(API_SRC)
	  $(PROTEX) $(PROTEX_FLAGS) $(API_SRC) > MAPL_API__.tex

esma_clean clean:
	  $(RM) -f *~ *.aux *.log *.out *.toc *__.tex $(MAPLUG).*

esma_distclean distclean:
	  $(RM) -f *~ *.aux *.log *.out *.toc *__.tex $(MAPLUG).*

  -include $(ESMADIR)/Config/ESMA_post.mk  # ESMA additional targets, macros

