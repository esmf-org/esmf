#
# Earth System Modeling Applications (ESMA) post processing makefile fragment.
# Optionally, include this at the end of your makefile.
#
# Notes:
# 1. Building the "pinstall" target in a directory does the following:
#    a) If the directory has SUBDIRS, then
#       - each SUBDIR is built with "gmake pinstall"
#       - the directory is then built with the "local_install" target
#    b) If the directory has no SUBDIRS, then 
#       - the directory is built with the "install" target
# 2. Use $(subdir)_install dependency statements in the GNUmakefile
#    to indicate subdirectory dependencies.
#
# REVISION HISTORY:
#
# 15Dec2006  da Silva  First Crack
# 02Feb2010  Stassi    Added notes and made small changes to the code structure
#                      for clarity (i.e. to help me remember how it works).
#--------------------------------------------------------------------------

#                     -----------------
#                     Parallel Install
#                     -----------------

ifdef SUBDIRS
	PINSTALL_DIRS = $(foreach dir,$(SUBDIRS),$(dir)_install) 
	LOCAL_INSTALL = local_install
	PINSTALL_TARGET = $(LOCAL_INSTALL)
else
	PINSTALL_TARGET = install
endif

pinstall: $(PINSTALL_DIRS) 
	$(MAKE) -e $(PINSTALL_TARGET)

%_install: 
	@$(ESMA_TIMER_BEG)
	$(MAKE) -e -C $* pinstall
	@$(ESMA_TIMER_END)

pinstall_skip:
	@echo "Skipping local_install in `pwd`"
