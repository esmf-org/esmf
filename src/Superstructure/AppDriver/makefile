# $Id: makefile,v 1.4 2003/11/12 22:02:46 nscollins Exp $ 


LOCDIR   = src/Superstructure/AppDriver

# this causes the makefile to build the quick_start rules
QUICKSTART = yes

# currently there are no subdirs in the quick_start dir.
QUICKSTART_SUBDIRS = 

# copy all files in this dir up to quick_start dir.
# (you should not have build the example first)
QUICKSTART_COPYDIRS = $(ESMF_DIR)/$(LOCDIR)/seq_pairwise_fdriver_spmd

include $(ESMF_DIR)/makefile

# at the present time do NOT list the subdirs here, because the makefiles
# inside them are not intended to be executed as part of the esmf build.
# (the doc dir is the exception - we do need to make the docs.)
DIRS     =   doc

CLEANDIRS   =
CLEANFILES  =
CLOBBERDIRS =










