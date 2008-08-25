# $Id: makefile,v 1.31 2008/08/25 22:04:17 theurich Exp $

# leave alone.
ALL: build_here 

# If you have C files in this directory, list them all on the line below.
SOURCEC	  = ESMCI_FTable.C
# If you have F90 files in this directory, list them all on the line below.
SOURCEF	  = ESMF_Comp.F90 ESMF_GridComp.F90 ESMF_CplComp.F90 
# If you have .h include files in this dir, list them all on the line below.
SOURCEH	  = 
# If you have public .h files in this dir, list them here
STOREH    = ESMCI_FTable.h

# Leave these alone.
OBJSC     = $(addsuffix .o, $(basename $(SOURCEC)))
OBJSF     = $(addsuffix .o, $(basename $(SOURCEF)))

# Is MANSEC something about documentation?
MANSEC	  = Componentobj
LIBBASE	  = libesmf



# The following line must be set to the current subdir name relative
# to the top level esmf directory. (e.g. Base/src, TimeMgr/interface, etc)
LOCDIR	  = src/Superstructure/Component/src


include $(ESMF_DIR)/makefile

# Leave the following line blank.
DIRS	  = 

CLEANDIRS   =
CLEANFILES  = 
CLOBBERDIRS =
