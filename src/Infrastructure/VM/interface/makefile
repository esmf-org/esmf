# $Id: makefile,v 1.12 2008/07/21 23:25:53 theurich Exp $

# leave alone.
ALL: build_here

SOURCEC	  = ESMCI_VM_F.C
SOURCEF	  = ESMF_VM.F90
SOURCEH	  = 
STOREH	  = 

# Leave these alone.
OBJSC     = $(addsuffix .o, $(basename $(SOURCEC)))
OBJSF     = $(addsuffix .o, $(basename $(SOURCEF)))

MANSEC	  = VMobj
LIBBASE	  = libesmf

# The following line must be set to the current subdir name relative
# to the top level esmf directory. (e.g. Base/src, TimeMgr/interface, etc)
LOCDIR	  = src/Infrastructure/VM/interface

CLEANDIRS  =
CLEANFILES =
CLOBBERDIS =

include ${ESMF_DIR}/makefile

# Leave the following line blank.
DIRS	  = 

