# $Id: makefile,v 1.13 2008/07/21 23:51:21 theurich Exp $

# leave alone.
ALL: build_here

SOURCEC	  = ESMCI_VM_F.C ESMC_VM.C
SOURCEF	  = ESMF_VM.F90
SOURCEH	  = 
STOREH	  = ESMC_VM.h

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

