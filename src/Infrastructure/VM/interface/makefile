# $Id: makefile,v 1.9.4.2 2008/04/02 23:16:39 theurich Exp $

# leave alone.
ALL: build_here

SOURCEC	  = ESMC_VM_F.C
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

