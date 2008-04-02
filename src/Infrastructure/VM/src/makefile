# $Id: makefile,v 1.5.6.2 2008/04/02 23:16:39 theurich Exp $

# leave alone.
ALL: build_here 

# If you have C files in this directory, list them all on the line below.
SOURCEC	  = ESMC_VM.C ESMC_VMKernel.C
# If you have F90 files in this directory, list them all on the line below.
SOURCEF	  = 
# If you have .h include files in this dir, list them all on the line below.
SOURCEH	  = 
# List all .h files which should be copied to common include dir
STOREH	  = ESMC_VM.h ESMC_VMKernel.h

# Leave these alone.
OBJSC     = $(addsuffix .o, $(basename $(SOURCEC)))
OBJSF     = $(addsuffix .o, $(basename $(SOURCEF)))

MANSEC	  = VMobj
LIBBASE	  = libesmf

# The following line must be set to the current subdir name relative
# to the top level esmf directory. (e.g. Base/src, TimeMgr/interface, etc)
LOCDIR	  = src/Infrastructure/VM/src

CLEANDIRS  =
CLEANFILES =
CLOBBERDIS =

include ${ESMF_DIR}/makefile

# Leave the following line blank.
DIRS	  = 

