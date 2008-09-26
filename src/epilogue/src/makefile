# $Id: makefile,v 1.5 2008/09/26 04:17:35 cdeluca Exp $

# leave alone.
ALL: build_here 

# if you have C files in this directory, list them all on the line below.
SOURCEC	  = ESMCI_Test.C ESMC_Test.C
# if you have F90 files in this directory, list them all on the line below.
SOURCEF	  = ESMF_Test.F90
# list all .h files which should be copied to common include dir
STOREH	  = ESMCI_Test.h ESMC_Test.h

# leave these alone.
OBJSC     = $(addsuffix .o, $(basename $(SOURCEC)))
OBJSF     = $(addsuffix .o, $(basename $(SOURCEF)))

LIBBASE	  = libesmf

# the following line must be set to the current subdir name relative
# to the top level esmf directory. (e.g. Base/src, TimeMgr/interface, etc)
LOCDIR	  = src/epilogue/src

CLEANDIRS  =
CLEANFILES =
CLOBBERDIS =

include ${ESMF_DIR}/makefile

# leave the following line blank.
DIRS	  = 

