# $Id: makefile,v 1.18.2.4 2008/05/13 05:22:03 theurich Exp $

# leave alone.
ALL: build_here

# If you have auto-generated files in this directory, list them all here.
AUTOGEN   = ESMF_ArrayCreate.F90 ESMF_ArrayGet.F90 ESMF_ArrayGather.F90 ESMF_ArrayScatter.F90
# If you have C files in this directory, list them all on the line below.
SOURCEC	  = ESMCI_Array_F.C
# If you have F90 files in this directory, list them all on the line below.
SOURCEF	  = $(AUTOGEN) ESMF_ArrayPr.F90 ESMF_Array.F90
# If you have .h include files in this dir, list them all on the line below.
SOURCEH	  = 
# List all .h files which should be copied to common include dir
STOREH	  = 

# Leave these alone.
OBJSC     = $(addsuffix .o, $(basename $(SOURCEC)))
OBJSF     = $(addsuffix .o, $(basename $(SOURCEF)))

MANSEC	  = Arrayobj
LIBBASE	  = libesmf

# The following line must be set to the current subdir name relative
# to the top level esmf directory. (e.g. Base/src, TimeMgr/interface, etc)
LOCDIR	  = src/Infrastructure/Array/interface

CLEANDIRS   =
CLEANFILES  = $(AUTOGEN)
CLOBBERDIRS =

include ${ESMF_DIR}/makefile

# Leave the following line blank.
DIRS	  = 

