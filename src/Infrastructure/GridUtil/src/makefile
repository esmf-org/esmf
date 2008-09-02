# $Id: makefile,v 1.5 2008/09/02 22:35:18 theurich Exp $

# leave alone.
ALL: build_here 

# If you have C files in this directory, list them all on the line below.
SOURCEC	  = ESMC_SparseMsgVM.C ESMC_DistDir.C ESMC_GridToMesh.C
            
# If you have F90 files in this directory, list them all on the line below.
SOURCEF	  = ESMF_GeomBase.F90
# If you have .h include files in this dir, list them all on the line below.
SOURCEH	  = 
# List all .h files which should be copied to common include dir
STOREH	  = 

# Leave these alone.
OBJSC     = $(addsuffix .o, $(basename $(SOURCEC)))
OBJSF     = $(addsuffix .o, $(basename $(SOURCEF)))

MANSEC	  = GridUtilobj
LIBBASE	  = libesmf

# The following line must be set to the current subdir name relative
# to the top level esmf directory. (e.g. Base/src, TimeMgr/interface, etc)
LOCDIR	  = src/Infrastructure/GridUtil/src


include ${ESMF_DIR}/makefile

DIRS =

CLEANDIRS   =
CLEANFILES  =
CLOBBERDIRS =

