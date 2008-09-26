# $Id: makefile,v 1.6 2008/09/26 05:53:54 cdeluca Exp $

ALL: build_here 

SOURCEC	  = ESMC_SparseMsgVM.C ESMC_DistDir.C ESMC_GridToMesh.C
SOURCEF	  = ESMF_GeomBase.F90
SOURCEH	  = 

# List all .h files which should be copied to common include dir
STOREH	  = 

OBJSC     = $(addsuffix .o, $(basename $(SOURCEC)))
OBJSF     = $(addsuffix .o, $(basename $(SOURCEF)))

LIBBASE	  = libesmf
LOCDIR	  = src/Infrastructure/GridUtil/src

CLEANDIRS   =
CLEANFILES  =
CLOBBERDIRS =

include ${ESMF_DIR}/makefile

DIRS =

