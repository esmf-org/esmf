# $Id: makefile,v 1.4 2008/10/29 14:52:39 rosalind Exp $

ALL: build_here 

SOURCEC	  = ESMCI_Mesh_F.C ESMC_Mesh.C
SOURCEF	  = ESMF_Mesh.F90 ESMF_Mesh_C.F90
SOURCEH	  = 

# list .h files to be copied to higher level common include directory
STOREH	  = ESMC_Mesh.h

OBJSC     = $(addsuffix .o, $(basename $(SOURCEC)))
OBJSF     = $(addsuffix .o, $(basename $(SOURCEF)))

LIBBASE	  = libesmf
LOCDIR	  = src/Infrastructure/Mesh/interface

CLEANDIRS   =
CLEANFILES  =
CLOBBERDIRS =

include $(ESMF_DIR)/makefile

DIRS	  = 
