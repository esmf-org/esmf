# $Id: makefile,v 1.1 2003/09/18 13:34:38 cdeluca Exp $

# leave alone.
ALL: lib 
# if you have C files in this directory, list them all on the line below.
SOURCEC	  = ESMC_DataMap.C
# if you have F90 files in this directory, list them all on the line below.
SOURCEF	  =  
# if you have .h include files in this dir, list them all on the line below.
SOURCEH	  = 
# include files which go into the public src dir
STOREH	  =  ESMC_DataMap.h

# leave these alone.
OBJSC     = $(addsuffix .o, $(basename $(SOURCEC)))
OBJSF     = $(addsuffix .o, $(basename $(SOURCEF)))

MANSEC	  = Dataobj
LIBBASE	  = libesmf



# the following line must be set to the current subdir name relative
# to the top level esmf directory. (e.g. Base/src, TimeMgr/interface, etc)
LOCDIR	  = src/Infrastructure/DataMap/interface


include $(ESMF_DIR)/makefile

# leave the following line blank.
DIRS	  = 

CLEANDIRS   =
CLEANFILES  =
CLOBBERDIRS =
