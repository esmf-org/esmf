# $Id: makefile,v 1.2 2008/09/02 22:35:19 theurich Exp $

# leave alone.
ALL: build_here 

# if you have C files in this directory, list them all on the line below.
SOURCEC	  = ESMC_InternArrayComm_F.C
# if you have F90 files in this directory, list them all on the line below.
SOURCEF	  =  ESMF_InternArrayComm.F90
# if you have .h include files in this dir, list them all on the line below.
SOURCEH	  =

# leave these alone.
OBJSC     = $(addsuffix .o, $(basename $(SOURCEC)))
OBJSF     = $(addsuffix .o, $(basename $(SOURCEF)))

# is MANSEC something about documentation?
MANSEC	  = InternArrayobj
LIBBASE	  = libesmf



# the following line must be set to the current subdir name relative
# to the top level esmf directory. (e.g. Base/src, TimeMgr/interface, etc)
LOCDIR	  = src/Infrastructure/InternArrayComm/interface


include $(ESMF_DIR)/makefile

# leave the following line blank.
DIRS	  = 

CLEANDIRS   =
CLEANFILES  =
CLOBBERDIRS =

