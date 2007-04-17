# $Id: makefile,v 1.1 2007/04/17 21:28:05 tjcnrl Exp $ 

# Leave alone.
ALL: build_here 

# Don't set these unless absolutely necessary - they've got to be
# architecture and compiler independent and should be inherited
# from the upper level makefile if possible.
CFLAGS    =
FFLAGS    =

# If you have C files in this directory, list them all on the line below.
SOURCEC	  = ESMC_Config.C
# If you have F90 files in this directory, list them all on the line below.
SOURCEF	  = ESMF_Config_C.F90 
# If you have .h include files in this dir, list them all on the line below.
SOURCEH	  =
# List .h files to be copied to higher level common include directory
STOREH	  = ESMCI_Config.h ESMC_Config.h 

# Leave these alone.
OBJSC     = $(addsuffix .o, $(basename $(SOURCEC)))
OBJSF     = $(addsuffix .o, $(basename $(SOURCEF)))

MANSEC	  = Configobj
LIBBASE	  = libesmf


# The following line must be set to the current subdir name relative
# to the top level esmf directory. (e.g. Base/src, TimeMgr/interface, etc)
LOCDIR	  = src/Infrastructure/Config/interface


include $(ESMF_DIR)/makefile

# Leave the following line blank.
DIRS	  = 

CLEANDIRS   =
CLEANFILES  =
CLOBBERDIRS =


