# +-======-+ 
#  Copyright (c) 2003-2007 United States Government as represented by 
#  the Admistrator of the National Aeronautics and Space Administration.  
#  All Rights Reserved.
#  
#  THIS OPEN  SOURCE  AGREEMENT  ("AGREEMENT") DEFINES  THE  RIGHTS  OF USE,
#  REPRODUCTION,  DISTRIBUTION,  MODIFICATION AND REDISTRIBUTION OF CERTAIN 
#  COMPUTER SOFTWARE ORIGINALLY RELEASED BY THE UNITED STATES GOVERNMENT AS 
#  REPRESENTED BY THE GOVERNMENT AGENCY LISTED BELOW ("GOVERNMENT AGENCY").  
#  THE UNITED STATES GOVERNMENT, AS REPRESENTED BY GOVERNMENT AGENCY, IS AN 
#  INTENDED  THIRD-PARTY  BENEFICIARY  OF  ALL  SUBSEQUENT DISTRIBUTIONS OR 
#  REDISTRIBUTIONS  OF THE  SUBJECT  SOFTWARE.  ANYONE WHO USES, REPRODUCES, 
#  DISTRIBUTES, MODIFIES  OR REDISTRIBUTES THE SUBJECT SOFTWARE, AS DEFINED 
#  HEREIN, OR ANY PART THEREOF,  IS,  BY THAT ACTION, ACCEPTING IN FULL THE 
#  RESPONSIBILITIES AND OBLIGATIONS CONTAINED IN THIS AGREEMENT.
#  
#  Government Agency: National Aeronautics and Space Administration
#  Government Agency Original Software Designation: GSC-15354-1
#  Government Agency Original Software Title:  GEOS-5 GCM Modeling Software
#  User Registration Requested.  Please Visit http://opensource.gsfc.nasa.gov
#  Government Agency Point of Contact for Original Software:  
#  			Dale Hithon, SRA Assistant, (301) 286-2691
#  
# +-======-+ 
#
# Earth System Modeling Applications (ESMA) base makefile fragment.
# This fragment costumizes ESMF_base.mk for each each architecture. 
#
# REVISION HISTORY:
#
# 06Jun2003  da Silva  First Crack
# 07aug2003  Zaslavsky Added -64 option for IRIX64
# 29Oct2003  Sawyer    Merged back most material lost in 1.26
# 01Dec2003  da Silva  Mods to IRIX64 for building GMAO legacy code
# 02Dec2003  da Silva  Changed INC_MPI to /usr/include (blank causes -I )
# 28Mar2005  Owens/RT  Added courant.
# 01Apr2005  Todling   Changed location of perl on IRIX64 to /usr/bin
# 04Apr2005  da Silva  Changed back perl on IRIX as /usr/bin/perl breaks fdp
# 26Apr2005  da Silva  Merged in Carlos mods for building FVGSI.
# 27Mar2006  da Silva  On IRIX, removed $(OMPFLAG) from FFLAGS and kept its
#                      "-mp" definition; user is supposed to activated where
#                      desired, not by default. Also, added OMPFLAG to Altix.
# 18Apr2006  Stassi    Added MPFLAG for Intel Fortran Compiler
# 05Jan2007  Todling   Add -lsz from palm current baselibs
# 26Jan2007  da Silva  Merged in Kokron's discover port
# 23May2007  Stassi    Removed lf95 as possible Linux default f90 compiler
# 13Aug2008  da Silva  Removed g77 from LIBSCI under Linux.
# 25Oct2008  da Silva  Improved Mac support, made openmpi default on Linux, 
#                      added detection of intelmpi. Slowly, moving towards
#                      use of mpif90. Added F2PY customization.
# 22Jun2009  Stassi    Use mklpath.pl script to get Intel MKLPATH
# 19Jul2012  Thompson  Added blocks for Intel 12 to work on pleiades and
#                      discover with the AVX option as a choice FOPT path.
#                      Added detection of MPT as a possible MPI stack for
#                      PGI on Pleiades. Moved to CUDA 4.1 as default GPU 
#                      level.
# 24Sep2012  Thompson  Added blocks for Intel 13 to work on pleiades and
#                      discover with the AVX option as a choice FOPT path.
# 26Mar2013  Thompson  Added blocks for Open MPI that uses 
#                      'mpif90 -showme:link' to identify the correct flags
#                      for linking with Open MPI.
#                      Corrected MPT LIB_MPI via Dan Kokron.
#                      Corrected Intel 13 FPE to use '-fp-model source' as
#                      '-fp-model precise' is deprecated.
#                      For PGI, added Kepler info, moved to use gcc and 
#                      g++ for CC and CXX, and turned off the dual
#                      TARGET_ARCH by default.
# 03Dec2013  Thompson  Added entries for Intel 14 following Intel 13
#                      example. Removed sections for OSF1, AIX, IRIX64.
#                      Removed options specific to Intel Fortran 8, 9, 10.
#                      Fixed behavior of OMPFLAG. Additional entries for
#                      PGI in re arch and GPU.
#
#--------------------------------------------------------------------------

# -----
# TO DO: Remove default for BASEDIR, FC from here; user must set BASEDIR
# -----  and ESMA_FC to control these parameters.
#
  ifndef BASEDIR
     $(warning BASEDIR is undefined --- this will cause an error in the next release!)
  endif

#                             ---------------------
#                             User defined defaults
#                             ---------------------
  ifdef ESMA_FC
     FC := $(ESMA_FC)
  endif
  ifdef ESMA_F2PY
     F2PY := $(ESMA_F2PY)
  endif

#                               -----
#                               Linux
#                               -----

ifeq ($(ARCH),Linux)

# Linux default compilers
# -----------------------
  ifndef ESMA_FC
     FC := ifort
  endif
  CC  = cc
  CXX = c++
  CPP = cpp

#
#                    Linux Site Specific
#                    -------------------

# MAT: I do not think these are needed on modern
#      builds of the model. Carefully make sure 
#      before deleting
#
# Add -lgpfs to LIB_HDF5 on borg/discover nodes
# ---------------------------------------------
  ifeq ($(findstring borg,$(NODE)),borg)
     LIB_HDF5 += -lgpfs
  endif
  ifeq ($(findstring discover,$(NODE)),discover)
     LIB_HDF5 += -lgpfs
  endif

#
#                    Linux Compiler Specific
#                    -----------------------

  ifeq ($(ESMF_COMPILER), gfortran)
    FDEFS += -DGFORTRAN
    FOPT += -fno-range-check -fcoarray=single
  endif

# Absoft compiler
# ---------------
  ifeq ($(FC), f90) 
      EXTENDED_SOURCE = -W 132
      FREE_SOURCE = -f free
      FIXED_SOURCE = -f fixed
      LIB_ESMF = $(BASELIB)/esmf/libesmf.a #$(BASELIB)/esmf/libnetcdf_stubs.a
      LIB_MPI = -L$(BASELIB)/mpi -lpmpich++ -lfmpich -lmpich -lmpichfsup
      LIB_SYS = -lstdc++ -lpthread -lU77 -lrt
      FREAL4 =
      FREAL8 = -N113
      M = -p
      FINCS += -I$(BASEINC)/mpi
      FDEFS += -DABSOFT -DNO_R16 
      XFLAGS += -YEXT_NAMES=LCS -YEXT_SFX=_ $(EXTENDED_SOURCE)
      FOPT   = -g -trap=INVALID,DIVBYZERO,OVERFLOW
  endif

# Intel Fortran Compiler (ifort or mpiifort)
# ------------------------------------------
  ifeq ($(word 1,$(shell $(FC) --version)), ifort)

#   Determine compiler version
#   --------------------------
    IFORT_VER := $(subst ., ,$(word 3,$(shell ifort --version)))
    IFORT_MAJOR := $(word 1,$(IFORT_VER))
    IFORT_MINOR := $(word 2,$(IFORT_VER))
    FPIC := -fPIC
    EXTENDED_SOURCE := -extend_source
    FREE_SOURCE := -free
    FIXED_SOURCE := -fixed
    MPFLAG  := -mp
    OMPFLAG  := -openmp
    PP  := -fpp
    BIG_ENDIAN := -convert big_endian
    BYTERECLEN := -assume byterecl
    FPE = -fpe0
    ALIGNCOM = -align dcommons
    FREAL4 =
    FREAL8 = -r8
    FOPT2 += 
    ifeq ("$(BOPT)","g")
       FOPT = $(FOPTG) -O0 -ftz -align all -fno-alias -traceback -debug -nolib-inline -fno-inline-functions -assume protect_parens,minus0 -prec-div -prec-sqrt -check bounds -check uninit -fp-stack-check -ftrapuv -warn unused
    else
       ifeq ($(IFORT_MAJOR),11)
          FOPT = $(FOPT3) -vec-report0 -ftz -align all -fno-alias
#          FOPT = $(FOPT3) -xSSE4.1 -vec-report0 -ftz -align all -fno-alias
       else
       ifeq ($(IFORT_MAJOR),12)
          FOPT = $(FOPT3) -vec-report0 -ftz -align all -fno-alias
#         FOPT = $(FOPT3) -axAVX -xSSE4.1 -vec-report0 -ftz -align all -fno-alias
       else
       ifeq ($(IFORT_MAJOR),13)
          FOPT = $(FOPT3) -vec-report0 -ftz -align all -fno-alias -g -traceback
#         FOPT = $(FOPT3) -axAVX -xSSE4.1 -vec-report0 -ftz -align all -fno-alias -g -traceback
       else
       ifeq ($(IFORT_MAJOR),14)
          FOPT = $(FOPT3) -vec-report0 -ftz -align all -fno-alias -g -traceback
#         FOPT = $(FOPT3) -axAVX -xSSE4.1 -vec-report0 -ftz -align all -fno-alias -g -traceback
#         -mp is deprecated in 14
          MPFLAG := 
       else
       ifeq ($(IFORT_MAJOR),15)
          FOPT = $(FOPT3) -vec-report0 -ftz -align all -fno-alias -g -traceback
#         FOPT = $(FOPT3) -axAVX -xSSE4.1 -vec-report0 -ftz -align all -fno-alias -g -traceback
#         -mp is deprecated in 14
          MPFLAG := 
       else
          FOPT = $(FOPT3)
       endif
       endif
       endif
       endif
       endif
    endif

    ifeq ("$(BOPT)","Og")
       FOPT += -g -traceback
    endif

    LIB_ESMF = $(BASELIB)/libesmf.a

    CC  = gcc
    CXX = g++

#   Default MPI on i686
#   -------------------
    ifeq ($(MACH), i686)
      FC := mpif90
      INC_MPI := $(dir $(shell which mpif90))../include
      LIB_MPI := -L$(dir $(shell which mpif90))../lib -lmpi -lmpi_cxx -lmpi_f77
    endif

#   Handle MPI on x86_64
#   --------------------
    ifdef I_MPI_ROOT
        FC := mpiifort
        ifeq ($(MACH), x86_64) 
          INC_MPI := $(I_MPI_ROOT)/include64
          LIB_MPI := -L$(I_MPI_ROOT)/lib64  -lmpi -lmpiif # Intel MPI
        else
          INC_MPI := $(I_MPI_ROOT)/include
          LIB_MPI := -L$(I_MPI_ROOT)/lib  -lmpi -lmpiif # Intel MPI
        endif
    else
    ifdef OPENMPI
        FC := mpif90
        INC_MPI := $(OPENMPI)/include
        OPENMPI_LINK_FLAGS := $(shell mpif90 -showme:link) -lmpi_cxx
        LIB_MPI := -L$(OPENMPI)/lib $(OPENMPI_LINK_FLAGS)
    else
    ifdef M_MPI_ROOT
        FC := mpif90
        INC_MPI := $(M_MPI_ROOT)/include
        LIB_MPI := -L$(M_MPI_ROOT)/lib  -lmpich
    else
    ifdef MPI_HOME
        FC := mpif90
        INC_MPI := $(MPI_HOME)/include
        LIB_MPI := -L$(MPI_HOME)/lib  -lmpich
    else
    ifdef MVAPICH2
        FC := mpif90
        INC_MPI := $(MVAPICH2)/include
        LIB_MPI := -L$(MVAPICH2)/lib  -lmpich
    else
    ifdef FPATH
        FPATHS := $(subst :, ,$(FPATH))
        ifeq ($(MACH), x86_64) 
          FC := mpif90 
          INC_MPI := $(filter /nasa/sgi/mpt%,$(FPATHS)) \
                     $(filter /opt/scali%,$(FPATHS))
          INC_MPI := $(word 1,$(INC_MPI))
          LIB_MPI := -L$(subst include,lib,$(INC_MPI)) -lmpi -lmpi++
        endif
        ifeq ($(MACH), ia64)
          FC := mpif90 
          INC_MPI := $(filter /opt/sgi/mpt%,$(FPATHS)) \
                     $(filter /nasa/sgi/mpt%,$(FPATHS)) 
          INC_MPI := $(word 1,$(INC_MPI))
          LIB_MPI := -L$(subst include,lib,$(INC_MPI)) -lmpi -lmpi++
         endif
    else 
    endif
    endif
    endif
    endif
    endif
    endif

#   Define LIB_SYS
#   --------------
    LIB_SCI := 
    LIB_SYS := -ldl -lc -lpthread -lrt 

    ifeq ($(IFORT_MAJOR), 11)
          LIB_SYS := -lirc -lguide $(LIB_SYS)
          ifneq ($(MACH), i686)
              FPE += -fp-model precise
              MPFLAG :=# -mp is incompatible with the -fp-model option
#ams              CC  = icc
#ams              CXX = icpc
          endif
    else
    ifeq ($(IFORT_MAJOR), 12)
          LIB_SYS := -lirc $(LIB_SYS)
          ifneq ($(MACH), i686)
              FPE += -fp-model precise
              MPFLAG :=# -mp is incompatible with the -fp-model option
          endif
    else
    ifeq ($(IFORT_MAJOR), 13)
          LIB_SYS := -lirc $(LIB_SYS)
          ifneq ($(MACH), i686)
              FPE += -fp-model source
              MPFLAG :=# -mp is incompatible with the -fp-model option
          endif
    else
    ifeq ($(IFORT_MAJOR), 14)
          LIB_SYS := -lirc $(LIB_SYS)
          ifneq ($(MACH), i686)
              FPE += -fp-model source
              MPFLAG :=# -mp is incompatible with the -fp-model option
          endif
    else
    ifeq ($(IFORT_MAJOR), 15)
          LIB_SYS := -lirc $(LIB_SYS)
          ifneq ($(MACH), i686)
              FPE += -fp-model source
              MPFLAG :=# -mp is incompatible with the -fp-model option
          endif
    else
          LIB_SYS +=  # This should not be used; better to handle Major verison
    endif
    endif
    endif
    endif
    endif

#   MKL math library
#   ----------------
    ifeq ($(wildcard $(ESMABIN)/mklpath.pl),$(ESMABIN)/mklpath.pl)
       MKLPATH = $(shell $(ESMABIN)/mklpath.pl)
    endif
    ifdef MKLPATH
       ifeq ($(wildcard $(MKLPATH)/libmkl_intel_lp64.so),)
           LIB_SCI += -lmkl_intel_lp64 -lmkl_sequential -lmkl_core
       else
           LIB_SCI += -L$(MKLPATH) -lmkl_intel_lp64 -lmkl_sequential -lmkl_core
       endif
    else
    ifeq ($(MACH), ia64)
       LIB_SCI += -lscs 
    endif
    endif

    ifeq ($(MACH), i686)
          LIB_SCI += -llapack -lblas
    endif 

#   Customize for each MACH
#   -----------------------
    GCC_DIR = $(shell dirname `gcc --print-libgcc-file-name`)
    ifeq ($(MACH), x86_64) 
       OVERRIDE_LIMITS =
       LOOP_VECT =
       FDEFS += $(D)HAVE_SHMEM
    else
    ifeq ($(MACH), ia64)
      OVERRIDE_LIMITS = -override_limits 
      FDEFS += $(D)HAVE_SHMEM
    endif # x86_64
    endif # ia64

    LIB_SYS += -L$(GCC_DIR) -lstdc++

    CFLAGS += $(FPIC)
    fFLAGS += $(FPIC) $(EXTENDED_SOURCE) $(FPE) $(OVERRIDE_LIMITS) $(ALIGNCOM)
    FFLAGS += $(FPIC) $(EXTENDED_SOURCE) $(FPE) $(OVERRIDE_LIMITS) $(ALIGNCOM)
    f90FLAGS += $(FPIC) $(FPE) $(OVERRIDE_LIMITS) $(ALIGNCOM)
    F90FLAGS += $(FPIC) $(FPE) $(OVERRIDE_LIMITS) $(ALIGNCOM)

#   Some safeguards
#   ---------------
    ifeq ($(INC_MPI),)
      FC := mpif90
      INC_MPI := $(dir $(shell which mpif90))../include
      LIB_MPI := -L$(dir $(shell which mpif90))../lib -lmpi -lmpi_cxx -lmpi_f77
    endif

    ifeq ($(ESMA_PROFILE),TAU)
      ORIGFC := $(FC)
      export ORIGFC
      TAUROOTDIR   = /discover/swdev/mathomp4/tau-2.23/intel-14.0.1.106_mvapich2-2.0b
      TAU_MAKEFILE =  $(TAUROOTDIR)/x86_64/lib/Makefile.tau-icpc-mpi-pdt
      PDTROOTDIR   = /discover/swdev/mathomp4/src/pdtoolkit-3.20/x86_64/intel14.0.1.106
      #TAU_OPTIONS  = '-optPdtF90Parser="$(PDTROOTDIR)/bin/f95parse" -optPreProcess -optVerbose -optPDTInst -optTauSelectFile="$(ESMADIR)/src/Config/select.tau"'
      TAU_OPTIONS  = '-optPdtF90Parser="$(PDTROOTDIR)/bin/f95parse" -optPreProcess -optVerbose -optPDTInst -optRevert'
      #TAU_OPTIONS  = '-optPdtF90Parser="$(PDTROOTDIR)/bin/f95parse" -optPreProcess -optVerbose -optPDTInst -optRevert -optKeepFiles'
      export TAU_OPTIONS
      FC = $(TAUROOTDIR)/x86_64/bin/tau_f90.sh -tau_options=$(TAU_OPTIONS) -tau_makefile=$(TAU_MAKEFILE)
      F2PY = f2py --f77exec=$(ORIGFC) --f90exec=$(ORIGFC)
    endif
  endif

# Lahey F95 Compiler
# ------------------
  ifeq ($(ESMA_FC), lf95) 

      EXTENDED_SOURCE := --wide
      FREE_SOURCE = -free
      FIXED_SOURCE = -fixed
      FREAL4   := 
      FREAL8   := -CcdRR8  

      OMPFLAG = --openmp

      fFLAGS   += --warn $(EXTENDED_SOURCE)
      f90FLAGS += --warn $(EXTENDED_SOURCE)
      FFLAGS   += --warn $(EXTENDED_SOURCE)
      F90FLAGS += --warn $(EXTENDED_SOURCE)
      LDFLAGS  += --warn $(EXTENDED_SOURCE)

      LIB_SCI  = -llapackmt -lblasmt 
      LIB_SYS = -ldl -lc -lpthread -lrt 

#     AssumeOpenMPI is installed
#     --------------------------
      INC_MPI = /usr/include
      LIB_MPI = -lmpi
      override FC = mpif90
      OMPI_FC = lf95
      export OMPI_FC 

  endif

# GNU Fortran Compiler
# --------------------
  ifeq ($(ESMA_FC), gfortran) 

      CC = gcc

      LIB_ESMF = $(BASELIB)/libesmf.a

      EXTENDED_SOURCE := -ffixed-line-length-132
      FREE_SOURCE = 
      FIXED_SOURCE = -ffixed-form
      FREAL4   := 
      FREAL8   := -fdefault-real-8
      # This is needed for m_fpe.F90
      NO_RANGE_CHECK   := -fno-range-check 
      FPIC   := -fpic

      OMPFLAG = -fopenmp

      PP = -cpp

      fFLAGS   += $(FPIC) $(D)__GFORTRAN__ $(EXTENDED_SOURCE) $(NO_RANGE_CHECK)
      FFLAGS   += $(FPIC) $(D)__GFORTRAN__ $(EXTENDED_SOURCE) $(NO_RANGE_CHECK)
      f90FLAGS += $(FPIC) $(D)__GFORTRAN__ -ffree-line-length-none $(NO_RANGE_CHECK)
      F90FLAGS += $(FPIC) $(D)__GFORTRAN__ -ffree-line-length-none $(NO_RANGE_CHECK)

      ifdef OPENMPI
          FC := mpif90
          INC_MPI := $(OPENMPI)/include
          OPENMPI_LINK_FLAGS := $(shell mpif90 -showme:link) -lmpi_cxx
          LIB_MPI := -L$(OPENMPI)/lib $(OPENMPI_LINK_FLAGS)
      endif

#      LIB_SCI  = -llapackmt -lblasmt 
      LIB_SYS = -ldl -lc -lpthread -lrt -lstdc++

  endif


# Wrapper around PGI compiler for the Cray XT-4 platform
# ------------------------------------------------------
  ifeq ($(FC),ftn)

    LIB_ESMF = $(BASELIB)/libesmf.a
    INC_MPI  = $(MPICH_DIR)/include
    LIB_MPI  = -L$(MPICH_DIR)/lib -lmpichf90

    LIB_SYS = -lstd -lrt -lC
    EXTENDED_SOURCE = -Mextend
    TARGET   = -target=linux
    FREAL4   = -r4
    FREAL8   = -r8
    FPE      = -Ktrap=divz,inv,ovf
    PP       = -Mpreprocess

    ifeq ("$(BOPT)","g")
#       FOPT   = $(FOPTG) -Ktrap=fp -Mbounds -Mchkptr
#       FOPT   = $(FOPTG) -O0 -Ktrap=fp -Mbounds
       FOPT   = $(FOPTG) -O0 -Ktrap=fp
    else
       FOPT   = -fast -Kieee
#       FOPT   = -fast -Mvect=nosse -Kieee
    endif
    BIG_ENDIAN =
    fFLAGS += $(EXTENDED_SOURCE) $(FPE) $(TARGET)
    FFLAGS += $(EXTENDED_SOURCE) $(FPE) $(TARGET)
    f90FLAGS += $(FPE) $(TARGET)
    F90FLAGS += $(FPE) $(TARGET)
    CFLAGS   += -DpgiFortran
    CXXFLAGS +=
    CC  := cc
    CXX := CC
#    TAUROOTDIR      = /ccs/home/dkokron/play/tau/tau-2.18.1
#    include $(TAUROOTDIR)/x86_64/lib/Makefile.tau-callpath-mpi-compensate-pdt-pgi
#    include $(TAUROOTDIR)/x86_64/lib/Makefile.tau-callpath-mpi-papi-compensate-pdt-pgi
#    include $(TAUROOTDIR)/x86_64/lib/Makefile.tau-linuxtimers-multiplecounters-mpi-papi-compensate-pdt-pgi
##    OPTS            = -optPdtF90Parser=f95parse -optPreProcess -optVerbose -optCompile=$(TAU_F90_SUFFIX) -optKeepFiles -optNoRevert -optCPPOpts="-P -traditional-cpp" -optTauSelectFile=${ESMADIR}/src/Config/select.tau
#    OPTS            = -optPdtF90Parser=f95parse -optPreProcess -optCompile=$(TAU_F90_SUFFIX) -optNoRevert -optCPPOpts="-P -traditional-cpp" -optTauSelectFile=${ESMADIR}/src/Config/select.tau
#    FC              = $(TAU_COMPILER) $(OPTS) $(TAU_F90)
##    CF              = $(FC)
##    CC              = $(TAU_COMPILER) $(OPTS) $(TAU_CC)
##    CXX             = $(TAU_COMPILER) $(OPTS) $(TAU_CXX)
  endif

# Portland Group Compiler (also CUDA Fortran)
# -------------------------------------------
  ifeq ($(ESMA_FC), pgfortran)

      # Determine compiler version
      # --------------------------
      PGI_VER := $(subst -, ,$(subst ., ,$(word 2,$(shell pgfortran --version | sed 1d))))
      PGI_MAJOR := $(word 1,$(PGI_VER))
      PGI_MINOR := $(word 2,$(PGI_VER))

      ifndef MPI_HOME
         MPI_HOME := $(dir $(shell which mpif90))..
      endif
      FC := $(MPI_HOME)/bin/mpif90
      FREE_SOURCE = -Mfree
      FIXED_SOURCE = -Mfixed
      EXTENDED_SOURCE = -Mextend
      LIB_ESMF = $(BASELIB)/libesmf.a
      FREAL4 = 
      FREAL8 = -r8
      FPE = -Ktrap=fp #Equiv to -Ktrap=divz,inv,ovf
      FPIC = -fpic
      BACKSLASH_STRING = -Mbackslash

      OMPFLAG  := -mp

      # PGI autoselects the optimization target to the be host currently compiling
      # thus if you compile on a Sandy Bridge node, you'll generate code that cannot
      # run on a Westmere (=nehalem). To solve this we must always compile for
      # all targets that might be run.
      TARGET_ARCH = -tp=nehalem-64,sandybridge-64

      # If we are on janus, only one TARGET_ARCH possible, just unset
      ifeq ($(NODE),janus.gsfc.nasa.gov)
         TARGET_ARCH = 
      endif

      INC_PGI := $(dir $(shell which pgfortran))../include

      ifeq ("$(BOPT)","g")
         GPU_TARGET :=
         FOPT = -O0 -g -Kieee -Minfo=all -Mbounds -traceback -Mchkfpstk -Mchkstk -Mdepchk $(GPU_TARGET)
      else
      ifeq ("$(BOPT)","GPU")

         #FERMI To use the Fermis, compile with this:
         GPU_TARGET := -Mcuda=nofma,ptxinfo,5.0,cc20 -acc -ta=nvidia:nofma,5.0,cc20 -Minfo=accel,par,ccff
         TARGET_ARCH := -tp=nehalem-64            # GPU can't do more than one target_arch. All Fermis on Westmere

         #K10 To use the K10 Keplers, compile with this:
         #K10 GPU_TARGET := -Mcuda=nofma,ptxinfo,5.0,cc30 -ta=nvidia:wait,nofma,5.0,cc30 -Minfo=accel,par,ccff
         #K10 TARGET_ARCH := -tp=sandybridge-64            # GPU can't do more than one target_arch. All Keplers on Sandy

         #K20 To use the K20 Keplers, compile with this:
         #K20 GPU_TARGET := -Mcuda=nofma,ptxinfo,5.0,cc35,maxregcount:72 -ta=nvidia:wait,nofma,5.0,cc35,maxregcount:72 -Minfo=accel,par,ccff
         #K20 TARGET_ARCH := -tp=sandybridge-64            # GPU can't do more than one target_arch. All Keplers on Sandy

         FOPT = -fast -Kieee $(GPU_TARGET)
         USER_FDEFS += $(D)GPU_PRECISION=MAPL_R8 # Select precision for GPU Code that can be double prec (DQSAT, deledd)
         USER_FDEFS += $(D)GPU_MAXLEVS=72        # Select max level for GPU Code (could save space with this)
         USER_FDEFS += $(D)GPU_NUMAERO=15        # Select number of aerosols for GPU Code
         USER_FDEFS += $(D)_CUDA                 # Set this always so the GEOS-5 dependency builder can use it.
         USER_FDEFS += $(D)CUDAFOR               # Deprecated flag kept here for safety's sake. Move to _CUDA
      else
         GPU_TARGET :=
         FOPT = -fast -Kieee -g
      endif
      endif

      ifeq ("$(BOPT)","Og")
         FOPT += -g -traceback
      endif

      #USER_FDEFS += $(D)OVERCAST # Uncomment for OVERCAST Radiation code

      fFLAGS   += $(FPIC) $(EXTENDED_SOURCE) $(BACKSLASH_STRING) $(FPE) $(TARGET_ARCH)
      FFLAGS   += $(FPIC) $(EXTENDED_SOURCE) $(BACKSLASH_STRING) $(FPE) $(TARGET_ARCH)
      f90FLAGS += $(FPIC) $(BACKSLASH_STRING) $(FPE) $(TARGET_ARCH)
      F90FLAGS += $(FPIC) $(BACKSLASH_STRING) $(FPE) $(TARGET_ARCH)
      
      CFLAGS += $(FPIC) -DpgiFortran
      PP = -Mpreprocess

      LDFLAGS += -pgcpplibs $(TARGET_ARCH)

      INC_MPI = $(MPI_HOME)/include

      # Test if we are running Open MPI
      ifeq ($(findstring openMpi,$(INC_MPI)),openMpi)
         OPENMPI_LINK_FLAGS := $(shell mpif90 -showme:link)
         LIB_MPI := -L$(subst include,lib,$(INC_MPI)) $(OPENMPI_LINK_FLAGS)
      else
      ifeq ($(findstring openmpi,$(INC_MPI)),openmpi)
         OPENMPI_LINK_FLAGS := $(shell mpif90 -showme:link)
         LIB_MPI := -L$(subst include,lib,$(INC_MPI)) $(OPENMPI_LINK_FLAGS)
      else
      ifeq ($(findstring mvapich2,$(INC_MPI)),mvapich2)
         LIB_MPI := -L$(subst include,lib,$(INC_MPI)) -lmpich
      else
      # Test if we are using mpt at NAS
      ifdef FPATH
          FPATHS := $(subst :, ,$(FPATH))
         INC_MPI := $(filter /nasa/sgi/mpt%,$(FPATHS)) \
                    $(filter /opt/scali%,$(FPATHS))
         INC_MPI := $(word 1,$(INC_MPI))
         LIB_MPI := -L$(subst include,lib64,$(INC_MPI)) -lmpi -lmpi++ -lstdc++
      else
         LIB_MPI := -L$(subst include,lib,$(INC_MPI)) -lmpich
      endif
      endif
      endif
      endif


      LIB_SYS = -ldl -lstd -lrt -lC $(GPU_TARGET)

      LIB_SCI = -llapack -lblas
    
      CC = gcc
      CXX = g++

  endif

endif  #    Linux

#                               -----------------
#                               Darwin (Mac OS X)
#                               -----------------

ifeq ($(ARCH),Darwin)

# Linux default compilers
# -----------------------
  ifndef ESMA_FC
     FC = gfortran
  else
     override FC = $(ESMA_FC)
  endif
  CC  = gcc
  CXX = g++
  CPP = /usr/bin/cpp -xc++
  SED = /usr/bin/sed
  MAKEFLAGS = -w
  DLLEXT = dylib
  LIB_HDF = -lmfhdf -ldf -ljpeg -lz -lsz 
  LIB_SYS = 
  OMPFLAG = 

#
#                    Darwin Site Specific
#                    -------------------

# Darwin MPI default: assumes openmpi
# -----------------------------------
  INC_MPI := $(dir $(shell which mpif90))../include
  LIB_MPI := -L$(dir $(shell which mpif90))../lib -lmpi -lmpi_cxx -lmpi_f77

#                    Darwin Compiler Specific
#                    -----------------------

      LIB_SCI = -lblas -llapack

# GNU Fortran Compiler
# --------------------
  ifeq ($(FC), gfortran) 

      LIB_ESMF = $(BASELIB)/libesmf.a

      EXTENDED_SOURCE := -ffixed-line-length-132
      FREE_SOURCE = 
      FIXED_SOURCE = -ffixed-form
      FREAL4   := 
      FREAL8   := -fdefault-real-8

      fFLAGS   += $(D)__GFORTRAN__ $(EXTENDED_SOURCE)
      FFLAGS   += $(D)__GFORTRAN__ $(EXTENDED_SOURCE)
      f90FLAGS += $(D)__GFORTRAN__ -ffree-line-length-256
      F90FLAGS += $(D)__GFORTRAN__ -ffree-line-length-256

  endif


# G95 Fortran Compiler
# --------------------
  ifeq ($(FC), g95) 

      LIB_ESMF = $(BASELIB)/libesmf.a

      EXTENDED_SOURCE := -ffixed-line-length-132
      FREE_SOURCE = 
      FIXED_SOURCE = -ffixed-form
      FREAL4   := 
      FREAL8   := -r8 -i4

      fFLAGS   +=  $(EXTENDED_SOURCE)
      FFLAGS   +=  $(EXTENDED_SOURCE)
      f90FLAGS +=  -ffree-line-length-huge
      F90FLAGS +=  -ffree-line-length-huge

  endif # g95

# Intel Fortran Compiler
# ----------------------
  ifeq ($(FC), ifort) 

      EXTENDED_SOURCE := -extend-source
      FREE_SOURCE = -free
      FIXED_SOURCE = -fixed
      FREAL4 := 
      FREAL8 := -r8 -i4

      fFLAGS += $(EXTENDED_SOURCE)
      FFLAGS += $(EXTENDED_SOURCE)

#      LIB_SYS = -limf -lm -ldl -lirc -lguide -lstdc++ -lgcc_s.1
      LIB_SYS = -limf -lm -ldl -lirc -lstdc++ -lgcc_s.1

      override FC = mpif90

  endif # ifort

endif  #    Darwin

include $(ESMFMKFILE)
FC := $(ESMF_F90COMPILER)
CC := $(ESMF_CXXCOMPILER)
CXX := $(ESMF_CXXCOMPILER)
$(info Compiler is set to $(FC) and $(CC), make sure this is the MPI wrapper of the actual compiler)
